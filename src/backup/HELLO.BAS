$Debug

Const GAME_VERSION$ = "6.82"
Const MAX_SCHEDULE_GAMES = 1500
Const MAX_TEAMS = 1000
Const MAX_FILES = 10240
'optimium = 1048576 (but slows code)
Const MAX_CONFERENCES = 40
Const TEAMS_PER_DIV = 15
Const DIVS_PER_CONFERENCE = 2
Const TEAMS_PER_CONFERENCE = 30

Const MAX_SCHED_STATS = 162
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
Dim nbrInnings(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS), oppScore(MAX_SCHED_STATS)

Dim losePitcher$(MAX_SCHED_STATS), winPitcher$(MAX_SCHED_STATS)
Dim locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)

Dim statB%(0 To 22), statB0%(0 To 22, 0 To 41)
Dim statP%(0 To 21), statP0%(0 To 21, 0 To 41)
Dim statSS%(0 To 22, 0 To 6)
Dim statT0%(0 To 21), statT1%(0 To 22), statZ1%(40)

Dim statT0!(22), statT1!(22)

'-- For Road Data
Dim HR$(MAX_SCHED_STATS), LR$(MAX_SCHED_STATS)
Dim PLR$(MAX_SCHED_STATS), WR$(MAX_SCHED_STATS)
Dim HR%(MAX_SCHED_STATS), IR%(MAX_SCHED_STATS), VR%(MAX_SCHED_STATS)



' *** Schedule Data ***
' -------------------------
'Dim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
Dim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

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
Dim nbrGamesCurr, nbrGamesRoad
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

Dim scheduleNG%(MAX_SCHEDULE_GAMES, 18)

Dim eventSettings(1 To 13)
Dim scheduleAP%(1)

Dim modeAbbrev$(3)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_SCHEDULE_GAMES, 1)


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

'Used temporarily for troubleshooting
Dim E!, JJ!, R!, SE!, Q1!, QQ!

'Used by POSTGAME
Dim DB, HR, TR, SB
Dim eventNbr
Dim BP(1 To 9), F(1 To 9)

Dim B6%(22, 21), BB%(1 To 9), assigned(1 To 9)
Dim gameD1%(1), DYS%(0 To 1, 0 To 21)

Dim gameTP%(0 To 11, 0 To 2), MF%(1)
Dim startPitcher(1), SX%(0 To 1)
Dim gameT1%(0 To 1)

Dim grounderDir$(10), PT$(1)


' Shared / Global
Dim Shared badString%, gameW%, H6%
Dim Shared IJL%, INNING%, Inotloop%, LYN%

Dim Shared autoPlay, compTeam, desigHit, D
Dim Shared endGame, endAllGames, ejectCnt, FontColAdj
Dim Shared gameDelayed, gameLoc, gameTemp
Dim Shared noLUFound, normalFinish
Dim Shared P, PPH, pbpLine, pitchEraOpt, playerMode, playerOpt
Dim Shared precipType, rainyDay, rainStartInning
Dim Shared skyType, useInj, useRest, useVGA
Dim Shared windSpeed

'-- Delete the row below when done
Dim Shared testB, testJ, testL, testT, testU, testP9

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
Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(0 To 1, 1 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(0 To 1, 1 To 9, 1 To 9)
Dim Shared CF%(0 To 9, 0 To 2), CSS%(1, 22, 6), D3%(1), DP%(1)
Dim Shared gameB0%(0 To 3, 0 To 2), gameD0%(1), gameM%(0 To 9), gameSA%(1, 24)
Dim Shared GK%(1), H0%(1), INJ%(0 To 1, 0 To 22), L0%(1), LB%(9, 1)
Dim Shared MG%(0 To 1, 999), NG%(18)
Dim Shared P1%(0 To 1), P2%(1), P3%(1, 21), P4%(0 To 22)
Dim Shared P5%(1, 21, 41), P6%(1), P8%(0 To 1, 0 To 21, 0 To 41)
Dim Shared S1%(1), S6%(1, 2), S8%(1, 1), SO%(0 To 5, 0 To 2)
Dim Shared T3%(22), TS%(1, 11), X0%(1, 2, 23), YR%(1)

Dim Shared B1!(9), parkHRVals!(1)

Dim Shared alpha$(12)
Dim Shared baseName$(0 To 4), batters$(0 To 1, 0 To 22), B1$(4)
Dim Shared DB$(0 To 22), diskIDs$(1), ejections$(20), eventDesc$(0 To 22)
Dim Shared fieldPos$(0 To 11), G$(10)
Dim Shared gameManagers$(1), gameStadiums$(1), gameTeams$(1), gameUmpire$(4)
Dim Shared H0$(1), homeRuns$(0 To 22), IJ$(20)
Dim Shared PARK$(99), pitchers$(1, 21), player$(23), precip$(4)
Dim Shared Q3$(100), SB$(0 To 22), skyCond$(4)
Dim Shared teamAbbreviatons$(0 To 1), TR$(0 To 22)
Dim Shared windDirection$(10)



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
Open "errlog" For Append As #10
Print #10, "Error #"; Err; "on program file line"; _ErrorLine
If _FileExists("GAME82") Then
    Print #10, "Schedule game # "; Using "###"; schedGame(0) + schedGame(2)
    Print #10, gameTeams$(0) + " vs. " + gameTeams$(1)
End If
Print #10, "B3%(0, x) = "; B3%(0, 1); B3%(0, 2); B3%(0, 3); B3%(0, 4); B3%(0, 5); B3%(0, 6); B3%(0, 7); B3%(0, 8); B3%(0, 9)
Print #10, "B3%(1, x) = "; B3%(1, 1); B3%(1, 2); B3%(1, 3); B3%(1, 4); B3%(1, 5); B3%(1, 6); B3%(1, 7); B3%(1, 8); B3%(1, 9)
Print #10,
Close #10
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
'INCLUDE: 'SOURCE.bm'


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

    'Declare all variables for the modules
    Dim F1$(10)
    Dim PH%(0 To 22, 0 To 1)

    Shared grounderDir$()

    Shared gameTP%(), SU()

    Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

    If useVGA = 1 Then Call LOADSCREEN

    15000 '

    '-- An error comes up every so many games
    '-- when B9%(D, 1) gets a negative value
    '-- Nothing in the the code modifies B9%()
    '-- between here and when it is corrupted.
    '-- Here, we save the non-negative value
    '-- so we can restore after it's corrupted.
    b9d1 = B9%(D, 1)
    b9d2 = B9%(D, 2)

    Erase nbrOuts
    Erase gameB0%, B1%

    F% = 0
    H0% = 0
    S4% = 0

    L0%(P) = L0%(P) + MO
    MO = 0

    If P = 1 Then
        INNING% = INNING% + 1
        Erase CZ%
    End If

    'Switch posession
    P = 1 - P
    D = 1 - P

    If INNING% = 7 And tmRat_GAME(1, 13) = 8 And P = 1 And Val(Left$(gameTeams$(1), 2)) > 75 Then
        Call WINDEX
        Call PBP("ALL RIGHT..."): Call PBP("LEMME HEAR YA..."): Call PBP("A ONE, A TWO...")
    End If

    'Check scores / innings
    '------------------------------
    If INNING% > 9 And D = 1 And S6%(1, 0) <> S6%(0, 0) Or INNING% = 28 Or INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then
        'After 9th inning, score not tied, etc..
        'End of game!
        GoTo 18500
    End If

    If rainyDay = 1 And INNING% >= rainStartInning And rainStartInning >= 6 And (S6%(1, 0) > S6%(0, 0) And P = 1 Or S6%(0, 0) > S6%(1, 0) And P = 0) Then
        Cls
        If useVGA = 0 Then Screen 0
        Print "THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!"
        Print "ALL RECORDS ARE OFFICIAL!!"
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
        I$ = GetKeyPress$

        'End of game!
        GoTo 18500
    End If

    If rainyDay = 1 And P = 0 And INNING% = 4 Then
        Call PBP("THE RAIN IS FALLING A LITTLE MORE STEADILY")
        Call PBP("THIS GAME MAY NOT GO 9!")
        I$ = GetKeyPress$
    End If

    'Game is not over - show the field
    If useVGA = 1 Then
        '-- Here, we restore the non-negative value
        '-- of B9%() to prevent an error
        B9%(D, 1) = b9d1
        B9%(D, 2) = b9d2

        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
    Else
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
    End If

    For IJ0% = 0 To 22

        If batRating(D, IJ0%, 21) > 39 And batRating(D, IJ0%, 21) <= 49 Then

            I1 = IJ0%
            I2 = batRating(D, IJ0%, 21) - 40

            batRating(D, I1, 21) = 1
            B3%(D, I2) = I1
            B9%(D, B7%(D, I2)) = I1
            gameD0%(D) = gameD0%(D) + 1
            X0%(D, 0, gameD0%(D)) = B3%(D, I2)
            X0%(D, 1, gameD0%(D)) = B7%(D, I2)
            X0%(D, 2, gameD0%(D)) = (I2 * 10) + (gameD0%(D) - 9)

            W5 = D
            W6 = D
            VV(D) = I2
            VV1(D) = I2

            For xx = 22 To 25
                If B7%(D, I2) = batRating(D, I1, xx) Then
                    batRating(D, I1, 15) = batRating(D, I1, 33 + (xx - 22) * 2)
                    batRating(D, I1, 17) = batRating(D, I1, 34 + (xx - 22) * 2)
                    batRating(D, I1, 19) = batRating(D, I1, xx + 4)
                    Exit For
                End If
            Next

            '14955  ---> this is it's own routine used often
            Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

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

    A1 = S6%(D, 0) - S6%(P, 0)

    If Not (MG%(D, 560) <> 999 Or INNING% < 8 Or playerMode = 0 Or playerMode = 1 And P = compTeam) Then

        For IJ0% = 1 To 4

            If MG%(D, 560 + IJ0%) <> -1 And batRating(D, MG%(D, 564 + IJ0%), 21) = 0 Then

                Call d100(RN)

                If B9%(D, MG%(D, 576 + IJ0%)) = MG%(D, 560 + IJ0%) And RN <= MG%(D, 568 + IJ0%) And A1 >= MG%(D, 572 + IJ0%) Then

                    I1 = MG%(D, 564 + IJ0%)

                    For I2 = 1 To 9
                        If B7%(D, I2) = MG%(D, 576 + IJ0%) Then Exit For
                    Next

                    batRating(D, I1, 21) = 1
                    B3%(D, I2) = I1
                    B9%(D, B7%(D, I2)) = I1

                    gameD0%(D) = gameD0%(D) + 1
                    X0%(D, 0, gameD0%(D)) = B3%(D, I2)
                    X0%(D, 1, gameD0%(D)) = B7%(D, I2)
                    X0%(D, 2, gameD0%(D)) = (I2 * 10) + (gameD0%(D) - 9)

                    W5 = D
                    W6 = D
                    VV(D) = I2
                    VV1(D) = I2

                    For xx = 22 To 25
                        If B7%(D, I2) = batRating(D, I1, xx) Then
                            batRating(D, I1, 15) = batRating(D, I1, 33 + (xx - 22) * 2)
                            batRating(D, I1, 17) = batRating(D, I1, 34 + (xx - 22) * 2)
                            batRating(D, I1, 19) = batRating(D, I1, xx + 4)
                            Exit For
                        End If
                    Next

                    Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

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

    If B9%(D, 1) = 99 And playerMode = 0 Or B9%(D, 1) = 99 And playerMode = 1 And compTeam = 1 - D Then
        '----- move stuff here -----
        GoTo 14900
    End If

    15050 '
    SH = 0
    F% = 0
    SO = 0
    B1!(P) = B1!(P) + 1
    INFPOS% = 0
    nbrStrikes = 0

    If B1!(P) = 10 Then B1!(P) = 1

    '--- Start of a new sub
    15051 '
    Call WHOSON1ST(S4%)
    Call PLAYERNAME(P)

    If useVGA = 1 Then
        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
    Else
        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
    End If

    P2 = 0
    D2 = 0

    ' Game over - there's a winner
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) > 0 Then
        GoTo 18500
    End If

    15052 '

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

            FORMATTED$ = FUsing$(Str$(S6%(I0, 0)), "##", 1)
            PrintOnGfx 53, I0 + 28, FORMATTED$, imageScreen&

            FORMATTED$ = FUsing$(Str$(S6%(D, 2)), "##", 1)
            'PrintOnGfx 59, 1 - I0 + 28, FORMATTED$, imageScreen&
            PrintOnGfx 59, D + 28, FORMATTED$, imageScreen&

        Else

            Locate I0 + 2, 40
            Print Using "##"; S6%(I0, 0);
            Locate D + 2, 46
            Print Using "##"; S6%(D, 2);
            Color 15, 0

        End If

        Z2% = gameB0%(2, 0)
        F% = 0

        15080 '
        If (playerMode = 2 Or playerMode = 1 And P <> compTeam) Then
            GoTo 18700
        End If

        If S4% = 1 Or S4% = 5 Then
            H0% = 1
            If useVGA = 1 Then
                Call HOLDSTATUS_G(H0%)
            Else
                Call HOLDSTATUS(H0%)
            End If
        End If

        '----------------------------------------
        ' DEFENSE OPTIONS MENU
        '----------------------------------------
        15081 '
        pbpString$ = PadRight$(Left$(gameTeams$(D), 3) + teamAbbreviatons$(D), 8)

        'Call WINDEX

        If useVGA = 1 Then

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color 4278190080

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

            INFPOS% = 3

            If useVGA = 1 Then
                pbpString$ = "INF:GUARD LINE"
                _PutImage (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color 4278190080

                PrintOnGfx 52, 26, pbpString$, imageScreen&

                'Printing white on black
                Color 4294967295
                _PrintMode _FillBackground , imageScreen&
            Else
                Color 15, 2
                Locate 23, 46: Print "GUARD LINE"
            End If

            GoTo 15052

        End If

        If UCase$(I$) = "L" Then
            Call PopUp_Lineup
            GoTo 15580
        End If

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = "Q" Then
            If useVGA = 0 Then Cls
            Call EndOfGame
            Exit Sub
        End If

        If UCase$(I$) = "S" Then
            Call POPUPSTATS
            GoTo 15580
        End If

        If UCase$(I$) = "B" Then
            If useVGA = 1 Then Screen 0
            Call PopUp_Batter(D, P1%(D))
            GoTo 15580
        End If

        If UCase$(I$) = "P" Then
            If useVGA = 1 Then Screen 0
            Call PopUp_Pitcher(P, P1%(P))
            GoTo 15580
        End If

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        If UCase$(I$) = "H" Then

            H0% = 1 - H0%

            If useVGA = 1 Then
                Call HOLDSTATUS_G(H0%)
            Else
                Call HOLDSTATUS(H0%)
            End If

            GoTo 15081

        End If

        If Val(I$) = 7 Then
            P2 = 1
            D2 = 1

            GoTo 13050

        End If

        D2 = Val(I$)

    Loop Until D2 >= 1 And D2 <= 9


    15055 '

    Select Case D2
        Case 1
            '

        Case 2
            Call WINDEX
            Call BASEONBALLS(S2%, D2, D, P)
            GoTo 13201

        Case 3
            INFPOS% = 2
            infStatus$ = "INF:INFIELD IN"

        Case 4
            INFPOS% = 1
            infStatus$ = "INF:CORNERS IN"

        Case 5
            INFPOS% = 0
            infStatus$ = "INF:NORMAL    "

        Case 6
            P9 = D
            W5 = -1
            W6 = -1
            GoTo 15500

    End Select

    'We've assigned the text,
    'now we have to print it on the screen
    If D2 >= 3 And D2 <= 5 Then

        If useVGA = 1 Then
            'Restore image where options go
            _PutImage (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16), backupScreen&, imageScreen&, (52 * FontColAdj, 416)-Step(14 * FontColAdj, 16)

            'Printing black on background
            _PrintMode _KeepBackground , imageScreen&
            Color 4278190080

            'Print infield status
            PrintOnGfx 52, 26, infStatus$, imageScreen&

            'Printing white on black
            Color 4294967295
            _PrintMode _FillBackground , imageScreen&
        Else
            Color 15, 2
            Locate 23, 46: Print infStatus$
        End If

        GoTo 15052

    End If

    '----------------------------------------
    ' OFFENSE OPTIONS MENU
    '----------------------------------------
    15085 '
    Do

        If (playerMode = 2 Or playerMode = 1 And P = compTeam) Then

            JM = B3%(P, B1!(P))

            If PPH <> 1 Then

                A1 = S6%(P, 0) - S6%(D, 0)

                If B7%(P, B1!(P)) <> 1 Then

                    If MG%(P, 847) = 999 Then

                        'PH FOR POS PLAYER CASES
                        Call d100(RN)
                        MOB% = 0

                        For I = 1 To 3
                            If gameB0%(I, 0) > 0 Then MOB% = MOB% + 1
                        Next

                        If batRating(P, B3%(P, B1!(P)), 4) = 0 Then
                            I7! = 0
                            I6! = 0
                        Else
                            I7! = Int(batRating(P, B3%(P, B1!(P)), 6) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                            I6! = Int((batRating(P, B3%(P, B1!(P)), 6) + batRating(P, B3%(P, B1!(P)), 7) + batRating(P, B3%(P, B1!(P)), 8) * 2 + batRating(P, B3%(P, B1!(P)), 9) * 3) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                        End If

                        For I = 0 To 2
                            If INNING% >= MG%(P, 848 + I * 6) And A1 <= MG%(P, 849 + I * 6) * -1 And I6! <= MG%(P, 851 + I * 6) And I7! <= MG%(P, 852 + I * 6) And RN <= MG%(P, 853 + I * 6) And MOB% >= MG%(P, 850 + I * 6) Then
                                GoTo 18909
                            End If
                        Next

                    Else

                        If batRating(P, JM, 4) <> 0 Then
                            GoTo 18909
                        Else

                            If Not (batRating(P, JM, 6) / batRating(P, JM, 4) * 1000 > 233 Or batRating(P, JM, 9) >= 15) Then
                                If INNING% > 6 And A1 < -2 Or A1 <= 0 And INNING% >= 8 Or A1 = 0 And INNING% >= 9 Then 18909
                            End If

                        End If

                    End If

                End If

            Else

                For I = 0 To 21

                    If pitchRating(P, I, 15) = 0 Then

                        If MG%(P, 590) = 999 Then

                            'PH FOR PITCHER WITH PROFILE
                            Call d100(RN)

                            MOB% = 0

                            For I = 1 To 3
                                If gameB0%(I, 0) > 0 Then MOB% = MOB% + 1
                            Next

                            If pitchRating(P, P1%(P), 11) <> 999 Then
                                I7! = pitchRating(I0, P1%(I0), 11)
                            Else
                                If pitchRating(P, P1%(P), 20) = 0 Then
                                    I7! = 0
                                Else
                                    I7! = Int((pitchRating(P, P1%(P), 22) / pitchRating(P, P1%(P), 20) * 1000 + .5))
                                End If
                            End If

                            For I = 0 To 2
                                If INNING% >= MG%(P, 591 + I * 6) And A1 <= MG%(P, 592 + I * 6) * -1 And nbrOuts(0) <= MG%(P, 594 + I * 6) And I7! <= MG%(P, 595 + I * 6) And RN <= MG%(P, 596 + I * 6) And MOB% >= MG%(P, 593 + I * 6) Then
                                    GoTo 18930
                                End If
                            Next

                            Exit For

                        Else

                            If INNING% > 4 And INNING% < 7 And A1 < -2 And S6%(P, 0) > 0 Then 18930
                            If INNING% > 4 And INNING% < 7 And A1 < -3 Then 18930
                            If INNING% > 6 And A1 < 0 Or A1 = 0 And INNING% > 6 And INNING% < 9 And S4% > 1 Then 18930
                            If A1 = 0 And INNING% >= 9 And S4% > 0 Or A1 = 0 And INNING% >= 9 And nbrOuts(0) = 0 Then 18930
                            If pitchRating(P, P1%(P), 6) / pitchRating(P, P1%(P), 4) <= 2 And P8%(P, P1%(P), 0) >= 6 Then 18930

                            Exit For

                        End If

                    End If

                Next

            End If

            GoTo 18941

        Else

            Call WINDEX

            _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)

            pbpString$ = PadRight$(Left$(gameTeams$(P), 3) + teamAbbreviatons$(P), 8)

            If useVGA = 1 Then
                'Printing black on background
                _PrintMode _KeepBackground , imageScreen&
                Color 4278190080

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
                Color 4294967295
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

        _PutImage (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176), backupScreen&, imageScreen&, (1 * FontColAdj, 304)-Step(14 * FontColAdj, 176)

        If UCase$(I$) = "D" Then Call VIEWTEAM: GoTo 15580

        If UCase$(I$) = "L" Then Call PopUp_Lineup: GoTo 15580

        If UCase$(I$) = "O" Then
            Call OPTIONS
            GoTo 15580
        End If

        If UCase$(I$) = "Q" Then
            If useVGA = 0 Then Cls
            Call EndOfGame
            Exit Sub
        End If

        If UCase$(I$) = "S" Then Call POPUPSTATS: GoTo 15580

        If UCase$(I$) = "W" Then
            Call WEATHER
            GoTo 15580
        End If

        P2 = Val(I$)

    Loop Until P2 >= 1 And P2 <= 5

    15064 '
    Select Case P2

        Case 1, 4

            If D2 <> 8 Then 13050

            Call WINDEX

            Call d100(RN)

            If P2 = 4 Or (P2 = 1 And RN <= 50) Then 13050

            Call BASEONBALLS(S2%, D2, D, P)

            GoTo 13201

        Case 2
            If nbrOuts(0) = 2 Or (S4% <> 1 And S4% <> 4 And S4% <> 5) Then 15051

        Case 3
            GoTo 14800

        Case 5
            P9 = P
            W5 = -1
            W6 = -1
            GoTo 15500

    End Select

    15067 '
    '-- this is related to the PICKEDOFF routine and it's immediate aftermath
    If S4% = 4 Then I = 3 Else I = 2

    Call d100(I3)

    If I3 < 6 Then

        Call PICKEDOFF(D, I)

        PO = 1
        A5%(I - 1) = 10
        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        gameW% = 1

        Call CREDITASSIST(gameW%, D)
        Call ADVANCEMENT(P, D, S2%, F%, I3)
        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        I5 = P1%(D)
        I6 = 0
        Call ADDPIT(D, I5, I6)

        If nbrOuts(0) >= 3 Then

            B1!(P) = B1!(P) - 1
            PO = 0
            GoTo 13900

        End If

    End If

    If PO = 1 Then PO = 0: GoTo 15051

    Call WINDEX

    I3 = Int(Rnd(1) * 1000) + 1
    I9 = Int(Rnd(1) * 1000) + 1

    If I3 > 350 Then
        Call PBP("Runner(s) going")
    Else
        Call PBP("Count not favorable...hit away")
        P2 = 1
    End If

    Call DELAY

    If I3 <= 350 Then 13050

    If I9 <= (B4%(P, 4, B1!(P)) * 2 * .33) Then
        nbrStrikes = nbrStrikes + 1
        Call PBP(player$(0) + " swings and misses")
        Call DELAY
        If nbrStrikes = 3 Then
            GoTo 13601
        Else
            GoTo 14800
        End If
    End If

    If I9 <= B4%(P, 4, B1!(P)) * 2 Then
        Call PBP(player$(0) + " fouls off the pitch...hit away")
        Call DELAY
        P2 = 1
    End If

    GoTo 13050


    '--- end of a new sub

    '================================================================================
    ' Below are all the different GoTo branches for the gameplay
    ' These have not been worked out into a different flow
    ' yet for the game's logic.
    '================================================================================

    '----- NOODLE -----


    12000 ' BRANCH FOR RARE PLAYS
    Call RAREPLAYS(H6%, H7%, IP%)

    If IP% = 1 Then IP% = 0: Call RELIEVERS(PPH, pz)

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

        Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo 15052

    End If

    '5
    '25 to 29
    If H6% = 5 Or H6% >= 25 And H6% <= 29 Then

        Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo 13900

    End If

    '9
    If H6% = 9 Then

        Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo 13201

    End If

    '45
    If H6% = 45 Then

        Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

        If useVGA = 1 Then
            Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
        Else
            Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
        End If

        GoTo 13201

    End If

    '30 to 44
    '46+ ???
    Call HITTINGLINES '(tmRat_GAME(), p%(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

    If useVGA = 1 Then
        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
    Else
        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
    End If

    GoTo 15052

    '----- NOODLE -----


    13050 '
    Call d100(RN)

    If (((precipType = 1 Or precipType = 2 Or precipType = 3) And RN = 2) Or (rainyDay = 1 And RN <= 5)) And INNING% >= 2 And gameDelayed = 0 Then
        H7% = 20
        H6% = 21
        GoTo 12000
    Else
        H6% = Int(Rnd(1) * 1000) + 1
    End If

    If H6% <= 3 And useInj = 1 Then
        GoTo 12000
    End If

    SN = 0
    H6% = Int(Rnd(1) * 1000) + 1

    If nbrOuts(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 768 Or nbrOuts(0) = 0 And S4% = 4 And INFPOS% < 2 And H6% = 123 Or nbrOuts(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 245 Or nbrOuts(0) = 0 And S4% = 7 And INFPOS% < 2 And H6% = 381 Then
        Call TRIPLEPLAY(D)
        GoTo 13900
    End If

    H7% = Int(Rnd(1) * 7500) + 1

    Call WINDEX

    If H6% <= 4 Then
        ' *** hit by pitch ***
        Call HITBYPITCH(S2%, D)

        13201 '
        A5%(0) = 1
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 0

        If gameB0%(1, 0) > 0 Then
            If gameB0%(2, 0) > 0 Then
                If gameB0%(3, 0) > 0 Then
                    A5%(3) = 1
                    If gameW% = 2 And S2% > 2 Then A5%(3) = 11
                Else
                    A5%(2) = 1
                End If
            Else
                A5%(1) = 1
            End If
        End If

        Call ADVANCEMENT(P, D, S2%, F%, I3)
        GoTo 15050

    End If

    If H6% <= 6 Then

        ' *** balk ***
        If S4% = 0 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        Else
            S2% = 4

            Call WINDEX

            Call d100(RN)
            rndUmp = Int(Rnd(1) * 4) + 1

            Select Case RN
                Case Is <= 33
                    pbpString$ = "a balk has been called"
                Case 34 To 66
                    Call PBP("and a BALK is called")
                    pbpString$ = pitchers$(D, P1%(D)) + " dropped the ball"
                Case 67 To 100
                    pbpString$ = gameUmpire$(rndUmp) + " calls a balk on " + pitchers$(D, P1%(D))
            End Select

            Call PBP(pbpString$)

            '13223
            If gameB0%(1, 0) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then
                A5%(0) = 1
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                GoTo 13900
            Else
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 0
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                GoTo 15051
            End If

        End If

    End If

    '7 to 11 OR 7 to 200 And ...
    If H6% <= 11 Or H6% <= 200 And batRating(D, B9%(D, 2), 22) <> 2 And batRating(D, B9%(D, 2), 23) <> 2 And batRating(D, B9%(D, 2), 24) <> 2 And batRating(D, B9%(D, 2), 25) <> 2 Then

        13220 ' *** passed ball ***
        If S4% = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        End If

        S2% = 5
        Call WINDEX
        Call DELIVERY(D)

        Call d100(RN2)
        If RN2 <= 50 Then
            Call PBP("pitch gets by " + batters$(D, B9%(D, 2)))
        Else
            'RN > 50
            Call PBP("off " + batters$(D, B9%(D, 2)) + "'s glove")
            Call PBP("it gets away from him")
        End If

        Call PBP("It's ruled a passed ball")

        If gameB0%(1, 0) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then nbrOuts(1) = nbrOuts(1) + 1

        13222 '
        RN3 = Int(Rnd(1) * 10) + 1

        If S4% = 1 And P2 = 2 And RN3 < batRating(P, B3%(P, gameB0%(1, 0)), 16) Or S4% = 5 And P2 = 2 And RN3 < batRating(P, B3%(P, gameB0%(1, 0)), 16) Then
            A5%(1) = 2
            A5%(3) = 1
            A5%(0) = 0
            A5%(2) = 2
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 15051
        End If

        13223 '
        If gameB0%(1, 0) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) > 1 And RN = 9 And SO = 1 Then
            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 13900
        Else
            A5%(0) = 0
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1
            Call ADVANCEMENT(P, D, S2%, F%, I3)
            GoTo 15051
        End If

    End If

    If H6% <= 19 Then

        '13230 / *** wild pitch ***
        If S4% = 0 And SO <> 1 Then
            '13100 / *** fouled out of play ***
            Call FOULBALL
            GoTo 15052
        End If

        Call WILDPITCH(S2%, D)

        GoTo 13222

    End If

    If H7% = 27 Then

        S2% = 2
        Call WINDEX
        Call PBP(batters$(D, B9%(D, 2)) + " is called for interference!!")
        Call PBP(player$(0) + " trots to first base")
        GoTo 13201

    End If

    If H7% = 28 Then

        Call WINDEX
        Call PBP(player$(0) + " is out for obstruction!")

        gameW% = 2
        Call CREDITREGPO(gameW%, D)
        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1

        If B7%(P, B1!(P)) <> 1 Then
            I5 = B3%(P, B1!(P))
            I6 = 0
            Call INCBATOFF(P, I5, I6)
        Else
            I5 = P1%(P)
            I6 = 23
            Call INCPITOFF(P, I5, I6)
        End If

        I5 = P1%(D)
        I6 = 0

        Call ADDPIT(D, I5, I6)

        GoTo 13900
    End If

    If H6% <= B4%(P, 5, B1!(P)) + 25 Then
        S2% = 2
        '13240 / *** base on balls ***
        Call BASEONBALLS(S2%, D2, D, P)
        GoTo 13201
    End If

    If P2 = 4 Then 14100

    H6% = Int(Rnd(1) * 1000) + 1
    H7% = Int(Rnd(1) * 1000) + 1
    I4 = 0

    If P2 = 2 Then I4 = -5

    If INFPOS% = 2 Then
        SN = 70
        I4 = I4 + 15
    End If

    If nbrStrikes = 2 Then I4 = I4 - 15

    If INFPOS% = 1 Then SN = 20
    If INNING% > 5 And S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) Then I4 = I4 + 40

    If P = 0 Then I4 = I4 - 2
    If P = 1 Then I4 = I4 + 2

    Call WINDEX

    If H7% <= SN Then
        Call PBP("It gets through the drawn-in infield!")
        Call DELAY
        GoTo 13063
    End If

    If H6% > B4%(P, 0, B1!(P)) + I4 Then

        ' *** strikeout ***
        H6% = Int(Rnd(1) * 1000) + 1

        If H6% > B4%(P, 4, B1!(P)) Then

            ' *** ground ball for out ***
            Call d100(H6%)

            E2% = batRating(P, B3%(P, B1!(P)), 18) + pitchRating(D, P1%(D), 12) - 52
            E3% = 23 + pitchRating(D, P1%(D), 12)
            E4% = 0

            If H6% > E2% And B7%(P, B1!(P)) <> 1 Then E4% = 1
            If H6% > E3% And B7%(P, B1!(P)) = 1 Then E4% = 1

            If E4% = 1 Then

                'balls hit to the air
                Call d100(RN)
                A5%(0) = 0
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0

                Select Case RN

                    Case 1 To 73
                        ' *** FLY  OUTS ***
                        Call GETOUTFIELDER(gameW%, P, I1, D)

                        If B7%(P, B1!(P)) <> 1 Then
                            I5 = B3%(P, B1!(P)): I6 = 0
                            Call INCBATOFF(P, I5, I6)
                        Else
                            I5 = P1%(P): I6 = 23
                            Call INCPITOFF(P, I5, I6)
                        End If

                        Call d100(RN)

                        Select Case RN

                            Case 1 To 17
                                If RN <= 17 Then
                                    ' *** shallow fly ball ***
                                    S2% = 19
                                    Call WINDEX

                                    X$ = fieldPos$(gameW%)
                                    If Int(Rnd(1) * 100) <= 25 And gameW% = 7 Then X$ = "lcf"
                                    If Int(Rnd(1) * 100) <= 25 And gameW% = 9 Then X$ = "rcf"

                                    Call DELIVERY(D)

                                    'What is this used for?
                                    Call d100(RN)

                                    Call GetPBPString(60, 1170, Y$, D)
                                    Call PBP(Y$ + " " + X$)

                                    Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                                    If F% = 1 Then 13900
                                    GoTo 14025

                                End If

                            Case 18 To 33
                                ' *** deep fly out ***
                                S2% = 17

                                Call WINDEX
                                Call LONGFLY(D, gameW%)

                                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                                If F% = 1 Then 13900

                                Call CREDITREGPO(gameW%, D)
                                Call CAUGHTFLY(RN, S2%, D)

                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                I5 = P1%(D)
                                I6 = 0

                                Call ADDPIT(D, I5, I6)

                                If nbrOuts(0) = 3 Then 13900

                                If gameB0%(3, 0) <> 0 Then

                                    A5%(3) = 1
                                    I5 = P1%(D)
                                    I6 = 16

                                    Call ADDPIT(D, I5, I6)

                                    If B7%(P, B1!(P)) = 1 Then
                                        I5 = P1%(P)
                                        I6 = 23
                                        Call PITOFF(P, I5, I6)
                                        I5 = P1%(P)
                                        I6 = 41
                                        Call INCPITOFF(P, I5, I6)
                                    Else

                                        I5 = B3%(P, B1!(P))
                                        I6 = 0

                                        Call BATOFF(P, I5, I6)
                                        I5 = B3%(P, B1!(P))
                                        I6 = 19
                                        Call INCBATOFF(P, I5, I6)

                                    End If

                                End If

                                14066 '
                                If gameB0%(2, 0) <> 0 Then

                                    I4 = (batRating(P, B3%(P, gameB0%(2, 0)), 16) - 5) * 25
                                    ZM = (batRating(D, B9%(D, gameW%), 15) - 5) * 25
                                    I4 = I4 - ZM

                                    RN = Int(Rnd(1) * 1000) + 1

                                    '-- These are ALL conditions for assigning the same value
                                    If gameW% = 9 And S2% = 17 And RN <= 800 + I4 And P2 <> 2 Or gameW% = 9 And S2% = 18 And RN <= 400 + I4 And P2 <> 2 Or gameW% = 8 And S2% = 17 And RN <= 700 + I4 And P2 <> 2 Or gameW% = 8 And S2% = 18 And RN <= 350 + I4 And P2 <> 2 Or gameW% = 7 And S2% = 17 And RN <= 250 + I4 And P2 <> 2 Then A5%(2) = 1
                                    If P2 = 2 And batRating(P, B3%(P, gameB0%(2, 0)), 16) > 6 And gameW% = 9 And S2% = 17 And RN <= 400 + I4 Or P2 = 2 And batRating(P, B3%(P, gameB0%(2, 0)), 16) > 6 And gameW% = 8 And S2% = 17 And RN <= 400 + I4 Then A5%(2) = 1

                                    If A5%(2) <> 1 And S2% = 18 And P2 <> 2 And S4% < 6 Then

                                        If (gameW% = 9 And RN > 800) Or (gameW% = 8 And RN > 840) Then
                                            I8! = 3
                                            LN% = 3308
                                            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)
                                        End If

                                        If gameW% = 7 And P2 <> 2 And S4% < 6 And RN > 880 Then
                                            I8! = 3
                                            LN% = 3308
                                            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)
                                        End If

                                    End If

                                End If

                                If gameB0%(1, 0) <> 0 Then
                                    RN = Int(Rnd(1) * 1000) + 1
                                    A5%(1) = 0
                                    I3 = (batRating(P, B3%(P, gameB0%(1, 0)), 16) - 5) * 25
                                    I4 = (batRating(D, B9%(D, gameW%), 15) - 5) * 25
                                    J5 = I3 - I4

                                    If S2% = 17 And P2 <> 2 And S4% = 1 And RN <= 100 + J5 Then
                                        I8! = 2
                                        LN% = 3308
                                        If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)
                                    End If
                                    I8! = 2
                                    LN% = 3308
                                End If

                                Call ADVANCEMENT(P, D, S2%, F%, I3)
                                GoTo 13900

                            Case 34 To 100
                                ' *** regular fly out ***
                                S2% = 18
                                Call WINDEX
                                X$ = fieldPos$(gameW%)
                                If Int(Rnd(1) * 100) <= 25 And gameW% = 7 Then X$ = "lcf"
                                If Int(Rnd(1) * 100) <= 25 And gameW% = 9 Then X$ = "rcf"

                                Call DELIVERY(D)
                                Call d100(RN)

                                range = 90
                                min = 1070
                                If (gameW% = 7 Or gameW% = 9) And RN <= 10 Then X$ = "": range = 10: min = 1160

                                Call GetPBPString(range, min, Y$, D)
                                Call PBP(Y$ + " " + X$)

                                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                                If F% = 1 Then 13900

                                '--- where does this end?
                                '--- can we turn into subroutine?
                                14025 '
                                Call CREDITREGPO(gameW%, D)
                                Call CAUGHTFLY(RN, S2%, D)

                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                I5 = P1%(D)
                                I6 = 0

                                Call ADDPIT(D, I5, I6)
                                If nbrOuts(0) = 3 Then 13900

                                Call d100(RN)

                                If S2% = 19 And S4% = 1 And RN <= 33 And P2 = 2 Or S2% = 19 And S4% = 5 And RN <= 33 Then
                                    A5%(1) = 10
                                    nbrOuts(0) = nbrOuts(0) + 1
                                    nbrOuts(1) = nbrOuts(1) + 1

                                    Call ADDPIT(D, I5, I6)

                                    D3%(D) = D3%(D) + 1
                                    DP%(D) = DP%(D) + 1

                                    Call CREDITASSIST(gameW%, D)

                                    GoTo 14009

                                End If

                                If gameB0%(3, 0) > 0 Then
                                    I8! = 4
                                    LN% = 3308
                                    If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)

                                End If

                                If I9 = 1 Then
                                    I9 = 0
                                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                                    GoTo 13900
                                Else
                                    GoTo 14066
                                End If

                        End Select

                    Case 74 To 89

                        ' infield popup
                        Call GETINFIELDER(gameW%, P, I1, D, S2%, P2)
                        S2% = 20
                        Call WINDEX
                        Call d100(RN0)

                        If gameW% = 1 Then gameW% = 2: player$(4) = batters$(D, B9%(D, gameW%))
                        If (gameW% = 2 Or gameW% = 5 Or gameW% = 3) And RN0 <= 30 Then fo% = 1

                        If fo% = 0 And (S4% = 4 Or S4% = 7) And nbrOuts(0) < 2 Then Call PBP("INFIELD FLY RULE CALLED")
                        If fo% = 1 And gameW% = 2 Then range = 24: min = 180
                        If fo% = 1 And (gameW% = 3 Or gameW% = 4) Then range = 31: min = 210
                        If fo% = 1 And (gameW% = 5 Or gameW% = 6) Then range = 31: min = 250
                        If fo% <> 1 And gameW% = 2 Then range = 50: min = 400
                        If fo% <> 1 And gameW% = 3 Then range = 60: min = 450
                        If fo% <> 1 And gameW% = 5 Then range = 60: min = 510
                        If fo% <> 1 And gameW% = 6 Then range = 60: min = 570
                        If fo% <> 1 And gameW% = 4 Then range = 60: min = 630

                        Call GetPBPString(range, min, Y$, D)
                        Call PBP(Y$)

                        If fo% = 1 And gameW% = 2 Then range = 12: min = 321
                        If fo% = 1 And (gameW% = 3 Or gameW% = 4) Then range = 25: min = 340
                        If fo% = 1 And (gameW% = 5 Or gameW% = 6) Then range = 25: min = 370
                        If fo% <> 1 Then range = 25: min = 690

                        Call GetPBPString(range, min, Y$, D)
                        Call PBP(Y$)

                        If B7%(P, B1!(P)) = 1 Then
                            I5 = P1%(P)
                            I6 = 23
                            Call INCPITOFF(P, I5, I6)
                        Else
                            I5 = B3%(P, B1!(P))
                            I6 = 0
                            Call INCBATOFF(P, I5, I6)
                        End If

                        If S4% <> 4 Then

                            Call d100(RN0)
                            If RN0 <= 25 And (tmRat_GAME(1, 13) = 11 Or tmRat_GAME(1, 13) = 8) Then Call PBP("fights the wind...")
                            If RN0 <= 15 And (tmRat_GAME(1, 13) = 2 Or tmRat_GAME(1, 13) = 4 Or tmRat_GAME(1, 13) = 13 Or tmRat_GAME(1, 13) = 24) Then Call PBP("fights the lights...")

                            If fo% <> 1 Then
                                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                                If F% = 1 Then 13900
                            End If

                        End If

                        fo% = 0

                        Call PBP(player$(4) + " makes the catch")
                        Call CREDITREGPO(gameW%, D)

                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1
                        I5 = P1%(D)
                        I6 = 0

                        Call ADDPIT(D, I5, I6)
                        GoTo 13900

                    Case 90 To 100
                        S2% = 21

                        Call GETINFIELDER(gameW%, P, I1, D, S2%, P2)
                        Call WINDEX
                        Call DELIVERY(D)

                        Call GetPBPString(42, 70, Y$, D)
                        Call PBP(Y$)

                        Call GetPBPString(34, 120, Y$, D)
                        Call PBP(Y$)

                        If B7%(P, B1!(P)) = 1 Then
                            I5 = P1%(P)
                            I6 = 23
                            Call INCPITOFF(P, I5, I6)
                        Else
                            I5 = B3%(P, B1!(P))
                            I6 = 0
                            Call INCBATOFF(P, I5, I6)
                        End If

                        Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                        If F% = 1 Then 13900

                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1

                        Call CREDITREGPO(gameW%, D)
                        I5 = P1%(D)
                        I6 = 0

                        Call ADDPIT(D, I5, I6)
                        If nbrOuts(0) = 3 Then 13900

                        Call d100(RN)

                        If (S4% = 1 Or S4% = 5) And RN >= 80 Or (S4% = 1 Or S4% = 5) And P2 = 2 Then

                            'runner doubled off of 1st
                            A5%(1) = 10
                            nbrOuts(0) = nbrOuts(0) + 1
                            nbrOuts(1) = nbrOuts(1) + 1

                            Call ADDPIT(D, I5, I6)

                            D3%(D) = D3%(D) + 1
                            DP%(D) = DP%(D) + 1

                            If gameW% <> 3 Then
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
                            If gameW% <> 3 Then Call CREDITASSIST(gameW%, D)


                        Else

                            If (S4% = 4 Or S4% = 2) And RN >= 80 Or S4% = 4 And P2 = 2 Then

                                'runner doubled off of 2nd
                                A5%(2) = 10
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1

                                Call ADDPIT(D, I5, I6)

                                D3%(D) = D3%(D) + 1
                                DP%(D) = DP%(D) + 1
                                I3 = 2

                                Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                                Call PBP(" --- DOUBLE PLAY ---")
                                Call CREDITASSIST(gameW%, D)

                            End If

                        End If

                        14009 '
                        Call ADVANCEMENT(P, D, S2%, F%, I3)
                        GoTo 13900

                End Select

            End If

            S2% = 13
            Call GETINFIELDER(gameW%, P, I1, D, S2%, P2)
            Call WINDEX

            S2% = 13
            Call DELIVERY(D)
            Call GROUNDERPBP(gameW%, D)

            If gameW% <> 2 Then
                If gameW% = 1 Then
                    pbpString$ = pbpString$ + "the pitcher"
                    Call PBP(pbpString$)
                Else
                    pbpString$ = pbpString$ + grounderDir$(gameW%)
                    Call PBP(pbpString$)
                End If
            End If

            Call d100(RN)

            If (S4% = 1 Or S4% = 4 Or ((S4% = 7 Or S4% = 5) And INFPOS% < 2)) And nbrOuts(0) <= 1 And RN <= 50 Then
                DPC% = 1
                Call DPPBP
            End If

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
            End If

            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

            ' *** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
            If nbrOuts(0) = 2 Or S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then 13650

            H6% = Int(Rnd(1) * 1000) + 1
            E2% = ((tmRat_GAME(D, 5) - 80) * 4)
            E3% = ((5 - batRating(P, B3%(P, B1!(P)), 16)) * 9)

            If (gameW% = 2 And H6% > 250 + E2% + E3%) Or (H6% > 490 + E2% + E3%) Or P2 = 2 Then 13650

            S2% = 13

            If S4% = 4 Then

                If gameW% = 5 And Int(Rnd(1) * 3) + 1 = 1 Then
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo 13634
                End If

                If gameW% = 2 Then
                    Call CREDITASSIST(gameW%, D)
                    A5%(3) = 0
                    A5%(2) = 11
                    A5%(1) = 0
                    A5%(0) = 0
                    GoTo 13634
                End If

            End If

            If S4% = 7 And gameW% = 1 Then Call CREDITASSIST(gameW%, D): TB = 9: gameW% = 2: Call PBP("He throws home"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And nbrOuts(0) = 0 And INFPOS% > 0 And gameW% = 5 Or S4% = 5 And S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 7 And nbrOuts(0) = 0 And INFPOS% > 0 And gameW% = 3 Then 13650
            If S4% = 7 And INFPOS% > 0 And (gameW% = 5 Or gameW% = 3) Then Call CREDITASSIST(gameW%, D): TB = 9: gameW% = 2: Call PBP("He fires it home"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 7 And INFPOS% = 2 And gameW% = 4 Then Call CREDITASSIST(gameW%, D): TB = 9: gameW% = 2: Call PBP("He throws home for the force"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634
            If S4% = 7 And INFPOS% = 2 And gameW% = 6 Then Call CREDITASSIST(gameW%, D): TB = 9: gameW% = 2: Call PBP("He goes home with it"): A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GoTo 13634

            If gameW% <= 4 Then
                Call CREDITASSIST(gameW%, D)
                gameW% = 6
                Call PBP("He throws to " + batters$(D, B9%(D, 6)))
            Else
                Call CREDITASSIST(gameW%, D)
                gameW% = 4
                Call PBP("He throws to " + batters$(D, B9%(D, 4)))
            End If

            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0
            A5%(0) = 0

            If S4% = 7 And (gameW% = 2 Or gameW% = 1) Then
                A5%(3) = 11
                A5%(2) = 1
                A5%(1) = 1
                A5%(0) = 1
            End If

            GoTo 13634

        End If

        'Strikeout!
        13601 '
        Call WINDEX
        Call STRIKEOUT(nbrStrikes, D)

        gameW% = 2
        Call CREDITREGPO(gameW%, D)
        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        SO = 1

        If B7%(P, B1!(P)) <> 1 Then
            I5 = B3%(P, B1!(P))
            I6 = 0
            Call INCBATOFF(P, I5, I6)
            I6 = 8
            Call INCBATOFF(P, I5, I6)
            If P2 = 2 Then nbrStrikes = 3
        End If

        I5 = P1%(D)
        I6 = 5
        Call ADDPIT(D, I5, I6)

        I6 = 0
        Call ADDPIT(D, I5, I6)

        If B7%(P, B1!(P)) = 1 Then

            I5 = P1%(P)
            I6 = 23
            Call INCPITOFF(P, I5, I6)

            I6 = 31
            Call INCPITOFF(P, I5, I6)
            If P2 = 2 Then nbrStrikes = 3
        End If

        If P2 = 2 And nbrOuts(0) < 3 Then
            SO = 7
            GoTo 14800
        End If

        ' *** WILD PITCH OR PASSED BALL ON A STRIKEOUT
        RN = Int(Rnd(1) * 600) + 1
        If gameB0%(1, 0) = 0 And RN = 9 And SO = 1 Or nbrOuts(0) = 3 And RN = 9 Then

            nbrOuts(0) = nbrOuts(0) - 1
            nbrOuts(1) = nbrOuts(1) - 1
            I5 = P1%(D)
            P8%(D, I5, 0) = P8%(D, I5, 0) - 1
            P5%(D, I5, 0) = P5%(D, I5, 0) - 1
            B8%(D, B9%(D, 2), 16) = B8%(D, B9%(D, 2), 16) - 1
            B5%(D, B9%(D, 2), 16) = B5%(D, B9%(D, 2), 16) - 1

            Call d100(I1)

            If I1 < 77 Then

                '13230 / *** wild pitch ***
                If S4% = 0 And SO <> 1 Then
                    '13100 / *** fouled out of play ***
                    Call FOULBALL
                    GoTo 15052
                Else
                    Call WILDPITCH(S2%, D)
                    GoTo 13222
                End If
            Else
                GoTo 13220
            End If

        End If

        '-- this should be an Else statement above?
        GoTo 13900

    Else

        H6% = Int(Rnd(1) * 1000) + 1

        If H6% <= B4%(P, 1, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 And P2 = 2 Then

            S2% = 8

            '3500
            ' *** double s2%=8 ***
            Call GETOUTFIELDER(gameW%, P, I1, D)

            I5 = P1%(D)
            I6 = 1

            Call ADDPIT(D, I5, I6)
            Call WINDEX
            Call DELIVERY(D)

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
                I6 = 4
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
                I6 = 27
                Call INCPITOFF(P, I5, I6)
            End If

            If RN <= 40 Then

                If gameW% = 7 Then
                    Call GetPBPString(40, 2900, Y$, D)
                    Call PBP(Y$)
                End If

                If gameW% = 9 Then
                    Call GetPBPString(40, 2940, Y$, D)
                    Call PBP(Y$)
                End If

                If gameW% = 8 Then
                    Call GetPBPString(40, 2980, Y$, D)
                    Call PBP(Y$)
                End If

                Call GetPBPString(30, 3020, Y$, D)
                Call PBP(Y$)

            Else

                Call d100(RN)

                Select Case RN

                    Case Is <= 25
                        Call LONGFLY(D, gameW%)
                        HW% = 1
                        Call WALLDOUBLE(HW%, gameW%)

                    Case 26 To 50
                        Call LDRIVE(X$)
                        Call PBP(player$(0) + " " + X$ + " one to the gap in " + OUTFLDLOCATION$(gameW%))

                    Case 51 To 60
                        Call PBP(player$(0) + " hits a sinking liner into " + OUTFLDLOCATION$(gameW%))
                        Call PBP("It goes to the wall")

                    Case 61 To 75
                        Call PBP(player$(0) + " lines one to the alley in " + OUTFLDLOCATION$(gameW%))

                    Case 76 To 85
                        If (gameW% = 7 Or gameW% = 9) Then
                            Call LDRIVE(X$)
                            Call PBP(player$(0) + " " + X$ + " one down the " + fieldPos$(gameW%) + " line")
                            Call PBP("It rolls into the corner")
                        End If

                        If gameW% = 8 Then
                            Call LDRIVE(X$)
                            Call PBP(player$(0) + " " + X$ + " one between the fielders")
                            Call PBP(player$(4) + " cuts it off...")
                        End If

                    Case 86 To 92
                        Call LDRIVE(X$)
                        Call PBP(player$(0) + " " + X$ + " a blue darter into " + fieldPos$(gameW%))

                    Case 93 To 100
                        Call PBP(player$(0) + " lines one to " + OUTFLDLOCATION$(gameW%))
                        Call PBP("It's down and to the wall!")

                End Select

            End If

            HW% = 0
            Call d100(RN0)

            If RN0 <= 80 Then
                pbpString$ = "Double!!"
            Else
                pbpString$ = "2-Bagger!!"
            End If

            Call PBP(pbpString$)

            For I = 0 To 3: A5%(I) = 2: Next I
            '-- wtf?
            A5%(3) = 1

            If gameB0%(1, 0) <> 0 Then

                Call d100(H6%)
                BR% = batRating(P, B3%(P, gameB0%(1, 0)), 16)

                If B7%(P, gameB0%(1, 0)) = 1 Then BR% = pitchRating(P, P1%(P), 31)

                I3 = 0

                If nbrOuts(0) = 0 And gameW% = 7 Then I3 = 18 + BR% * 3
                If nbrOuts(0) = 1 And gameW% = 7 Then I3 = 16 + BR% * 3
                If nbrOuts(0) = 2 And gameW% = 7 Then I3 = 34 + BR% * 3

                If nbrOuts(0) = 0 And gameW% = 8 Then I3 = 37 + BR% * 3
                If nbrOuts(0) = 1 And gameW% = 8 Then I3 = 44 + BR% * 3
                If nbrOuts(0) = 2 And gameW% = 8 Then I3 = 65 + BR% * 3

                If nbrOuts(0) = 0 And gameW% = 9 Then I3 = 12 + BR% * 3
                If nbrOuts(0) = 1 And gameW% = 9 Then I3 = 18 + BR% * 3
                If nbrOuts(0) = 2 And gameW% = 9 Then I3 = 40 + BR% * 3

                If H6% <= I3 Or P2 = 2 Then A5%(1) = 3

            End If

            Call ADVANCEMENT(P, D, S2%, F%, I3)

            F% = 0
            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD

            GoTo 13900

        End If


        If H6% <= B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P)) And P2 <> 2 Or H6% <= B4%(P, 1, B1!(P)) * .5 + B4%(P, 2, B1!(P)) * .5 And P2 = 2 Then

            S2% = 9

            '3520
            ' *** TRIPLE S2%=9 ***
            Call OUTFLDNAME(gameW%, D)
            Call DELIVERY(D)
            Call WINDEX

            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
                I6 = 5
                Call INCBATOFF(P, I5, I6)
            Else
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
                I6 = 28
                Call INCPITOFF(P, I5, I6)
            End If

            I5 = P1%(D)
            I6 = 1

            Call ADDPIT(D, I5, I6)

            Call d100(RN)

            If RN <= 30 Then

                If gameW% = 7 Then
                    Call GetPBPString(40, 2900, Y$, D)
                    Call PBP(Y$)
                End If

                If gameW% = 9 Then
                    Call GetPBPString(40, 2940, Y$, D)
                    Call PBP(Y$)
                End If

                If gameW% = 8 Then
                    Call GetPBPString(40, 2980, Y$, D)
                    Call PBP(Y$)
                End If

                Call GetPBPString(40, 3050, Y$, D)
                Call PBP(Y$)

                Call d100(RN)
                If RN <= 90 Then
                    pbpString$ = "Triple!!"
                Else
                    pbpString$ = "3-Bagger!"
                End If

                Call PBP(pbpString$)

            Else

                Call d100(RN)

                If RN <= 10 And gameW% <> 8 Then

                    Call d100(RN)
                    X$ = "1st"
                    If gameW% = 7 Then X$ = "3rd"

                    If RN <= 50 Then
                        Call PBP(player$(0) + " hits a hot smash")
                    Else
                        Call PBP(player$(0) + " lines one")
                    End If

                    Call PBP("Fair past " + X$ + "!!")
                    Call PBP("It rolls into the " + fieldPos$(gameW%) + " corner")
                    Call PBP("It's a triple!!")

                Else

                    Call LONGFLY(D, gameW%)
                    If gameW% = 7 And tmRat_GAME(1, 13) = 27 Then
                        ad$ = "screen"
                    Else
                        ad$ = "wall"
                    End If

                    Call d100(BV)
                    Call d100(RN)
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

                    Call d100(RN)
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

            Call ADVANCEMENT(P, D, S2%, F%, I3)

            F% = 0
            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD
            GoTo 13900

        End If

        '-- this could be nested in an Else clause below
        Call HITS2GAP(gameW%, P, I1, D)

        I3 = B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P))

        E2% = I3 + B4%(P, 3, B1!(P))
        If gameW% = 8 And H6% <= E2% And P2 <> 2 Or gameW% = 8 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        E2% = I3 + B4%(P, 6, B1!(P))
        If gameW% = 7 And H6% <= E2% And P2 <> 2 Or gameW% = 7 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        E2% = I3 + B4%(P, 7, B1!(P))
        If gameW% = 9 And H6% <= E2% And P2 <> 2 Or gameW% = 9 And H6% <= E2% * .5 And P2 = 2 Then
            S2% = 10
            GoTo 13530
        End If

        13063 '
        Call d100(H6%)
        S2% = 7

        If Not (B7%(P, B1!(P)) = 1 Or H7% <= SN) Then

            If H6% <= (batRating(P, B3%(P, B1!(1)), 16) * 2.5) Then
                S2% = 12
                Call GETINFIELDER(gameW%, P, I1, D, S2%, P2)
                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                If F% = 1 Then 13900
            End If

        End If

        13065 '/ *** single ***
        Call SINGLEROUTINE(gameW%, P, I1, D, S2%, P2)

        If S2% = 12 Then

            Call SCOREBOARD
            GoTo 15050

        Else

            F% = 0
            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 0 Then Call TRYFOREXTRABASE(LN%, S2%, P2, Z2%, D, gameW%, SAH%, SB%, I8!, I9)

            Call SCOREBOARD
            GoTo 13900

        End If

    End If


    '----- NOODLE -----


    13530 '
    ' *** HOME RUN S2%=10 ***

    Call WINDEX

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
        I6 = 1
        Call INCBATOFF(P, I5, I6)
        I6 = 6
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 1

    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
        I6 = 24
        Call INCPITOFF(P, I5, I6)
        I6 = 29
        Call INCPITOFF(P, I5, I6)
    End If

    Call d100(RN)

    If RN <= 80 Then

        Call LONGFLY(D, gameW%)

        Call d100(RN)

        If RN <= 5 And tmRat_GAME(1, 13) = 8 And gameW% = 7 Then Call PBP("THAT'S OUT OF THE PARK"): Call PBP("ON WAVELAND AVENUE!!!"): pbpString$ = "HOME RUN...HOLY COW!!!": GoTo 13543
        If RN <= 15 And tmRat_GAME(1, 13) = 8 And gameW% = 7 Then Call PBP("IT'S GONE"): pbpString$ = "LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!": GoTo 13543
        If RN <= 25 Then

            '13580
            If tmRat_GAME(1, 13) = 21 And gameW% = 7 Then Call PBP("IT CLEARS THE GREEN MONSTER!!"): pbpString$ = "HOME RUN!!!": GoTo 13543
            If tmRat_GAME(1, 13) = 27 And gameW% = 7 Then Call PBP("IT CLEARS THE SCREEN!!"): pbpString$ = "HOME RUN!!!": GoTo 13543
            If tmRat_GAME(1, 13) = 33 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): pbpString$ = "THE BUSCH EAGLE FLAPS ITS WINGS": GoTo 13543
            If tmRat_GAME(1, 13) = 25 And P = 1 Then Call PBP("IT'S OUT OF HERE FOR A HOMER!!!"): pbpString$ = "THERE GOES THE EXPLODING SCOREBOARD!": GoTo 13543
            If tmRat_GAME(1, 13) = 7 And gameW% = 9 Then Call PBP("IT CLEARS THE WAGON GATE!!"): pbpString$ = "THAT'S A HOME RUN!!": GoTo 13543
            If tmRat_GAME(1, 13) = 7 And P = 1 Then Call PBP("ADIOS!! THAT'S A HOME RUN!!"): pbpString$ = "THERE GO THE CANNONS!!": GoTo 13543
            If tmRat_GAME(1, 13) = 8 Or tmRat_GAME(1, 13) = 40 Then pbpString$ = "IT CLEARS THE IVY FOR A HOME RUN!!": GoTo 13543
            If tmRat_GAME(1, 13) = 23 And (gameW% = 7 Or gameW% = 9) Then Call PBP("IT'S LONG, IT'S LONG, IT'S GONE!!"): pbpString$ = "THAT ONE'S IN THE UPPER DECK": GoTo 13543

            '13545
            Call d100(RN)
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

        End If

    Else

        Call d100(RN)

        If RN <= 20 And (gameW% = 7 Or gameW% = 9) Then

            pbpString$ = player$(0) + " hits it down the " + fieldPos$(gameW%) + " line"
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
                    pbpString$ = pitchers$(D, P1%(D)) + " WATCHES IT SAIL OUT!!"
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

    13543 '
    Call PBP(pbpString$)
    Call DELAY

    '13544
    A5%(3) = 1
    A5%(2) = 2
    A5%(1) = 3
    A5%(0) = 4
    I5 = P1%(D)
    I6 = 14

    Call ADDPIT(D, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call SCOREBOARD

    GoTo 13900


    '----- NOODLE -----

    13634 '
    I5 = P1%(D)
    I6 = 0
    3
    Call ADDPIT(D, I5, I6)

    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1
    player$(4) = batters$(D, B9%(D, gameW%))

    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITREGPO(gameW%, D)

    S2% = 15

    Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
    If F% = 1 Then 13900

    Call CREDITASSIST(gameW%, D)

    If gameW% = 5 And nbrOuts(0) < 2 And Int(Rnd(1) * 10) + 1 <= batRating(P, B3%(P, gameB0%(1, 0)), 16) Then
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
    I5 = P1%(D)
    I6 = 0

    Call DPRELAY
    Call ADDPIT(D, I5, I6)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
    Call d100(RN)

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

    GoTo 13900


    '----- NOODLE -----


    13650 '
    If nbrOuts(0) >= 2 Then

        RN = Int(Rnd(1) * 10) + 1
        S2% = 13

        If gameW% = 5 Then

            If RN <= 5 And (S4% = 4 Or S4% = 7) Then

                Call PITCHERNAME(gameW%, D)
                Call PBP(player$(4) + " gloves it and heads for 3rd")
                A5%(0) = 0
                A5%(1) = 0
                A5%(2) = 11
                A5%(3) = 0
                GoTo 13684

            Else

                Call PITCHERNAME(gameW%, D)
                Call GROUNDERS(gameW%, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            End If

        End If

        If gameW% = 6 Then

            If RN <= 8 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then
                Call PITCHERNAME(gameW%, D)
                Call GROUNDERS(gameW%, D)
                Call DPPIVOT(gameW%, D)
                AST% = 6
                A5%(0) = 0
                A5%(1) = 11
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            Else
                Call PITCHERNAME(gameW%, D)
                Call GROUNDERS(gameW%, D)
                Call THROW2FIRST(D)
                A5%(0) = 11
                A5%(1) = 0
                A5%(2) = 0
                A5%(3) = 0
                '-- this may not be necessary since the next statement is looking for W = 4
                GoTo 13684

            End If

        End If

        If gameW% = 4 And RN <= 5 And (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

            Call PITCHERNAME(gameW%, D)
            Call GROUNDERS(gameW%, D)
            Call DPPIVOT(gameW%, D)
            AST% = 4
            A5%(0) = 0
            A5%(1) = 11
            A5%(2) = 0
            A5%(3) = 0

        Else

            Call d100(RN)

            If gameW% = 3 Then

                If RN <= 94 Then

                    Call PITCHERNAME(gameW%, D)
                    Call GROUNDERS(gameW%, D)
                    Call PBP("He takes it to the bag unassisted")
                    fbun% = 1

                Else

                    Call PITCHERNAME(gameW%, D)
                    Call GROUNDERS(gameW%, D)
                    gameW% = 1
                    Call PITCHERNAME(gameW%, D)
                    Call PBP("He flips it to " + player$(4) + " covering")
                    FB2P% = 1
                    gameW% = 3

                End If

            Else

                Call PITCHERNAME(gameW%, D)
                Call GROUNDERS(gameW%, D)
                Call SECOND2FIRST(D)

            End If

            Call PITCHERNAME(gameW%, D)
            Call GROUNDERS(gameW%, D)
            Call SECOND2FIRST(D)

            A5%(0) = 11
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

        End If

        13684 'Call DELAY
        If fbun% = 0 Then Call CREDITASSIST(gameW%, D)
        fbun% = 0

        If DPC% = 1 And A5%(0) = 1 Then DPC% = 2

        Call ADVANCEMENT(P, D, S2%, F%, I3)

        If DPC% = 2 Then
            DPC% = 0
            Call DPSAFE
        End If

        If I3 > 4 Then I3 = 1

        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        I5 = P1%(D)
        I6 = 0
        Call ADDPIT(D, I5, I6)

        GoTo 13900

    Else

        Call d100(RN)
        If (RN <= 90 And P2 <> 2 Or S4% = 0) Or (RN <= 10 And P2 = 2) Then

            S2% = 14

            If S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% = 6 Then
                A5%(0) = 11
                Call OUTATFIRST(gameW%, D, fbun%, FB2P%)
                GoTo 13693
            End If

            Call d100(RN)

            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If INFPOS% = 0 And gameW% = 2 Then A5%(3) = 0
            If S4% = 5 Or S4% = 7 Or S4% = 4 Then 13725

            A5%(1) = 11

            GoTo 13684

        End If

        S2% = 13
        A5%(1) = 1
        A5%(2) = 1
        A5%(3) = 1
        A5%(0) = 11

        If Not (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

            13693 '
            Call d100(RN)
            A5%(1) = 0
            A5%(2) = 0
            A5%(3) = 0

            If gameW% = 5 And RN <= 25 And INFPOS% < 2 Then A5%(2) = 1
            If gameW% = 5 And RN <= 83 And INFPOS% = 0 Then A5%(3) = 1: ' WAS 50
            If gameW% = 5 And RN <= 10 And INFPOS% > 0 Then A5%(3) = 1

            If gameW% = 5 And RN < 5 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or gameW% = 5 And RN < 5 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then
                A5%(2) = 11
                A5%(0) = 1
                Call PBP("NO, he's going to third")
            End If

            If gameW% = 6 And RN <= 20 And INFPOS% < 2 Then A5%(2) = 1
            If gameW% = 6 And RN <= 92 And INFPOS% < 2 Then A5%(3) = 1
            If gameW% = 6 And RN <= 5 And INFPOS% = 2 Then A5%(3) = 1
            If gameW% = 6 And RN < 6 And INFPOS% < 2 And S4% = 2 And S6%(P, 0) - S6%(D, 0) > -2 Or gameW% = 6 And RN < 6 And INFPOS% < 2 And S4% = 6 And A5%(3) = 1 And S6%(P, 0) - S6%(D, 0) > -2 Then A5%(2) = 11: A5%(0) = 1: Call PBP("NO, he's going to third")
            If gameW% = 4 And RN <= 90 And INFPOS% < 2 Then A5%(2) = 1
            If gameW% = 4 And RN <= 90 And INFPOS% < 2 Then A5%(3) = 1
            If gameW% = 4 And RN <= 65 And INFPOS% = 2 Then A5%(2) = 1
            If gameW% = 4 And RN <= 5 And INFPOS% = 2 Then A5%(3) = 1
            If gameW% = 3 And RN <= 95 Then A5%(2) = 1
            If gameW% = 3 And RN <= 88 And INFPOS% = 0 Then A5%(3) = 1: ' WAS 60
            If gameW% = 3 And RN <= 15 And INFPOS% > 0 Then A5%(3) = 1
            If gameW% = 1 And RN <= 30 Then A5%(2) = 1
            If gameW% = 1 And RN <= 50 And INFPOS% = 0 Then A5%(3) = 1
            If gameW% = 1 And RN <= 10 And INFPOS% > 0 Then A5%(3) = 1
            If gameW% = 2 And RN <= 45 Then A5%(2) = 1

        End If

        GoTo 13684

    End If

    13725 '
    If S4% <> 4 And S4% <> 7 Then
        If S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And nbrOuts(0) = 0 And INFPOS% > 0 And gameW% = 5 Or S6%(P, 0) - S6%(D, 0) >= 0 And INNING% > 6 And nbrOuts(0) = 0 And INFPOS% > 0 And gameW% = 3 Then

            A5%(3) = 11

        Else

            If INFPOS% = 0 And gameW% <> 2 Then A5%(1) = 11: GoTo 13684
            If INFPOS% > 0 And gameW% = 5 Or INFPOS% > 0 And gameW% = 3 Then A5%(3) = 0: A5%(1) = 11: GoTo 13684
            If INFPOS% > 0 And (gameW% = 1 Or gameW% = 3) Then A5%(3) = 0: A5%(1) = 1: A5%(0) = 11: GoTo 13684
            If INFPOS% = 2 And gameW% = 4 Or INFPOS% = 2 And gameW% = 6 And RN <= 90 Then
                A5%(3) = 0
                A5%(1) = 11
            Else
                A5%(1) = 11
            End If

        End If

    Else

        If gameW% = 6 And RN < 6 And INFPOS% < 2 And INNING% < 9 Or gameW% = 5 And RN < 6 And INFPOS% < 2 And INNING% < 9 Then
            A5%(2) = 11
        Else
            If S4% = 7 Then

                Call d100(RN)
                A5%(1) = 11
                A5%(2) = 1
                A5%(3) = 1
                A5%(0) = 1

                If INFPOS% > 0 And gameW% = 5 Or INFPOS% > 0 And gameW% = 3 Or INFPOS% = 2 And gameW% = 4 Or INFPOS% = 2 And gameW% = 6 Or gameW% = 2 Or gameW% = 1 Then
                    A5%(3) = 11
                    A5%(0) = 1
                    A5%(1) = 1
                    A5%(2) = 1
                Else
                    If gameW% = 5 And RN <= 5 Or gameW% = 1 Or gameW% = 2 Then
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

                If gameW% = 5 And RN <= 5 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
                If gameW% = 1 And RN <= 3 Then A5%(1) = 1: A5%(2) = 11: A5%(3) = 0

                If gameW% = 2 And batRating(P, B3%(P, gameB0%(2, 0)), 16) < 7 And batRating(P, B3%(P, gameB0%(2, 0)), 16) > 0 Then
                    A5%(1) = 1
                    A5%(2) = 11
                    A5%(3) = 0
                Else
                    If gameW% = 2 And batRating(P, B3%(P, gameB0%(2, 0)), 16) > 6 Then
                        A5%(1) = 1
                        A5%(2) = 1
                        A5%(0) = 11
                    End If
                End If

            End If

        End If

    End If

    GoTo 13684


    '-----------------------------------------------------
    '--- THIS IS CRITICAL FOR PBP MOVING AUTOMATICALLY ---
    '-----------------------------------------------------
    ' Play-by-Play box
    13900 '
    If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 144)

    ' essentially it allows the user to press a key
    ' during game play to activate certain functions
    ' or special menus, etc.
    I$ = InKey$

    If UCase$(I$) = "Q" Then
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
    H0% = 0
    INFPOS% = 0
    F% = 0
    H0%(0) = 0

    If nbrOuts(0) = 3 Then
        GoTo 15000
    Else
        GoTo 15050
    End If

    '----- NOODLE -----

    14100 ' *** BUNT ROUTINE ***
    Call WINDEX

    Erase A5%

    SH = 2
    S2% = 23

    If (S4% = 3 Or S4% > 4) And nbrOuts(0) < 2 Then Call SQUEEZECHOICE(SH)

    Call d100(RN)

    If B7%(P, B1!(P)) = 1 Then BR = pitchRating(P, P1%(P), 32)
    If B7%(P, B1!(P)) <> 1 Then BR = batRating(P, B3%(P, B1!(P)), 30)
    If INFPOS% > 0 Then BR = BR - 2
    If BR <= 1 Then BR = 1

    For I = 1 To 9:
        If RN <= BT%(tmRat_GAME(1, 10), BR, I) Then Exit For
    Next

    WW% = Int(Rnd(1) * 10)
    If WW% <= 2 Then gameW% = 5
    If WW% > 2 And WW% <= 4 Then gameW% = 1
    If WW% > 4 And WW% <= 7 Then gameW% = 3
    If WW% > 7 Then gameW% = 2
    Call GETFIELDNAME(gameW%, D)

    Call d100(RN)

    If RN <= 52 Then 14175

    If I = 5 And SH = 1 Then I = 6

    'This I and the ones in the case statement
    'all have to do with the For-Next loop
    Select Case I

        Case 1
            'PU
            Call PBP(player$(0) + " bunts it in the air...")
            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            Call PBP(player$(4) + " makes the catch")
            Call ADDSTTS(P, D)

            nbrOuts(0) = nbrOuts(0) + 1
            nbrOuts(1) = nbrOuts(1) + 1
            Call CREDITREGPO(gameW%, D)

            If nbrOuts(0) <> 3 And SH <> 2 Then
                '--- start of a new sub ???
                '--- where does 4111 end ???
                14111 '
                If gameW% <> 5 Then
                    Call CREDITASSIST(gameW%, D)
                    gameW% = 5
                    Call CREDITREGPO(gameW%, D)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(gameW%, D)
                Else
                    Call CREDITASSIST(gameW%, D)
                    gameW% = 6
                    Call CREDITREGPO(gameW%, D)
                    player$(5) = player$(4)
                    Call GETFIELDNAME(gameW%, D)
                End If

                Call PBP(player$(5) + " throws to " + player$(4) + " at 3rd")
                A5%(3) = 10
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                I5 = P1%(D)
                I6 = 0

                Call ADDPIT(D, I5, I6)

                If SH = 1 Then
                    If nbrOuts(0) < 3 Then
                        GoTo 15051
                    Else
                        'nbrOuts(0) >= 3
                        B1!(P) = B1!(P) - 1
                    End If
                End If

            End If

            GoTo 13900

        Case 2
            'DP
            If S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7 Then

                Call PBP(player$(0) + " lays down a good bunt")

                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                If F% = 1 Then 13900

                Call PBP(player$(4) + " fields it cleanly")

                If SH = 1 And (S4% = 5 Or S4% = 7) Then

                    If gameW% = 2 Then
                        Call PBP("and catches " + player$(3) + " heading home!")
                        Call CREDITREGPO(gameW%, D)
                    Else
                        'gameW% <> 2
                        Call PBP("and throws home to nail " + player$(3) + "!")
                        Call CREDITASSIST(gameW%, D)
                        gameW% = 2
                        Call CREDITREGPO(gameW%, D)
                    End If

                    A5%(3) = 11

                    For I = 0 To 2: A5%(0) = 1: Next

                    '14122

                    Call ADDSTTS(P, D)
                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    GoTo 13900

                End If

                If S4% = 7 And nbrOuts(0) < 2 Then

                    A5%(3) = 11

                    If gameW% = 2 Then
                        Call PBP("He steps on home for one...")
                        Call CREDITREGPO(gameW%, D)
                        Call CREDITASSIST(gameW%, D)
                    Else
                        'gameW% <> 2
                        Call PBP("He throws home for the force")
                        Call CREDITASSIST(gameW%, D)
                        gameW% = 2
                        Call CREDITREGPO(gameW%, D)
                        Call CREDITASSIST(gameW%, D)
                    End If

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call PBP("Back to first...")

                    A5%(0) = 11

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                    Call PBP("DOUBLE PLAY!!")

                    nbrOuts(0) = nbrOuts(0) + 2
                    nbrOuts(1) = nbrOuts(1) + 2

                    Call ADDSTTS(P, D)

                    I5 = P1%(D)
                    I6 = 0

                    Call ADDPIT(D, I5, I6)

                    If nbrOuts(0) < 3 Then
                        A5%(2) = 1
                        A5%(1) = 1
                        Call ADVANCEMENT(P, D, S2%, F%, I3)
                    End If

                    GoTo 13900

                End If

                Call PBP("He throws to 2nd")
                Call CREDITASSIST(gameW%, D)

                A5%(1) = 11

                Call ADDSTTS(P, D)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If nbrOuts(0) < 3 Then

                    A5%(0) = 11
                    Call CREDITASSIST(gameW%, D)

                    A5%(2) = 1
                    Call PBP("over to first...")
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call PBP("DOUBLE PLAY!!")
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    I5 = P1%(D)
                    I6 = 0
                    Call ADDPIT(D, I5, I6)
                End If

                GoTo 13900

            Else
                Call PBP(player$(0) + " lays down the bunt...")
                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                Call PBP(player$(4) + " makes a nice fielding play")
                Call CREDITASSIST(gameW%, D): A5%(0) = 11
                If SH = 1 And (S4% = 3 Or S4% = 6) Then A5%(3) = 11: For I = 0 To 2: A5%(I) = 1: Next

                '14122
                Call ADDSTTS(P, D)
                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)
                GoTo 13900
            End If

        Case 3
            Call LDRNRCTDN
            GoTo 13900

        Case 4
            14140 'SHGOOD
            Call PBP(player$(0) + " lays down a good bunt...")
            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            Call PBP(player$(4) + " makes a nice play")

            If S4% = 0 Then

                S2% = -12
                Call PBP(player$(0) + " beats out an infield hit!")
                GoTo 13065

            Else
                '-- turn this into a unique sub?
                '-- or direct other code to this loop
                '-- with a value of 4 and a 'skip' variable
                14141 '
                I5 = P1%(D)
                I6 = 15

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then
                    I5 = P1%(P): I6 = 40: Call INCPITOFF(P, I5, I6)
                Else
                    I5 = B3%(P, B1!(P)): I6 = 18: Call INCBATOFF(P, I5, I6)
                End If

                Call d100(RN)

                If S4% = 5 And SH = 2 And RN > 98 Or RN > 97 Then
                    S2% = -12
                    Call PBP(player$(0) + " beats it out!")
                    GoTo 13065
                End If

                A5%(0) = 11

                Call PBP("The only play is to first")

                I5 = P1%(D)
                I6 = 0

                Call ADDPIT(D, I5, I6)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1

                Call CREDITASSIST(gameW%, D)
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                If nbrOuts(0) < 3 Then
                    For I = 1 To 3: A5%(I) = 1: Next
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                End If

            End If

            GoTo 13900

        Case 5
            14150 'C@3
            Call d100(RN)
            If B7%(P, B1!(P)) = 1 Then RR = 2
            If B7%(P, B1!(P)) <> 1 Then RR = batRating(P, B3%(P, B1!(P)), 16)
            If INFPOS% > 0 Then RR = RR - 2
            If RR <= 2 Then RR = 2

            For I = 0 To 2
                If RN <= CF%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1

                    If S4% > 0 Then 14157
                    Call PBP(player$(0) + " drops it right down the line")
                    Call PBP(player$(4) + " lets it roll...")
                    Call PBP(player$(0) + " is trying for 2nd!!")
                    Call PBP(player$(4) + " fields and throws too late!!")
                    Call PBP("You won't see that play very often")

                    A5%(0) = 2
                    I5 = P1%(D)
                    I6 = 1
                    Call ADDPIT(D, I5, I6)

                    If B7%(P, B1!(P)) <> 1 Then
                        I5 = B3%(P, B1!(P))
                        I6 = 1
                        Call INCBATOFF(P, I5, I6)
                        I6 = 4
                        Call INCBATOFF(P, I5, I6)
                        I6 = 0
                        Call INCBATOFF(P, I5, I6)
                    Else
                        I5 = P1%(P)
                        I6 = 23
                        Call INCPITOFF(P, I5, I6)
                        I6 = 24
                        Call INCPITOFF(P, I5, I6)
                        I6 = 27
                        Call INCPITOFF(P, I5, I6)
                    End If

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD

                    GoTo 13900

                Case 2
                    14157 '
                    S2% = 12
                    Call PBP(player$(0) + " lays down a great bunt...")

                    Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                    If F% = 1 Then 13900

                    Call PBP(player$(4) + " fields... but too late!")
                    S2% = -12
                    GoTo 13065

                Case 3
                    GoTo 14140

            End Select

        Case 6
            'PLYLDBS
            If B7%(P, B1!(P)) = 1 Then RR = 2
            If B7%(P, B1!(P)) <> 1 Then RR = batRating(P, B3%(P, B1!(P)), 16)
            If INFPOS% > 0 Then RR = RR - 2
            If RR <= 2 Then RR = 2

            If S4% = 0 Then 14150
            If S4% = 1 Then I3 = 1: J3$ = "2nd"
            If S4% = 2 Or S4% = 4 Then I3 = 2: J3$ = "3rd"
            If S4% = 3 Or S4% > 4 Then I3 = 3: J3$ = "home"

            Call PBP(player$(0) + " gets the bunt down...")

            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

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

            Call d100(RN)

            For I = 0 To 1
                If RN <= LB%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            If I = 0 Then
                PLB = 1
                Call LDRNRCTDN
            Else
                Call PBP(player$(4) + " throws to " + J3$ + "... SAFE!!")
                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6)
                For I = 0 To 3: A5%(I) = 1: Next
                Call ADVANCEMENT(P, D, S2%, F%, I3)
            End If

            GoTo 13900

        Case 7
            'FLKSO
            Call d100(RN)

            If pitchRating(D, P1%(D), 6) = 0 Then
                BR = 0
            Else
                BR = pitchRating(D, P1%(D), 9) / pitchRating(D, P1%(D), 6)
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
                    GoTo 13601

                Case 2
                    nbrStrikes = nbrStrikes + 1

                    If useVGA = 1 Then
                        PrintOnGfx 64, 28, Str$(nbrStrikes), imageScreen&
                    Else
                        Locate 2, 58: Print nbrStrikes
                    End If

                    Call WINDEX

                    Call d100(RN)
                    pbpString$ = player$(0) + " "
                    If RN <= 13 Or SH = 1 And RN <= 42 Then pbpString$ = pbpString$ + "misses the pitch"
                    If RN > 13 And RN <= 42 And SH <> 1 Then pbpString$ = pbpString$ + "takes a called strike"
                    If RN > 42 Then pbpString$ = pbpString$ + "bunts it foul"
                    Call PBP(pbpString$)

                    If nbrStrikes = 3 Then 13601

                    If SH = 1 And RN <= 42 Then gameW% = 2: Call GETFIELDNAME(gameW%, D): GoTo 14111

                    If nbrStrikes = 1 Then
                        Call PBP("He has" + Str$(nbrStrikes) + " strike")
                    Else
                        'nbrStrikes > 1
                        Call PBP("He has" + Str$(nbrStrikes) + "strikes")
                    End If
                    Call PBP(pbpString$)

                    If S4% = 4 Or S4% = 2 Then INFPOS% = 0

                    If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)

                    GoTo 15080

                Case 3
                    GoTo 14140

            End Select

        Case 8
            'TFPLY
            Call PBP(player$(0) + " lays down the bunt...")
            Call PBP(player$(4) + " has a tough play")

            Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
            If F% = 1 Then 13900

            If gameW% = 1 Then RR = 3
            If gameW% <> 1 Then RR = batRating(D, B9%(D, gameW%), 17)
            If INFPOS% > 0 And S4% <> 0 Then RR = RR + 1 + tmRat_GAME(1, 10)
            If INFPOS% > 0 And S4% = 0 Then RR = RR + tmRat_GAME(1, 10)
            If INFPOS% = 0 And S4% = 0 Then RR = RR - 1
            If RR > 11 Then RR = 11
            If RR < 0 Then RR = 0

            Call d100(RN)

            For I = 0 To 2
                If RN <= gameTP%(RR, I) Then Exit For
            Next

            'This I has to do with the above For-Next
            Select Case I + 1
                Case 1:
                    S2% = 12
                    Call PBP("but too late...base hit!")
                    GoTo 13065

                Case 2:
                    PLB = 1
                    Call LDRNRCTDN
                    GoTo 13900

                Case 3:
                    GoTo 14141

            End Select

        Case 9
            14175 '
            nbrStrikes = nbrStrikes + 1
            If useVGA = 1 Then
                PrintOnGfx 64, 28, Str$(nbrStrikes), imageScreen&
            Else
                Locate 2, 58: Print nbrStrikes
            End If

            Call WINDEX
            Call d100(RN)

            pbpString$ = player$(0) + " "
            If RN <= 13 Or SH = 1 And RN <= 42 Then pbpString$ = pbpString$ + "misses the pitch"
            If RN > 13 And RN <= 42 And SH <> 1 Then pbpString$ = pbpString$ + "takes a called strike"
            If RN > 42 Then pbpString$ = pbpString$ + "bunts it foul"
            Call PBP(pbpString$)

            If nbrStrikes = 3 Then
                GoTo 13601
            End If

            If SH = 1 And RN <= 42 Then gameW% = 2: Call GETFIELDNAME(gameW%, D): GoTo 14111

            If nbrStrikes = 1 Then
                Call PBP("He has" + Str$(nbrStrikes) + " strike")
            Else
                'nbrStrikes > 1
                Call PBP("He has" + Str$(nbrStrikes) + "strikes")
            End If

            If S4% = 4 Or S4% = 2 Then INFPOS% = 0

            If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)

            GoTo 15080

    End Select

    '-- at this point, each selection in the Case statement
    '-- should have directed the code somewhere else
    '-- in essence, this is kind of the end of this code


    '--------------------


    14800 '
    SH% = 0
    S2% = 16
    gameW% = 1
    player$(4) = pitchers$(D, P1%(D))

    Select Case S4%
        Case 0
            GoTo 15051
            'set a "skip" variable here

        Case 1, 5

            TGTBASE% = 2

            If P2 <> 2 Then

                Call WINDEX
                SB% = 1
                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                SB% = 0

                If F% = 1 Then
                    B1!(P) = B1!(P) - 1
                    Call WHOSON1ST(S4%)
                    GoTo 13900
                End If

            End If

        Case 2, 4

            TGTBASE% = 3

            If P2 <> 2 Then

                Call WINDEX
                SB% = 1
                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)
                SB% = 0

                If F% = 1 Then
                    B1!(P) = B1!(P) - 1
                    Call WHOSON1ST(S4%)
                    GoTo 13900
                End If

            End If

        Case 3, Is >= 6
            TGTBASE% = 4

    End Select

    I2 = B3%(P, gameB0%(TGTBASE% - 1, 0))
    For I3 = 1 To 2
        A5%(I3) = 1
        A5%(3) = 0
    Next I3

    Call STEALCHANCE(AA, I2, TGTBASE%, CHANCE%)
    If B7%(P, gameB0%(TGTBASE% - 1, 0)) = 1 Then CHANCE% = 0

    'Reduce chance for stealing home
    If TGTBASE% = 4 Then CHANCE% = CHANCE% * .05

    If batRating(P, I2, 14) > 0 Then Q0 = Int((batRating(P, I2, 13) / (batRating(P, I2, 13) + batRating(P, I2, 14))) * 1000): GoTo 14813
    If batRating(P, I2, 13) > 31 Then Q0 = 700 + ((batRating(P, I2, 13) - 30) * 2): GoTo 14813
    If batRating(P, I2, 13) > 10 Then Q0 = 600 + ((batRating(P, I2, 13) - 10) * 10): GoTo 14813
    If batRating(P, I2, 13) > 0 Then Q0 = 400 + ((batRating(P, I2, 13) * 20))
    Q0 = 100

    14813 '
    If B7%(P, gameB0%(TGTBASE% - 1, 0)) = 1 Then Q0 = 100

    Call STEAL3RD(TGTBASE%, AA, Q0, H0%, D) ' *** SUCCESS OF 3B STEAL ***

    If pitchRating(D, P1%(D), 0) = 1 And TGTBASE% = 2 Then CHANCE% = CHANCE% + 25: Q0 = Q0 + 10
    If pitchRating(D, P1%(D), 0) = -1 And TGTBASE% = 2 Then CHANCE% = CHANCE% - 50: Q0 = Q0 - 30
    If P2 = 2 Then

        CHANCE% = 100
        Q0 = Q0 - 200
        gameW% = 2
        player$(4) = batters$(D, B9%(D, 2))
        TGBASE% = B3%(P, gameB0%(TGTBASE% - 1, 0))
        gameB0%(TGTBASE% - 1, 2) = 18

        Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)

        If F% = 1 Then
            I5 = TGBASE%
            I6 = 9
            Call INCBATOFF(P, I5, I6)
            B1!(P) = B1!(P) - 1
            Call STRIKEOUTCS(SO)
            GoTo 13900
        Else
            GoTo 14827
        End If

    End If

    gameB0%(TGTBASE% - 1, 2) = gameB0%(TGTBASE% - 1, 2) + 1

    Call d100(I4)

    If I4 <= gameB0%(TGTBASE% - 1, 2) Then

        S2% = 22
        gameW% = 1
        player$(4) = pitchers$(D, P1%(D))

        14820 '
        Call WINDEX
        Call PBP(player$(TGTBASE% - 1) + " caught off base in a rundown!")

        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 0
        A5%(TGTBASE% - 1) = 11

        nbrOuts(0) = nbrOuts(0) + 1
        nbrOuts(1) = nbrOuts(1) + 1
        Call PUTOUT(TGTBASE%, P)
        Call ADVANCEMENT(P, D, S2%, F%, I3)
        Call CREDITPUTOUT(I3, FB2P%, AST%, D)

        I5 = P1%(D)
        I6 = 0
        Call ADDPIT(D, I5, I6)

        If nbrOuts(0) >= 3 Then
            B1!(P) = B1!(P) - 1
            GoTo 13900
        End If

        INFPOS% = 0

        GoTo 15051

    End If

    '----------------------------------------
    '-- this is the start a new large area of code
    '-- perhaps it could become a sub?
    '----------------------------------------

    '14825
    'RUNNER HAS A CHANCE TO STEAL
    gameB0%(TGTBASE% - 1, 2) = gameB0%(TGTBASE% - 1, 2) + 17
    I9 = Int(Rnd(1) * 1000) + 1

    If I9 > CHANCE% * 1.388 And D5 <> 3 Then

        Call WINDEX
        Call NOJUMP(D)

        If useVGA = 1 Then _PutImage (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80), backupScreen&, imageScreen&, (17 * FontColAdj, 128)-Step(46 * FontColAdj, 80)

        GoTo 15085

    End If

    14826 '
    Call WINDEX
    TGBASE% = B3%(P, gameB0%(TGTBASE% - 1, 0))

    Call GetPBPString(20, 3090, Y$, D)
    Call PBP(Y$)

    Call GetPBPString(13, 3110, Y$, D)
    Call PBP(Y$)

    Call PITCHOUT(S4%, TGTBASE%, Q0)

    If S4% <> 5 Then 14888 ' *** DOUBLE STEAL ***1st&3rd

    If SH% = 1 Then
        I9 = Int(Rnd(1) * 1000) + 1
    Else
        If playerMode = 2 Or playerMode = 1 And P <> compTeam Then
            I$ = "Y"
        Else
            Call PBP("Throw to 2B? (Y/N) ")
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then PJ = 7: GoTo 14829

        14888 '
        gameW% = 2
        player$(4) = batters$(D, B9%(D, 2))

        Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)

        If F% = 1 Then

            I5 = TGBASE%
            I6 = 9
            Call INCBATOFF(P, I5, I6)
            Call PITSWAP(TGTBASE%)
            B1!(P) = B1!(P) - 1
            GoTo 13900

        End If

        14827 '
        Call WHICHRUNNER(TGTBASE%, S4%, D5)
        I9 = Int(Rnd(1) * 1000) + 1
        Q0 = Q0 - ((batRating(D, B9%(D, 2), 15) - 5) * 25)

    End If

    RN = Int(Rnd(1) * 11) + 1
    If TGTBASE% <> 4 Then
        If RN = 1 Then pbpString$ = batters$(D, B9%(D, 2)) + " rifles it to " + baseName$(TGTBASE%)
        If RN = 2 Then pbpString$ = batters$(D, B9%(D, 2)) + " fires it to " + baseName$(TGTBASE%)
        If RN = 3 Then pbpString$ = batters$(D, B9%(D, 2)) + " unloads it"
        If RN = 4 Then pbpString$ = batters$(D, B9%(D, 2)) + " is up and firing"
        If RN = 5 Then pbpString$ = batters$(D, B9%(D, 2)) + " comes up firing"
        If RN = 6 Then pbpString$ = batters$(D, B9%(D, 2)) + " lets it fly"
        If RN = 7 Then pbpString$ = batters$(D, B9%(D, 2)) + " throws down"
        If RN = 8 Then pbpString$ = batters$(D, B9%(D, 2)) + " gets it off"
        If RN = 9 Then pbpString$ = batters$(D, B9%(D, 2)) + " fires a missile"
        If RN = 10 Then pbpString$ = batters$(D, B9%(D, 2)) + " throws quickly"
        If RN = 11 Then pbpString$ = batters$(D, B9%(D, 2)) + " throws"
        Call PBP(pbpString$)
    End If

    If Not (gameB0%(TGTBASE% - 1, 2) = 18 And I9 > Q0 Or gameB0%(TGTBASE% - 1, 2) > 18 And I9 > Q0 - 200) Then

        14829
        Call d100(RN)
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

        gameB0%(TGTBASE% - 1, 2) = 0
        INFPOS% = 0
        Call PITSWAP(TGTBASE%)
        Call ADVANCEMENT(P, D, S2%, F%, I3)

        I5 = B9%(D, 2)
        I6 = 20
        Call INCBATDF(D, I5, I6)

        If SH% <> 1 Then

            If Not (S4% <> 5 Or S4% = 5 And PJ = 7) Then
                PJ = 0
                TGTBASE% = 4
                SS% = Int(Rnd(1) * 20) + 1

                If SS% = 6 Then
                    GoTo 14820
                Else
                    GoTo 14831
                End If
            Else
                GoTo 14831
            End If
        Else
            SH% = 0
            GoTo 15051
        End If

    End If

    Call d100(RN)

    Select Case RN
        Case 1 To 20
            pbpString$ = batters$(D, B9%(D, 2)) + " nails " + player$(TGTBASE% - 1)

        Case 21 To 40
            pbpString$ = player$(TGTBASE% - 1) + " is pegged at " + baseName$(TGTBASE%)

        Case 41 To 60
            pbpString$ = "Great throw by " + batters$(D, B9%(D, 2)) + "!!"

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
    INFPOS% = 0
    I5 = B3%(P, gameB0%(TGTBASE% - 1, 0))
    I6 = 10

    Call INCBATOFF(P, I5, I6)

    I5 = P1%(D)
    I6 = 0

    Call ADDPIT(D, I5, I6)

    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1
    gameW% = 2

    Call CREDITASSIST(gameW%, D)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

    I5 = B9%(D, 2)
    I6 = 21

    Call INCBATDF(D, I5, I6)

    If nbrOuts(0) >= 3 Then
        B1!(P) = B1!(P) - 1
        Call STRIKEOUTCS(SO)
        GoTo 13900
    End If

    14831 '
    If nbrOuts(0) >= 3 Or P2 = 2 And nbrStrikes = 3 Then
        GoTo 13900
    End If

    If SH% = 1 Then
        SH% = 0
        GoTo 15051
    End If

    If AA < 20 Or S4% <> 5 Or B7%(P, gameB0%(3, 0)) = 1 Then
        GoTo 15051
    End If

    Call WINDEX

    If playerMode = 2 Or playerMode = 1 And P = compTeam Then
        I$ = "N"
    Else
        Call PBP("TRY TO STEAL HOME? (Y/N)")
        I$ = GetKeyPress$
    End If

    If UCase$(I$) = "Y" Then

        GoTo 15051

    Else
        SH% = 1
        TGTBASE% = 4
        A5%(1) = 0
        A5%(2) = 0
        A5%(3) = 1
        I2 = B3%(P, gameB0%(TGTBASE% - 1, 0))
        CHANCE% = 100
        Q0 = 700

        GoTo 14826 ' *** STEAL HOME ***

    End If


    '----------------------------------------
    '-- this is a major area of code, but
    '-- it's also short. perhaps it could
    '-- be made into a subroutine?
    '----------------------------------------

    14900 '
    Cls
    If useVGA = 0 Then Screen 0
    P9 = D
    B1!(P) = B1!(P) + 1
    nbrStrikes = 0
    If B1!(P) = 10 Then B1!(P) = 1

    Color 15
    Print "DOES "; batters$(D, B3%(D, B9%(D, 0))); " STAY IN THE GAME? (Y/N)"
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
            If B7%(P9, I) = 1 And I5 = 66 Then Print batters$(P9, B3%(P9, I));
            If B7%(P9, I) = 1 And I5 <> 66 Then Print pitchers$(P9, B3%(P9, I));
            If B7%(P9, I) <> 1 Then Print batters$(P9, B3%(P9, I));
            Print " "; fieldPos$(B7%(P9, I));
            Color 3
        Next I

        I5 = 0

        Do
            Color 15
            Locate 3, 1: Print "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"
            I$ = GetKeyPress$
            I2 = Val(I$)
        Loop Until I2 >= 1 And I2 <= 9

        If B7%(D, I2) = 10 Then B7%(D, I2) = 1

        INFPOS% = 0
        B7%(D, B9%(D, 0)) = B7%(D, I2)
        B9%(D, B7%(D, B9%(D, 0))) = B3%(D, B9%(D, 0))
        B7%(D, I2) = 1
        I7% = D
        I8% = B9%(D, 0)
        I9% = B3%(D, B9%(D, 0))

        Call REPLACEMENTS(I7%, I8%, I9%)

        I5 = 66

    End If

    GoTo 15504


    '-----------------------------------
    '-----      PERSONNEL MENU     -----
    '-----------------------------------
    '-- make this into a subroutine?!?!

    15500 '
    Cls
    If useVGA = 1 Then Screen 0
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
                If B7%(P9, I) <> 1 Then
                    batRating(P9, B3%(P9, I), 21) = 1
                End If
            Next I

            If useVGA = 1 Then
                Call LOADSCREEN
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
            End If

            GoTo 15051

        Case 2
            15504 '
            If P2 = 5 Then Call TIRING(P9): GoTo 15501

            Do

                Call PITDISP(P9)
                Locate 24, 1: Print "SELECT RELIEVER (Z = NO CHANGE)   ";
                If B9%(P9, 1) <> 99 Then Call TIRING(P9)
                Print

                Do

                    I$ = GetKeyPress$
                    If UCase$(I$) = "Z" Then
                        GoTo 15500
                    Else
                        Call LetterToNumber(I$, I1)
                    End If

                Loop Until I1 <= 21 And pitchRating(P9, I1, 15) <= 0

                Locate 23, 1: Print String$(78, 32)
                Locate 23, 1: Print "YOU CHOSE "; pitchers$(P9, I1); " "; B1$(pitchRating(P9, I1, 0) + 2); " - ";

                Print "ARE YOU SURE? (Y/N)";
                I$ = GetKeyPress$

            Loop Until UCase$(I$) = "Y"

            PPH = 0
            PR% = 0

            If S1%(P9) <> -1 And P6%(P9) > 1 Then I5 = P1%(P9): I6 = 22: Call OFFPIT(P9, I5, I6)

            P1%(P9) = I1
            P6%(P9) = P6%(P9) + 1
            pitchRating(P9, I1, 15) = P6%(P9)
            Call PITSTAM(P9)
            Call CHECKSAVE(D, P, S4%)

            15518 '
            P9 = 1 - P9
            W5 = P9
            W6 = W5
            VV(P9) = 1
            VV1(P9) = 9
            Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

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
                        Print "WHO WILL REPLACE "; batters$(D, B3%(D, V2)); " @ "; fieldPos$(B7%(D, V2));

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

                        gameD0%(D) = gameD0%(D) + 1
                        X0%(D, 0, gameD0%(D)) = V3
                        X0%(D, 1, gameD0%(D)) = B7%(D, V2)

                        For V0 = 1 To 9
                            If B7%(D, V0) = 1 Then X0%(D, 2, gameD0%(D)) = (V0 * 10) + (gameD0%(D) - 9)
                        Next V0

                        For V0 = 1 To 9
                            If B7%(D, V0) = 1 Then B7%(D, V0) = B7%(D, V2): B3%(D, V0) = V3: B9%(D, B7%(D, V0)) = V3: P9 = D: W5 = D: W6 = D: VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)
                            VV(D) = 1
                            VV1(D) = 9
                        Next V0

                        batRating(D, V3, 21) = 1
                        I7% = D
                        I8% = V2
                        I9% = V3

                        Call REPLACEMENTS(I7%, I8%, I9%)

                        I5 = 0

                        For V0 = 1 To 9
                            If V0 = V2 Then B7%(D, V2) = 1: B3%(D, V2) = P1%(D): B9%(D, 1) = P1%(D): VV(D) = V0: VV1(D) = V0: Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)
                        Next V0

                        VV(D) = 1
                        VV1(D) = 9

                        GoTo 15500 'Back to personnel menu

                    Else

                        GoTo 15518

                    End If

                End If

            End If

            I5 = 0

            For V0 = 1 To 9

                If B7%(D, V0) = 1 Or B7%(D, V0) = 10 Then

                    B3%(D, V0) = P1%(D)
                    B9%(D, 1) = P1%(D)
                    B7%(D, V0) = 1
                    P9 = D
                    W5 = D
                    W6 = D
                    VV(D) = V0
                    VV1(D) = V0

                    Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

                    VV(D) = 1
                    VV1(D) = 9
                    GoTo 15500

                End If

            Next V0

            GoTo 15540

        Case 3

            '--- START OF A SHORT SUB
            '-- ChangeBatterFielder

            15540 '
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
            If UCase$(I$) = "Z" Then 15500
            I1 = Val(I$)

            If I1 = 2 Then

                15567 '
                Do
                    Locate 20, 29: Print "B.O. #1 (1-9)"
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 1 And I1 <= 9 Or B7%(P9, I1) <> 1

                Do
                    Locate 21, 29: Print "B.O. #2 (1-9)"
                    I$ = GetKeyPress$
                    I2 = Val(I$)
                Loop Until I1 >= 1 And I1 <= 9 Or B7%(P9, I2) <> 1

                I3 = B7%(P9, I2)
                B7%(P9, I2) = B7%(P9, I1)
                B7%(P9, I1) = I3
                B9%(P9, B7%(P9, I1)) = B3%(P9, I1)
                B9%(P9, B7%(P9, I2)) = B3%(P9, I2)

                I7% = P9
                I8% = I2: I9% = B3%(P9, I2)
                Call REPLACEMENTS(I7%, I8%, I9%)

                I7% = P9
                I8% = I1
                I9% = B3%(P9, I1)
                Call REPLACEMENTS(I7%, I8%, I9%)

                GoTo 15540

            End If

            If I1 = 3 Then 15550
            If D2 = 6 Then 15550
            I2 = B1!(P): GoTo 15555

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

    Loop Until I2 >= 1 And I2 <= 9 And Not (P9 = D And B7%(P9, I2) = 1) Or UCase$(I$) = "Z"


    If UCase$(I$) <> "Z" Then

        Locate , 30
        If B7%(P9, I2) = 1 Then Print pitchers$(P9, P1%(P9));
        If B7%(P9, I2) <> 1 Then Print batters$(P9, B3%(P9, I2));

        15555 '
        Do
            Locate 22, 30: Print "# OF SUB"

            I$ = GetKeyPress$
            If Asc(I$) = 32 And PG = 0 Then PG = 1: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 32 And PG = 1 Then PG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 0 Then Call DisplayFielding_Lineup(PG, P9, TG): Call CHANGELINEUP(P9, P)
            If Asc(I$) = 9 And TG = 1 Then TG = 0: Call DisplayBatter_Lineup(PG, P9): Call CHANGELINEUP(P9, P)
            Call LetterToNumber(I$, I1)

        Loop Until I1 <= 22 Or batRating(P9, I1, 21) <= 0

        Do
            Locate 23, 30
            Print batters$(P9, I1); "... ARE YOU SURE? (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"


        If UCase$(I$) <> "N" Then

            batRating(P9, I1, 21) = 1
            B3%(P9, I2) = I1
            B9%(P9, B7%(P9, I2)) = I1

            If B7%(P9, I2) = 1 Or B7%(P9, I2) = 10 Then B9%(P9, 1) = 99: B9%(P9, 0) = I2: B7%(P9, I2) = 10

            gameD0%(P9) = gameD0%(P9) + 1
            X0%(P9, 0, gameD0%(P9)) = B3%(P9, I2)
            X0%(P9, 1, gameD0%(P9)) = B7%(P9, I2)
            X0%(P9, 2, gameD0%(P9)) = (I2 * 10) + (gameD0%(P9) - 9)
            W5 = P9
            W6 = P9
            VV(P9) = I2
            VV1(P9) = I2
            I7% = P9
            I8% = I2
            I9% = I1

            Call REPLACEMENTS(I7%, I8%, I9%)
            Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

            VV(P9) = 1
            VV1(P9) = 9

        End If

    End If

    GoTo 15540

    '----- NOODLE -----

    '----------------------------------------
    ' Print final score and run post game
    ' routines (scoreboard, stats, etc.)
    '----------------------------------------

    18500 '

    normalFinish = 1

    'Close #11

    'Print winning team and score
    If S6%(0, 0) > S6%(1, 0) Then
        Call PBP(gameTeams$(0) + " defeats " + gameTeams$(1) + Str$(S6%(0, 0)) + " TO" + Str$(S6%(1, 0)) + "!")
    Else
        Call PBP(gameTeams$(1) + " defeats " + gameTeams$(0) + Str$(S6%(1, 0)) + " TO" + Str$(S6%(0, 0)) + "!")
    End If

    If autoPlay <> 1 Or playerMode <> 2 Then
        If pbpDelay! = 0 Then pbpDelay! = 0.1
        'Delay of 0 will not present the text
        'But we want to be sure the user sees it
        Call PBP("PRESS ANY KEY TO CONTINUE")
        I$ = GetKeyPress$
    End If

    Call EndOfGame
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
    18700 '
    H0% = 0
    If useVGA = 1 Then
        Call HOLDSTATUS_G(H0%)
    Else
        Call HOLDSTATUS(H0%)
    End If

    A1 = S6%(D, 0) - S6%(P, 0)

    If Not (A1 > 1 Or S4% = 3 Or S4% >= 6) Then

        If S4% <> 1 And S4% <> 5 Then
            I2 = 2
        Else

            I2 = 1
            J = gameB0%(I2, 0)

            If J <> 0 Then

                If B7%(P, J) <> 1 Then

                    If B7%(P, J) <> 1 And batRating(P, B3%(P, J), 13) >= 10 Then
                        H0% = 1
                        If useVGA = 1 Then
                            Call HOLDSTATUS_G(H0%)
                        Else
                            Call HOLDSTATUS(H0%)
                        End If
                    End If

                End If

            End If

        End If

    End If

    If SU(D, 0) <> -1 And MG%(D, 589) = 0 And INNING% = MG%(D, 588) Then Call PITSTRAT(PPH, pz)
    If SU(D, 1) <> -1 And MG%(D, 589) = 0 And INNING% > MG%(D, 588) Then Call PITSTRAT(PPH, pz)
    If SU(D, 1) <> -1 And MG%(D, 589) <> 0 And INNING% >= MG%(D, 588) Then Call PITSTRAT(PPH, pz)

    If pz = 1 Then pz = 0: GoTo 15055

    A1 = S6%(D, 0) - S6%(P, 0)
    Call d100(RN)

    If Not (INNING% < 9 Or CZ%(D) = -1 Or MG%(D, 581) = 999) Then

        CZ%(D) = -1

        If A1 <= 3 And A1 >= 0 Then

            If P6%(D) <> 1 Then
                Call d100(RN)
                If RN <= 80 And pitchRating(D, P1%(D), 3) < 15 Then GK%(D) = 1

            Else

                If S6%(P, 0) = 0 And pitchRating(D, P1%(D), 5) > 0 And RN <= Int(pitchRating(D, P1%(D), 16) / pitchRating(D, P1%(D), 5) * 100 + .5) + 10 Then 18725
                If pitchRating(D, P1%(D), 5) > 0 And RN <= Int(pitchRating(D, P1%(D), 16) / pitchRating(D, P1%(D), 5) * 100 + .5) Then 18725
                Call d100(RN0)
                If pitchEraOpt > 0 And INNING% > 7 And S6%(D, 0) > S6%(P, 0) And pitchRating(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(pitchRating(D, P1%(D), 16) / pitchRating(D, P1%(D), 1) * 100 + .5) <= 100 And RN0 <= Int(pitchRating(D, P1%(D), 16) / pitchRating(D, P1%(D), 1) * 100 + .5) Then 18725
                If pitchEraOpt > 0 And INNING% > 7 And (S6%(D, 0) >= S6%(P, 0) Or S6%(P, 0) - S6%(D, 0) <= 2) And pitchRating(D, P1%(D), 1) > 0 And P6%(D) = 1 And Int(pitchRating(D, P1%(D), 16) / pitchRating(D, P1%(D), 1) * 100 + .5) >= 100 Then 18725
                If pitchEraOpt >= 8 Then 18725

                GK%(D) = 1

            End If

        End If

    End If

    18725 '
    If GK%(D) = 1 Then

        GK%(D) = 0
        Call RELIEVERS(PPH, pz)
        If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
        GoTo 15055

    End If

    If B7%(P, B1!(P)) = 1 Then BR = 0
    If B7%(P, B1!(P)) <> 1 Then BR = batRating(P, B3%(P, B1!(P)), 30)

    If pitchRating(P, P1%(P), 11) <> 999 Then
        I7! = pitchRating(I0, P1%(I0), 11)
    Else
        If pitchRating(P, P1%(P), 20) = 0 Then
            I7! = 0
        Else
            I7! = Int((pitchRating(P, P1%(P), 22) / pitchRating(P, P1%(P), 20) * 1000 + .5))
        End If
    End If

    D2 = 1

    If INFPOS% <= 0 Then

        If MG%(D, 690) = 999 Then

            If Not (S4% < 3 Or S4% = 4) Then

                ' PROFILE INFIELD IN WITH 3RD OCCUPIED
                Call d100(RN)
                If nbrOuts(0) = 0 And A1 >= MG%(D, 691) * -1 And A1 <= MG%(D, 692) * -1 And INNING% >= MG%(D, 693) And RN <= MG%(D, 694) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If nbrOuts(0) = 0 And A1 >= MG%(D, 695) * -1 And A1 <= MG%(D, 696) * -1 And INNING% >= MG%(D, 697) And RN <= MG%(D, 698) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If nbrOuts(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 699) * -1 And A1 <= MG%(D, 700) * -1 And INNING% >= MG%(D, 701) And RN <= MG%(D, 702) Then INFPOS% = 2: D2 = 3: GoTo 15055
                If nbrOuts(0) = 1 And (S4% = 5 Or S4% = 7) And A1 >= MG%(D, 703) * -1 And A1 <= MG%(D, 704) * -1 And INNING% >= MG%(D, 705) And RN <= MG%(D, 706) Then INFPOS% = 2: D2 = 3: GoTo 15055

            End If

        End If

        If MG%(D, 707) = 999 Then

            If S4% = 0 Then

                ' PROFILE CORNERS IN AGAINST A BUNT HIT
                Call d100(RN)
                If BR >= MG%(D, 708) And RN <= MG%(D, 709) Or BR >= MG%(D, 710) And RN <= MG%(D, 711) Or BR >= MG%(D, 712) And RN <= MG%(D, 713) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

            End If

        End If

        If MG%(D, 714) = 999 And B7%(P, B1!(P)) <> 1 Then

            If nbrOuts(0) = 0 Then

                If Not (S4% <> 1 And S4% <> 4) Then

                    ' PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
                    Call d100(RN)
                    If A1 <= MG%(D, 715) And A1 >= MG%(D, 716) * -1 And S4% = 1 And BR >= MG%(D, 717) And RN <= MG%(D, 718) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 719) And A1 >= MG%(D, 720) * -1 And S4% = 1 And BR >= MG%(D, 721) And RN <= MG%(D, 722) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 723) And A1 >= MG%(D, 724) * -1 And S4% = 4 And BR >= MG%(D, 725) And RN <= MG%(D, 726) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If
                    If A1 <= MG%(D, 727) And A1 >= MG%(D, 728) * -1 And S4% = 4 And BR >= MG%(D, 729) And RN <= MG%(D, 730) Then
                        INFPOS% = 1
                        D2 = 4
                        GoTo 15055
                    End If

                End If

            End If

        End If

        If Not (MG%(D, 731) <> 999 Or B7%(P, B1!(P)) <> 1 Or nbrOuts(0) = 2) Then

            If Not (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

                ' PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
                Call d100(RN)

                If I7! <= MG%(D, 732) And RN <= MG%(D, 733) And S4% = 1 And nbrOuts(0) <= 1 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

                If I7! <= MG%(D, 734) And RN <= MG%(D, 735) And S4% = 1 And nbrOuts(0) <= 1 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

                If I7! <= MG%(D, 736) And RN <= MG%(D, 737) And (S4% = 2 Or S4% = 4) And nbrOuts(0) = 0 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

                If I7! <= MG%(D, 738) And RN <= MG%(D, 739) And (S4% = 2 Or S4% = 4) And nbrOuts(0) = 0 Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If

            End If

        End If

        If MG%(D, 740) = 999 Then

            If Not (nbrOuts(0) = 2 Or S4% <> 3 And S4% < 5) Then

                ' PROFILE CORNERS IN FOR A SQUEEZE
                Call d100(RN)
                If nbrOuts(0) = 0 And A1 <= MG%(D, 741) And A1 >= MG%(D, 742) And BR >= MG%(D, 743) And RN <= MG%(D, 744) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If
                If nbrOuts(0) = 1 And A1 <= MG%(D, 745) And A1 >= MG%(D, 746) And BR >= MG%(D, 747) And RN <= MG%(D, 748) Then
                    INFPOS% = 1
                    D2 = 4
                    GoTo 15055
                End If


            End If

        End If

        If MG%(D, 756) = 999 Then

            If Not (S4% = 1 Or S4% = 4 Or S4% = 5 Or S4% = 7) Then

                MOB% = 0

                For I = 1 To 3
                    If gameB0%(I, 0) > 0 Then MOB% = MOB% + 1
                Next

                If A1 + MOB% + 1 < 0 Then
                    ' BATTER NOT TYING OR WINNING RUN
                    Call d100(RN)

                    If batRating(P, B3%(P, B1!(P)), 4) = 0 Then
                        I7! = 0
                        I6! = 0
                    Else
                        I6! = Int((batRating(P, B3%(P, B1!(P)), 6) + batRating(P, B3%(P, B1!(P)), 7) + batRating(P, B3%(P, B1!(P)), 8) * 2 + batRating(P, B3%(P, B1!(P)), 9) * 3) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                        If I6! <= MG%(D, 757) And INNING% >= MG%(D, 758) And RN <= MG%(D, 759) Then D2 = 8: GoTo 18850
                        If I6! <= MG%(D, 760) And INNING% >= MG%(D, 761) And RN <= MG%(D, 762) Then D2 = 8: GoTo 18850
                        If I6! <= MG%(D, 763) And INNING% >= MG%(D, 764) And RN <= MG%(D, 765) Then D2 = 8: GoTo 18850
                    End If

                End If

            End If

        End If

        A1 = S6%(D, 0) - S6%(P, 0)

        If A1 >= -1 And A1 <= 1 And INNING% >= 8 And (S4% = 3 Or S4% >= 5) And nbrOuts(0) < 2 Then
            INFPOS% = 2
            D2 = 3

        Else

            If A1 > 0 Or nbrOuts(0) > 0 Or S4% <= 2 Or S4% = 4 Or S4% = 5 And nbrOuts(0) = 1 Or INNING% <= 5 And A1 < -2 Or INNING% > 5 And A1 < -4 Then

                If Not (S4% = 0 Or S4% = 2 Or S4% = 3 Or S4% >= 5 Or nbrOuts(0) = 2 Or B1!(P) >= 3 And B1!(P) <= 6 Or INNING% <= 6 And A1 >= 3 Or INNING% <= 6 And A1 <= -3) Then
                    If Not (INNING% > 7 And A1 >= 1 Or INNING% > 7 And A1 < -1) Then
                        INFPOS% = 1
                        D2 = 4
                    End If
                End If
            Else
                INFPOS% = 2
                D2 = 3
            End If

        End If

        GoTo 15055

    End If

    If Not (A1 <= 0 Or A1 = 0 And S4% <= 1 Or A1 = 0 And S4% = 4 Or S4% = 7 Or B7%(P, B1!(P)) = 1) Then

        If INNING% >= 9 And P = 1 And nbrOuts(0) < 2 And A1 = 0 And S4% >= 2 And S4% <= 3 Or INNING% >= 9 And P = 1 And nbrOuts(0) = 2 And A1 = 0 And S4% >= 5 And S4% <= 6 Then
            '18835
            D2 = 2
            GoTo 15055
        End If

        Ib4 = B4%(P, 0, B1!(P))
        I1 = B4%(P, 3, B1!(P))
        If B1!(P) = 9 Then I2 = B4%(P, 0, 0): I3 = B4%(P, 3, 0)
        If B1!(P) < 9 Then I2 = B4%(P, 0, B1!(P) + 1): I3 = B4%(P, 3, B1!(P) + 1)

        If Not (S4% <> 2 And S4% <> 6) Then

            If Ib4 - I2 >= 50 And I1 - I3 >= 50 And A1 < 1 And INNING% > 8 Or Ib4 >= 280 And I1 >= 140 And INNING% > 8 And I1 > I3 And A1 < 1 Then
                D2 = 2
                GoTo 15055
            End If

        End If

    End If

    18850 '
    If (S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2)) Or B9%(D, 1) = 99 Then
        Call RELIEVERS(PPH, pz)
        If useVGA = 1 Then _PutImage (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128), backupScreen&, imageScreen&, (20 * FontColAdj, 48)-Step(44 * FontColAdj, 128)
    End If

    GoTo 15055


    '----- NOODLE -----


    18909 'GET A PINCH HITTER FOR A POSITION PLAYER
    If MG%(P, 540) = 999 Then

        'SELECT PH FOR HITTER
        For I = 0 To 22

            If batRating(P, I, 21) <= 0 And batters$(P, I) <> "XXX" Then

                If B7%(P, B1!(P)) = batRating(P, I, 22) Or B7%(P, B1!(P)) = batRating(P, I, 23) Or B7%(P, B1!(P)) = batRating(P, I, 24) Or B7%(P, B1!(P)) = batRating(P, I, 25) Then

                    I1 = 6
                    If pitchRating(D, P1%(D), 0) < 0 Then I1 = 1

                    For I = I1 To I1 + 4

                        If MG%(P, I1 + 540) <> -1 And batRating(P, MG%(P, I1 + 540), 21) = 0 Then
                            mg% = MG%(P, I1 + 540)
                            MG%(P, I1 + 540) = -1

                            PPH = 1
                            gameD0%(P) = gameD0%(P) + 1
                            X0%(P, 0, gameD0%(P)) = mg%
                            X0%(P, 1, gameD0%(P)) = B7%(P, B1!(P))
                            X0%(P, 2, gameD0%(P)) = (B1!(P) * 10) + (gameD0%(P) - 9)
                            batRating(P, I, 21) = 1
                            B3%(P, B1!(P)) = mg%
                            B9%(P, B7%(P, B1!(P))) = mg%

                            If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then
                                B9%(P, 1) = 99
                                B9%(P, 0) = B1!(P)
                                B7%(P, B1!(P)) = 10
                            End If

                            I7% = P
                            I8% = B1!(P)
                            I9% = B3%(P, B1!(P))
                            Call REPLACEMENTS(I7%, I8%, I9%)

                            GoTo 18938

                        End If

                    Next

                End If

            End If

        Next

    End If

    For I = 0 To 22:

        If Not (batRating(P, I, 21) > 0 Or batters$(P, I) = "XXX" Or batRating(P, I, 4) = 0) Then

            If Not (B7%(P, B1!(P)) <> batRating(P, I, 22) And B7%(P, B1!(P)) <> batRating(P, I, 23) And B7%(P, B1!(P)) <> batRating(P, I, 24) And B7%(P, B1!(P)) <> batRating(P, I, 25)) Then

                '-- The next 3 IF statements execute identical code

                If pitchRating(D, P1%(D), 0) = 1 And batRating(P, I, 0) > 0 And batRating(P, I, 6) / batRating(P, I, 4) * 1000 > batRating(P, JM, 6) / batRating(P, JM, 4) * 1000 + 35 Then

                    PPH = 1
                    gameD0%(P) = gameD0%(P) + 1
                    X0%(P, 0, gameD0%(P)) = I
                    X0%(P, 1, gameD0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, gameD0%(P)) = (B1!(P) * 10) + (gameD0%(P) - 9)
                    batRating(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)

                    GoTo 18938

                End If

                If pitchRating(D, P1%(D), 0) = -1 And batRating(P, I, 0) < 0 And batRating(P, I, 6) / batRating(P, I, 4) * 1000 > batRating(P, JM, 6) / batRating(P, JM, 4) * 1000 + 45 Then

                    PPH = 1
                    gameD0%(P) = gameD0%(P) + 1
                    X0%(P, 0, gameD0%(P)) = I
                    X0%(P, 1, gameD0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, gameD0%(P)) = (B1!(P) * 10) + (gameD0%(P) - 9)
                    batRating(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)

                    GoTo 18938

                End If

                If batRating(P, I, 0) = 0 And batRating(P, I, 6) / batRating(P, I, 4) * 1000 > batRating(P, JM, 6) / batRating(P, JM, 4) * 1000 + 30 Then

                    PPH = 1
                    gameD0%(P) = gameD0%(P) + 1
                    X0%(P, 0, gameD0%(P)) = I
                    X0%(P, 1, gameD0%(P)) = B7%(P, B1!(P))
                    X0%(P, 2, gameD0%(P)) = (B1!(P) * 10) + (gameD0%(P) - 9)
                    batRating(P, I, 21) = 1
                    B3%(P, B1!(P)) = I
                    B9%(P, B7%(P, B1!(P))) = I

                    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

                    I7% = P
                    I8% = B1!(P)
                    I9% = B3%(P, B1!(P))
                    Call REPLACEMENTS(I7%, I8%, I9%)

                    GoTo 18938

                End If

                '-- Done with identical If/Then code

            End If

        End If

    Next I

    GoTo 18941

    '----- NOODLE -----

    18930 'GET A PINCH HITTER FOR THE PITCHER

    callPinch = 0
    skipLoop = 0

    If MG%(P, 540) <> 999 Then

        J% = 0
        GP% = 0

        If (pitchEraOpt > 1 And INNING% < 4) Or (pitchEraOpt > 2 And INNING% < 7) Or (pitchEraOpt > 3 And INNING% < 9) Or (pitchEraOpt >= 8) Or (pitchEraOpt > 0 And INNING% > 7 And S6%(P, 0) > S6%(D, 0) And pitchRating(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(pitchRating(P, P1%(P), 16) / pitchRating(P, P1%(P), 1) * 100 + .5) <= 100 And RN0 <= Int(pitchRating(P, P1%(P), 16) / pitchRating(P, P1%(P), 1) * 100 + .5)) Or (pitchEraOpt > 0 And INNING% > 7 And (S6%(P, 0) >= S6%(D, 0) Or S6%(D, 0) - S6%(P, 0) <= 2) And pitchRating(P, P1%(P), 1) > 0 And P6%(P) = 1 And Int(pitchRating(P, P1%(P), 16) / pitchRating(P, P1%(P), 1) * 100 + .5) >= 100) Then skipLoop = 1

        If skipLoop = 0 Then

            For Idx = 0 To 22

                If Not (batters$(P, Idx) = "XXX" Or batRating(P, Idx, 21) > 0 Or batRating(P, Idx, 4) = 0) Then

                    If batRating(P, Idx, 0) > 0 And pitchRating(D, P1%(D), 0) = 1 Or batRating(P, Idx, 0) < 0 And pitchRating(D, P1%(D), 0) = -1 Then
                        PH%(J%, 0) = Int(batRating(P, Idx, 3) / 4 + .5)
                    Else
                        PH%(J%, 0) = batRating(P, Idx, 3)
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
            gameD0%(P) = gameD0%(P) + 1
            X0%(P, 0, gameD0%(P)) = PH%
            X0%(P, 1, gameD0%(P)) = 10
            X0%(P, 2, gameD0%(P)) = 81 + gameD0%(P)
            batRating(P, PH%, 21) = 1
            B3%(P, B1!(P)) = PH%

            If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10

            18938 '
            P9 = P
            W5 = P9
            W6 = P9
            VV(P9) = B1!(P)
            VV1(P9) = B1!(P)
            Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

            If useVGA = 1 Then
                Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
            Else
                Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO)
            End If

            VV(P9) = 1
            VV1(P9) = 9

            player$(0) = batters$(P, B3%(P, B1!(P)))

            If B7%(P, B1!(P)) = 1 Then player$(0) = pitchers$(P, P1%(D))
            Call WINDEX
            Call PBP(gameManagers$(P) + " is sending up a pinch hitter")
            PPH = 1

            GoTo 15052

        End If

        18941 '
        If PR% <> 1 Then Call PINCHRUNNER(MS%, A1, S4%, I2, P, PR%, nbrBalls, nbrStrikes, D, MO)
        If MS% = 1 Then MS% = 0: GoTo 15052

        If B7%(P, B1!(P)) = 1 Then
            BR = 0
        Else
            BR = batRating(P, B3%(P, B1!(P)), 30)
        End If


        Call d100(RN)
        If S4% = 0 And (INFPOS% > 0 And RN <= 5 Or INFPOS% = 0 And RN <= 5) And BR > 8 Then P2 = 4: GoTo 14100

        ' MANAGER PROFILE TO HIT AND RUN
        Call d100(RN)
        If Not (S4% = 0 Or MG%(P, 671) <> 999 Or B7%(P, B1!(P)) = 1) Then
            If batRating(P, B3%(P, B1!(P)), 4) = 0 Then I7! = 0 Else I7! = Int((batRating(P, B3%(P, B1!(P)), 6) + batRating(P, B3%(P, B1!(P)), 7) + batRating(P, B3%(P, B1!(P)), 8) * 2 + batRating(P, B3%(P, B1!(P)), 9) * 3) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
            If batRating(P, B3%(P, B1!(P)), 12) = 0 Then I6! = 0 Else I6! = Int(batRating(P, B3%(P, B1!(P)), 4) / batRating(P, B3%(P, B1!(P)), 12) + .5)

            If S4% = 1 And nbrOuts(0) = 0 And A1 <= MG%(P, 672) And A1 >= MG%(P, 673) * -1 And RN <= MG%(P, 674) And I7! <= MG%(P, 872) And I6! >= MG%(P, 866) Then P2 = 2: GoTo 15067
            If S4% = 1 And nbrOuts(0) = 1 And A1 <= MG%(P, 675) And A1 >= MG%(P, 676) * -1 And RN <= MG%(P, 677) And I7! <= MG%(P, 873) And I6! >= MG%(P, 867) Then P2 = 2: GoTo 15067

            If S4% = 5 And nbrOuts(0) = 0 And A1 <= MG%(P, 678) And A1 >= MG%(P, 679) * -1 And RN <= MG%(P, 680) And I7! <= MG%(P, 874) And I6! >= MG%(P, 868) Then P2 = 2: GoTo 15067
            If S4% = 5 And nbrOuts(0) = 1 And A1 <= MG%(P, 681) And A1 >= MG%(P, 682) * -1 And RN <= MG%(P, 683) And I7! <= MG%(P, 875) And I6! >= MG%(P, 869) Then P2 = 2: GoTo 15067

            If S4% = 4 And nbrOuts(0) = 0 And A1 <= MG%(P, 684) And A1 >= MG%(P, 685) * -1 And RN <= MG%(P, 686) And I7! <= MG%(P, 876) And I6! >= MG%(P, 870) Then P2 = 2: GoTo 15067
            If S4% = 4 And nbrOuts(0) = 1 And A1 <= MG%(P, 687) And A1 >= MG%(P, 688) * -1 And RN <= MG%(P, 689) And I7! <= MG%(P, 877) And I6! >= MG%(P, 871) Then P2 = 2: GoTo 15067
        End If

        If Not (MG%(P, 609) <> 999 Or B7%(P, B1!(P)) <> 1) Then

            If Not (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

                Call d100(RN)

                If S4% = 1 And INNING% <= 7 And MG%(P, 610) >= nbrOuts(0) And A1 <= MG%(P, 611) And A1 >= MG%(P, 612) * -1 And RN <= MG%(P, 613) Then 18970
                If S4% = 1 And INNING% > 7 And MG%(P, 614) >= nbrOuts(0) And A1 <= MG%(P, 615) And A1 >= MG%(P, 616) * -1 And RN <= MG%(P, 617) Then 18970

                If S4% = 2 And INNING% <= 7 And MG%(P, 618) >= nbrOuts(0) And A1 <= MG%(P, 619) And A1 >= MG%(P, 620) * -1 And RN <= MG%(P, 621) Then 18970
                If S4% = 2 And INNING% > 7 And MG%(P, 622) >= nbrOuts(0) And A1 <= MG%(P, 623) And A1 >= MG%(P, 624) * -1 And RN <= MG%(P, 625) Then 18970

                If S4% = 4 And INNING% <= 7 And MG%(P, 626) >= nbrOuts(0) And A1 <= MG%(P, 627) And A1 >= MG%(P, 628) * -1 And RN <= MG%(P, 629) Then 18970
                If S4% = 4 And INNING% > 7 And MG%(P, 630) >= nbrOuts(0) And A1 <= MG%(P, 631) And A1 >= MG%(P, 632) * -1 And RN <= MG%(P, 633) Then 18970

            End If

        End If

        If MG%(P, 634) = 999 And B7%(P, B1!(P)) <> 1 And nbrOuts(0) = 0 Or (S4% <> 1 And S4% <> 2 And S4% <> 4) Then

            If batRating(P, B3%(P, B1!(P)), 4) = 0 Then
                I7! = 0
                I6! = 0
            Else
                I7! = Int(batRating(P, B3%(P, B1!(P)), 6) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
                I6! = Int((batRating(P, B3%(P, B1!(P)), 6) + batRating(P, B3%(P, B1!(P)), 7) + batRating(P, B3%(P, B1!(P)), 8) * 2 + batRating(P, B3%(P, B1!(P)), 9) * 3) / batRating(P, B3%(P, B1!(P)), 4) * 1000 + .5)
            End If

            Call d100(RN)
            If S4% = 1 And INNING% <= 7 And A1 <= MG%(P, 635) And A1 >= MG%(P, 636) * -1 And I7! <= MG%(P, 637) And I6! <= MG%(P, 638) And pitchRating(D, P1%(D), 10) <= MG%(P, 639) And RN <= MG%(P, 640) Then 18970
            If S4% = 2 And INNING% <= 7 And A1 <= MG%(P, 641) And A1 >= MG%(P, 642) * -1 And I7! <= MG%(P, 643) And I6! <= MG%(P, 644) And pitchRating(D, P1%(D), 10) <= MG%(P, 645) And RN <= MG%(P, 646) Then 18970
            If S4% = 4 And INNING% <= 7 And A1 <= MG%(P, 647) And A1 >= MG%(P, 648) * -1 And I7! <= MG%(P, 649) And I6! <= MG%(P, 650) And pitchRating(D, P1%(D), 10) <= MG%(P, 651) And RN <= MG%(P, 652) Then 18970

            If S4% = 1 And INNING% > 7 And A1 <= MG%(P, 653) And A1 >= MG%(P, 654) * -1 And I7! <= MG%(P, 655) And I6! <= MG%(P, 656) And pitchRating(D, P1%(D), 10) <= MG%(P, 657) And RN <= MG%(P, 658) Then 18970
            If S4% = 2 And INNING% > 7 And A1 <= MG%(P, 659) And A1 >= MG%(P, 660) * -1 And I7! <= MG%(P, 661) And I6! <= MG%(P, 662) And pitchRating(D, P1%(D), 10) <= MG%(P, 663) And RN <= MG%(P, 664) Then 18970
            If S4% = 4 And INNING% > 7 And A1 <= MG%(P, 665) And A1 >= MG%(P, 666) * -1 And I7! <= MG%(P, 667) And I6! <= MG%(P, 668) And pitchRating(D, P1%(D), 10) <= MG%(P, 669) And RN <= MG%(P, 670) Then 18970

        End If

        P2 = 1

        If S4% = 1 Or S4% = 5 Then

            If A1 <= -1 And batRating(P, B3%(P, gameB0%(1, 0)), 13) < 10 Or gameB0%(1, 2) > 1 Then 18943: 'CHANGED FROM <10
            If A1 >= -1 And S4% = 5 Then 18943
            If A1 < -5 Or B7%(P, gameB0%(1, 0)) = 1 And S4% = 1 Or B7%(P, gameB0%(2, 0)) = 1 And S4% = 2 Then 18943
            If A1 < 0 And INNING% >= 7 Then 18943
            P2 = 3

            GoTo 14800

        End If

        If S4% = 2 Or S4% = 4 Then
            If Not (batRating(P, B3%(P, gameB0%(2, 0)), 13) < 40 Or B7%(P, B1!(P)) = 1 Or nbrOuts(0) = 2) Then
                If gameB0%(2, 2) <= 1 Then
                    P2 = 3
                    GoTo 14800
                End If
            End If
        End If

        18943 '
        If Not (S4% = 0 Or nbrOuts(0) > 0 Or S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 5 Or nbrStrikes = 2 Or P = 0 And INNING% > 7) Then

            If B7%(P, B1!(P)) = 1 And S4% = 1 And nbrOuts(0) < 2 Or B7%(P, B1!(P)) = 1 And S4% = 2 And nbrOuts(0) = 0 Or B7%(P, B1!(P)) = 1 And S4% = 4 And nbrOuts(0) = 0 Then 18969
            If B1!(P) > 2 And B1!(P) < 6 Or B4%(P, 0, B1!(P)) > 290 And B7%(P, B1!(P)) <> 1 Or B7%(P, B1!(P)) <> 1 And B4%(P, 3, B1!(P)) > 70 Then 18968
            If INNING% > 7 And A1 = -1 And nbrOuts(0) = 0 And S4% = 1 Then 18969

        End If

    End If

    18968 '
    PPH = 0
    PR% = 0

    Call d100(RN0)

    If nbrOuts(0) = 2 Or B4%(P, 4, B1!(P)) > 100 Or S4% = 0 Or S4% = 7 Or S4% = 6 Or S4% = 3 Or S4% = 2 Or A1 < -3 Or RN0 > 20 Then
        GoTo 15064
    Else
        P2 = 2
        GoTo 15067
    End If

    '----- NOODLE -----

    18969 '

    If nbrOuts(0) >= 1 And INNING% >= 7 Then
        GoTo 18968
    Else
        18970 '
        P2 = 4
        GoTo 14100
    End If


End Sub



'INCLUDE: 'GameRoutines.bm'


'----------------------------------------
'            INFO Subroutine
'----------------------------------------
'This subroutine displays a simple header
'and footer for the game options menu.
Sub INFO ()

    Cls

    Color , 1
    Locate 1, 1
    Print Tab(80);

    Locate , 33
    Color 14
    Print "GAME OPTIONS MENU"

    Locate 24, 1
    Print Tab(80);
    Locate , 1
    Print "    LANCE HAFFNER 1986-2000"; Tab(70); Left$(Time$, 5);
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

    Shared DB, HR, TR, SB

    Shared E!

    Shared actualAttendance&, avgAttendance&

    Shared scheduleFile$

    Shared BB%(), BP(), F()
    Shared gameT1%(), MF%()
    Shared nbrInnings(), oppScore()
    Shared startPitcher(), statT0%(), SU()
    Shared teamScore()

    Shared dayNight$(), desigHitOpt$(), gameType$()
    Shared leagueType$(), locIndicator$()
    Shared losePitcher$(), winPitcher$()
    Shared month$(), oppName$(), pitcherEra$()
    Shared playMode$(), PT$(), umpireName$()
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

    PT$(0) = "RIGHT"
    PT$(1) = "LEFT "

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

    B1$(0) = "LP"
    B1$(1) = "L"
    B1$(2) = "S"
    B1$(3) = "R"
    B1$(4) = "RP"

    baseName$(1) = "1b"
    baseName$(2) = "2b"
    baseName$(3) = "3b"
    baseName$(4) = "home"

    H0$(0) = " "
    H0$(1) = "H"

    P = 1

    'For I = 0 To 1: For I1 = 0 To 4: NB%(I, I1) = -1: Next: Next
    For I = 0 To 1:
        P1%(I) = -1: P6%(I) = -1
    Next

    startPitcher(0) = -1
    startPitcher(1) = -1

    For I = 0 To 1
        For J = 1 To 9
            B7%(I, J) = -1
            B3%(I, J) = -1
        Next
    Next

    autoPlay = 0
    compTeam = 0
    D = 0
    DB = 0
    ejectCnt = 0
    endAllGames = 0
    endGame = 1
    gameTemp = 0
    gameW% = 0
    H6% = 0
    HR = 0
    IJL% = 0
    INNING% = 1
    Inotloop% = 0
    LYN% = 0
    noLUFound = 0
    P = 0
    PB% = 0
    precipType = 0
    rainyDay = 0
    rainStartInning = 0
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

    Erase BP, F, MON, pitchRating
    Erase tmRat_GAME, VV, VV1

    Erase A5%, batRating, gameB0%
    Erase B1%, B2%, B3%, B4%
    Erase B5%, B7%, B8%, B9%, BT%
    Erase BB%, CSS%, gameD0%, D3%, DP%
    Erase gameSA%, gameT1%, GK%, H0%, INJ%, inningScore
    Erase L0%, gameM%, MF%, MG%
    Erase NG%, nbrInnings, nbrOuts
    Erase oppScore
    Erase P1%, P2%, P3%, P4%, P5%, P6%, P8%
    Erase S1%, S6%, S8%
    Erase SU%, startPitcher, statT0%
    Erase T3%, teamScore, TS%, X0%

    Erase B1!, parkHRVals!

    Erase batters$, DB$, diskIDs$, eventDesc$, ejections$
    Erase G$, gameManagers$, gameStadiums$, gameTeams$
    Erase homeRuns$, IJ$
    Erase locIndicator$, oppName$
    Erase losePitcher$, winPitcher$
    Erase PARK$, pitchers$, player$
    Erase Q3$, SB$
    Erase teamAbbreviatons$, TR$, umpireName$

    For P9 = 0 To 1
        For I3 = 0 To 21
            If pitchRating(P9, I3, 15) <> 99 Then pitchRating(P9, I3, 15) = 0
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
            If batRating(I, I1, 21) <> 99 Then batRating(I, I1, 21) = 0
        Next
    Next

End Sub


'----------------------------------------
'          AUTOSETUP Subroutine
'----------------------------------------
'This subroutine auto-assigns game param-
'eters from a schedule file's data.
Sub AUTOSETUP

    'Setup variables for auto-play
    playerMode = NG%(8)
    desigHit = NG%(3)
    pbpDelay! = NG%(4) / 10
    gameMonth = NG%(17)
    useWeather = NG%(5)
    useVGA = NG%(11)
    useInj = NG%(6)
    useLRSplit = NG%(7)
    useRest = NG%(9)
    useMgrProf = NG%(10)
    pitchEraOpt = NG%(14)

    If NG%(15) < 1600 Then
        dayOrNight = 0
    Else
        dayOrNight = 1
    End If


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
    Color tmRat_GAME(teamIdx, 11), tmRat_GAME(teamIdx, 12)

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

    Locate 8, 8
    Color tmRat_GAME(teamIdx, 11), tmRat_GAME(teamIdx, 12)

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
'            d100 Subroutine
'----------------------------------------
'This subroutine assigns a random number
'between 1 and 100 to the variable that
'it is called with
Sub d100 (randNumVar)

    randNumVar = Int(Rnd(1) * 100) + 1

End Sub



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

        badString% = 0

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
                badString% = 1
            Else
                stringPosition% = stringPosition% + 1
            End If

        Loop Until stringPosition% = Len(fileString$) Or badString% = 1

    Loop Until stringPosition% = Len(fileString$) Or badString% = 0

    If TGTBASE% = 3 Then
        If Left$(fileString$, 2) = "Q1" Then fileString$ = "Q2 " + Right$(fileString$, Len(fileString$) - 3)
        If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + "Q2"
    End If

    If Left$(fileString$, 2) = "BB" Then fileString$ = player$(0) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q1" Then fileString$ = player$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q2" Then fileString$ = player$(2) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "PP" Then fileString$ = pitchers$(D, P1%(D)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "UU" Then fileString$ = gameUmpire$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "XX" Then fileString$ = batters$(D, B9%(D, 5)) + " and " + batters$(D, B9%(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "TT" Then fileString$ = batters$(D, B9%(D, 5)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "FB" Then fileString$ = batters$(D, B9%(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "YY" Then fileString$ = batters$(D, B9%(D, 4)) + " and " + batters$(D, B9%(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "ZZ" Then fileString$ = batters$(D, B9%(D, 4)) + " and " + batters$(D, B9%(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q4" Then fileString$ = player$(4) + Right$(fileString$, Len(fileString$) - 2)

    If Right$(fileString$, 3) = " PP" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + pitchers$(D, P1%(D))
    If Right$(fileString$, 3) = " BB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(0)
    If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(1)
    If Right$(fileString$, 2) = "Q2" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(2)
    If Right$(fileString$, 2) = "UU" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + gameUmpire$(1)
    If Right$(fileString$, 2) = "Q4" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(4)
    If Right$(fileString$, 3) = " TT" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 5))
    If Right$(fileString$, 2) = "FB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 3))
    If Right$(fileString$, 3) = " XX" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 5)) + " and " + batters$(D, B9%(D, 6))
    If Right$(fileString$, 2) = "YY" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 3)) + " and " + batters$(D, B9%(D, 4))
    If Right$(fileString$, 3) = " ZZ" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 6)) + " and " + batters$(D, B9%(D, 4))

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
    If Left$(fileString$, 2) = "PP" Then fileString$ = pitchers$(D, P1%(D)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "UU" Then fileString$ = gameUmpire$(1) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "XX" Then fileString$ = batters$(D, B9%(D, 5)) + " and " + batters$(D, B9%(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "TT" Then fileString$ = batters$(D, B9%(D, 5)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "FB" Then fileString$ = batters$(D, B9%(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "YY" Then fileString$ = batters$(D, B9%(D, 4)) + " and " + batters$(D, B9%(D, 3)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "ZZ" Then fileString$ = batters$(D, B9%(D, 4)) + " and " + batters$(D, B9%(D, 6)) + Right$(fileString$, Len(fileString$) - 2)
    If Left$(fileString$, 2) = "Q4" Then fileString$ = player$(4) + Right$(fileString$, Len(fileString$) - 2)
    If Right$(fileString$, 3) = " PP" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + pitchers$(D, P1%(D))
    If Right$(fileString$, 3) = " BB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(0)
    If Right$(fileString$, 2) = "Q1" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(1)
    If Right$(fileString$, 2) = "Q2" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(2)
    If Right$(fileString$, 2) = "UU" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + gameUmpire$(1)
    If Right$(fileString$, 2) = "Q4" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + player$(4)
    If Right$(fileString$, 3) = " TT" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 5))
    If Right$(fileString$, 2) = "FB" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 3))
    If Right$(fileString$, 3) = " XX" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 5)) + " and " + batters$(D, B9%(D, 6))
    If Right$(fileString$, 2) = "YY" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 3)) + " and " + batters$(D, B9%(D, 4))
    If Right$(fileString$, 3) = " ZZ" Then fileString$ = Left$(fileString$, Len(fileString$) - 2) + batters$(D, B9%(D, 6)) + " and " + batters$(D, B9%(D, 4))

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

    If UCase$(M$) = "Q" Then
        Cls
        End
    End If

    If pbpDelay! > 0 Then

        'Clean up / restrict to 44 char's
        'this is the width of the PBP window
        pbpString$ = PadRight(pbpString$, 44)
        pbpString$ = Left$(pbpString$, 44)

        PB% = PB% + 1

        ' Put PBP text in a slot for line PB%
        If PB% < 9 Then G$(PB%) = pbpString$

        ' Or we're past line 8, move all slots down
        ' to make room
        If PB% > 8 Then

            For I9 = 1 To 7
                G$(I9) = G$(I9 + 1)
            Next

            G$(8) = pbpString$

        End If

        'Print all 8 lines from scratch
        For I9 = 1 To 8

            If useVGA = 1 Then
                Color 4294967295
                _PrintMode _FillBackground , imageScreen&
                PrintOnGfx 20, I9 + 3, G$(I9), imageScreen&
            Else
                Locate 14 + I9, 1
                Print G$(I9)
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

    P8%(D, I5, I6) = P8%(D, I5, I6) + 1
    P5%(D, I5, I6) = P5%(D, I5, I6) + 1

End Sub

'----------------------------------------
'           ADDSTTS Subroutine
'----------------------------------------
' ...explanation...
Sub ADDSTTS (P, D)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 0
    Call ADDPIT(D, I5, I6)

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
        If S2% = 13 And A5%(idx) = 11 And B1%(idx, 0) = 0 Then A5%(0) = 11
    Next idx

    If Not (B1%(3, 0) = 0 Or A5%(3) = 0) Then
        idx = 3
        If A5%(3) >= 1 And A5%(3) <= 4 Then Call CHARGERUN(D, idx)
        If A5%(3) = 10 Then I3 = 3: Call CLEARBASE(idx)
        If A5%(3) = 11 Then I3 = 4: Call CLEARBASE(idx)
    End If

    If Not (B1%(2, 0) = 0 Or A5%(2) = 0) Then
        idx = 2
        Select Case A5%(2)
            Case 1
                If B1%(3, 0) = 0 Then Call BATTER1(idx)
            Case 2 To 4
                If B1%(3, 0) = 0 Then Call CHARGERUN(D, idx)
            Case 10
                I3 = 2
                Call CLEARBASE(idx)
            Case 11
                I3 = 3
                Call CLEARBASE(idx)
            Case 12
                If B1%(3, 0) = 0 Then I3 = 4: Call CLEARBASE(idx)
        End Select

    End If

    If Not (B1%(1, 0) = 0 Or A5%(1) = 0) Then
        idx = 1
        Select Case A5%(1)
            Case 1
                If B1%(2, 0) = 0 Then Call BATTER1(idx)
            Case 2
                If B1%(2, 0) = 0 And B1%(3, 0) = 0 Then Call BATTER2(idx)
            Case 3 To 4
                If B1%(2, 0) = 0 And B1%(3, 0) = 0 Then Call CHARGERUN(D, idx)
            Case 8
                If B1%(2, 0) = 0 Then Call BATTER1(idx)
            Case 10
                I3 = 1
                Call CLEARBASE(idx)
            Case 11
                I3 = 2
                Call CLEARBASE(idx)
            Case 12
                If B1%(3, 0) = 0 And B1%(2, 0) = 0 Then I3 = 3: Call CLEARBASE(idx)
            Case 13
                If B1%(3, 0) = 0 And B1%(2, 0) = 0 Then I3 = 4: Call CLEARBASE(idx)
        End Select

    End If

    idx = 0
    B1%(0, 0) = B1!(P)
    B1%(0, 1) = P1%(D)

    If A5%(0) <> 0 Then

        Select Case A5%(0)
            Case 1 To 3:
                I3 = A5%(0): Call BATTER3(I3, P, D)
            Case 4
                I3 = A5%(0): Call CHARGERUN(D, idx)
            Case 11 To 14
                I3 = 10 - A5%(0): Call CLEARBASE(idx)
        End Select

    End If

End Sub


'----------------------------------------
'           ADV1BASE Subroutine
'----------------------------------------
' ...explanation...
Sub ADV1BASE (i)

    For I4 = 0 To 2
        gameB0%(i + 1, I4) = gameB0%(i, I4)
        gameB0%(i + 1, 2) = 0
        gameB0%(i, I4) = 0
    Next I4

    player$(i + 1) = player$(i)
    player$(i) = ""

End Sub

'----------------------------------------
'          ADV2BASES Subroutine
'----------------------------------------
' ...explanation...
Sub ADV2BASES (i)
    For I4 = 0 To 2
        gameB0%(i + 2, I4) = gameB0%(i, I4)
        gameB0%(i + 1, 2) = 0
        gameB0%(i, I4) = 0
    Next I4

    player$(i + 2) = player$(i)
    player$(i) = ""

End Sub

'----------------------------------------
'          ADV3BASES Subroutine
'----------------------------------------
' ...explanation...
Sub ADV3BASES (I3, P, D)

    gameB0%(I3, 0) = B1!(P)
    gameB0%(I3, 1) = P1%(D)
    gameB0%(I3, 2) = 0
    gameB0%(0, 0) = 0

    player$(I3) = player$(0)

End Sub

'----------------------------------------
'         ADVANCEMENT Subroutine
'----------------------------------------
' ...explanation...
Sub ADVANCEMENT (P, D, S2%, F%, I3)

    If gameB0%(3, 0) <> 0 And A5%(3) <> 0 Then
        i = 3
        If A5%(3) >= 1 And A5%(3) <= 4 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, gameW%, INFPOS%)
        If A5%(3) = 10 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(3) = 11 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If gameB0%(2, 0) <> 0 And A5%(2) <> 0 Then
        i = 2
        If A5%(2) = 1 And gameB0%(3, 0) = 0 Then Call ADV1BASE(i)
        If A5%(2) >= 2 And A5%(2) <= 4 And gameB0%(3, 0) = 0 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, gameW%, INFPOS%)

        'nothing happens for values 5-9? or are they not possible?

        If A5%(2) = 10 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(2) = 11 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(2) = 12 And gameB0%(3, 0) = 0 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If gameB0%(1, 0) <> 0 And A5%(1) <> 0 Then
        i = 1
        If A5%(1) = 1 And gameB0%(2, 0) = 0 Then Call ADV1BASE(i)
        If A5%(1) = 2 And gameB0%(2, 0) = 0 And gameB0%(3, 0) = 0 Then Call ADV2BASES(i)
        If A5%(1) >= 3 And A5%(1) <= 4 And gameB0%(2, 0) = 0 And gameB0%(3, 0) = 0 Then Call RecordRun(sah%, i, D, F%, PQ, S2%, gameW%, INFPOS%)
        If A5%(1) = 8 And gameB0%(2, 0) = 0 Then Call ADV1BASE(i)
        If A5%(1) = 10 Then I3 = 1: Call RecordOut(i, I3)
        If A5%(1) = 11 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(1) = 12 And gameB0%(3, 0) = 0 And gameB0%(2, 0) = 0 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(1) = 13 And gameB0%(2, 0) = 0 And gameB0%(3, 0) = 0 Then I3 = 4: Call RecordOut(i, I3)
    End If

    i = 0
    gameB0%(0, 0) = B1!(P)
    gameB0%(0, 1) = P1%(D)

    If A5%(0) <> 0 Then
        'values 1 to 3
        If A5%(0) > 0 And A5%(0) < 4 Then I3 = A5%(0): Call ADV3BASES(I3, P, D)
        If A5%(0) = 4 Then I3 = A5%(0): Call RecordRun(sah%, i, D, F%, PQ, S2%, gameW%, INFPOS%)
        If A5%(0) = 11 Then I3 = 1: Call RecordOut(i, I3)
        If A5%(0) = 12 Then I3 = 2: Call RecordOut(i, I3)
        If A5%(0) = 13 Then I3 = 3: Call RecordOut(i, I3)
        If A5%(0) = 14 Then I3 = 4: Call RecordOut(i, I3)
    End If

    If F% = 0 And nbrOuts(1) < 3 Then Call ADJRUNNERS(S2%, P)

    Erase A5%

End Sub


'----------------------------------------
'    DisplayBatter_Lineup Subroutine
'----------------------------------------
' ...explanation...
Sub DisplayBatter_Lineup (PG, P9)

    Locate 1, 1

    Color 14: Print "#";
    Color tmRat_GAME(P9, 11), tmRat_GAME(P9, 12): Print gameTeams$(P9);

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

        If batRating(P9, idx, 21) = 0 Then

            Color 2, 0

            If idx < 10 Then Print Using "#"; idx;
            If idx > 9 Then Print alpha$(idx - 10);

            Color 15, 0

            Print Tab(3); batters$(P9, idx); Tab(17); B1$(batRating(P9, idx, 0) + 2);

            For xx = 20 To 23
                Print Tab(xx);: If batRating(P9, idx, xx + 2) <> -1 Then Print Using "#"; batRating(P9, idx, xx + 2);
            Next

            Print Tab(24);
            Print Using " ### ### ## ## ## ### ### ### #"; batRating(P9, idx, 3); batRating(P9, idx, 4); batRating(P9, idx, 7); batRating(P9, idx, 8); batRating(P9, idx, 9); batRating(P9, idx, 10); batRating(P9, idx, 11); batRating(P9, idx, 12); batRating(P9, idx, 16);
            Print Using "#"; batRating(P9, idx, 17);
            Print Using "#"; batRating(P9, idx, 15);
            Print Using "# "; batRating(P9, idx, 30);
            Print Using "### ## ## "; batRating(P9, idx, 13); batRating(P9, idx, 14); batRating(P9, idx, 18);
            Print Tab(69); ".";

            'BA
            If batRating(P9, idx, 4) <> 0 Then
                bAvg! = batRating(P9, idx, 6) / batRating(P9, idx, 4)
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

    B8%(P, I5, I6) = B8%(P, I5, I6) - 1
    B5%(P, I5, I6) = B5%(P, I5, I6) - 1

    If Not (I6 > 8 Or I6 = 2 Or I6 = 3) Then

        If pitchRating(1 - P, P1%(1 - P), 0) <> 1 Then

            If I6 >= 4 Then
                CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) - 1
            Else
                CSS%(P, I5, I6) = CSS%(P, I5, I6) - 1
            End If

        End If

    End If

End Sub


'----------------------------------------
'           BATTER1 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER1 (i)

    For I4 = 0 To 1
        B1%(i + 1, I4) = B1%(i, I4)
        B1%(i, I4) = 0
    Next I4

End Sub

'----------------------------------------
'           BATTER2 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER2 (i)

    For I4 = 0 To 1
        B1%(i + 2, I4) = B1%(i, I4)
        B1%(i, I4) = 0
    Next I4

End Sub

'----------------------------------------
'           BATTER3 Subroutine
'----------------------------------------
' ...explanation...
Sub BATTER3 (I3, P, D)

    B1%(I3, 0) = B1!(P)
    B1%(I3, 1) = P1%(D)

End Sub

'----------------------------------------
'          CAUGHTFLY Subroutine
'----------------------------------------
' ...explanation...
Sub CAUGHTFLY (i, S2%, D)

    Call d100(RN)

    Select Case RN

        Case 1 To 96
            i = 1
            If S2% <> 17 Then
                Call GetPBPString(60, 1230, fileString$, D)
                Call PBP(player$(4) + " " + fileString$)
            Else
                If tmRat_GAME(1, 13) = 37 Then Call PBP("he climbs the terrace")
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
        If B1!(P9) = luIdx Then Print "*";

        Locate , 2: Print Using "#"; luIdx;
        Locate , 4

        If B7%(P9, luIdx) = 1 Then Print pitchers$(P9, P1%(P9));
        If B7%(P9, luIdx) <> 1 Then Print batters$(P9, B3%(P9, luIdx));
        If B7%(P9, luIdx) <> 0 And B7%(P9, luIdx) < 10 Then Locate , 20: Print batRating(P9, B3%(P9, luIdx), 19);

        Locate , 17
        Print fieldPos$(B7%(P9, luIdx));
        Locate , 25

        For I1 = 1 To 3
            If gameB0%(I1, 0) = luIdx And P9 = P Then Color 3: Print I1;: Color 15
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
        I5 = B1%(i, 1)
        I6 = 3
        Call ADDPIT(D, I5, I6)
        Call CLEARBASE(i)
    End If

End Sub


'----------------------------------------
'          CHECKSAVE Subroutine
'----------------------------------------
' ...explanation...
Sub CHECKSAVE (D, P, S4%)

    I3 = S6%(D, 0) - S6%(P, 0)
    S1%(D) = -1

    If I3 >= 1 Then
        If S4% = 0 And I3 <= 2 Then S1%(D) = P1%(D)
        If S4% > 0 And S4% < 4 And I3 <= 3 Then S1%(D) = P1%(D)
        If S4% > 3 And S4% < 7 And I3 <= 4 Then S1%(D) = P1%(D)
        If S4% = 7 And I3 <= 5 Then S1%(D) = P1%(D)
        If S4% = 0 And nbrOuts(0) = 0 And I3 <= 3 Then S1%(D) = P1%(D)
        If INNING% > 5 And I3 > 0 And INNING% < 8 Then S1%(D) = P1%(D)
    End If

    RI = 0

    If S4% <> 0 Then
        If S4% = 1 Or S4% = 2 Or S4% = 3 Then RI = 1
        If S4% = 4 Or S4% = 5 Or S4% = 6 Then RI = 2
        If S4% = 7 Then RI = 3
        P5%(D, P1%(D), 17) = P5%(D, P1%(D), 17) + RI
        P8%(D, P1%(D), 17) = P8%(D, P1%(D), 17) + RI
    End If

End Sub

'----------------------------------------
'          CLEARBASE Subroutine
'----------------------------------------
' ...explanation...
Sub CLEARBASE (i)

    B1%(i, 0) = 0
    B1%(i, 1) = 0

End Sub

'----------------------------------------
'        CREDITASSIST Subroutine
'----------------------------------------
'Credit player with assist
Sub CREDITASSIST (gameW%, D)

    If gameW% = 1 Then
        I5 = P1%(D)
        I6 = 39
        Call ADDPIT(D, I5, I6)
    Else
        I5 = B9%(D, gameW%)
        I6 = 17
        Call INCBATDF(D, I5, I6)
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
        I5 = B9%(D, po%)
        I6 = 16
        Call INCBATDF(D, I5, I6)

    Else

        If po% = 3 And FB2P% = 1 Then

            FB2P% = 0
            po% = 1
            If po% = 1 Then
                I5 = P1%(D)
                I6 = 38
                Call ADDPIT(D, I5, I6)
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
        I5 = B9%(D, po%)
        I6 = 16
        Call INCBATDF(D, I5, I6)

    End If

End Sub

'----------------------------------------
'         CREDITREGPO Subroutine
'----------------------------------------
'Credit player with regular putout
Sub CREDITREGPO (gameW%, D)


    If gameW% = 1 Then
        I5 = P1%(D)
        I6 = 38
        Call ADDPIT(D, I5, I6)
    Else
        I5 = B9%(D, gameW%)
        I6 = 16
        Call INCBATDF(D, I5, I6)
    End If

End Sub


'----------------------------------------
'          DELIVERY Subroutine
'----------------------------------------
'This subroutine generates the PBP for
'a pitch delivery.
Sub DELIVERY (D)

    Call d100(RN)

    If RN <= 20 Then

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

    For luIdx = 2 To 13: Locate luIdx, 1: Print String$(78, 32): Next

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

        If batRating(P9, luIdx, 21) = 0 Then
            Color 2
            If luIdx < 10 Then Print Using "#"; luIdx;
            If luIdx > 9 Then Print alpha$(luIdx - 10);

            Color 15, 0
            Print Tab(3); batters$(P9, luIdx);

            Locate , 17: If batRating(P9, luIdx, 22) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, 22); batRating(P9, luIdx, 26); batRating(P9, luIdx, 33); batRating(P9, luIdx, 34);
            Locate , 31: If batRating(P9, luIdx, 23) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, 23); batRating(P9, luIdx, 27); batRating(P9, luIdx, 35); batRating(P9, luIdx, 36);
            Locate , 45: If batRating(P9, luIdx, 24) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, 24); batRating(P9, luIdx, 28); batRating(P9, luIdx, 37); batRating(P9, luIdx, 38);
            Locate , 59: If batRating(P9, luIdx, 25) <> -1 Then Print Using "# #### # #"; batRating(P9, luIdx, 25); batRating(P9, luIdx, 29); batRating(P9, luIdx, 39); batRating(P9, luIdx, 40);

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

    J = gameB0%(I2, 0)

    If J <> 0 Then

        Color tmRat_GAME(P, 11), tmRat_GAME(P, 12)

        If B7%(P, J) = 1 And pitchRating(P, P1%(P), 11) <> 999 Then

            Print Left$(pitchers$(P, P1%(P)), 10)
            Color 15, 2
            Locate , X: Print "1/  0"

        Else
            If B7%(P, J) = 1 Then
                Print Left$(pitchers$(P, P1%(P)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; pitchRating(P, P1%(P), 31);
                Print "/";: Print Using "###"; pitchRating(P, P1%(P), 29)
            Else
                Print Left$(batters$(P, B3%(P, J)), 10)
                Locate , X
                Color 15, 2: Print Using "#"; batRating(P, B3%(P, J), 16);
                Print "/";: Print Using "###"; batRating(P, B3%(P, J), 13)
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

    Call d100(RN)

    Select Case RN
        Case 1 To 19: pbpString$ = "may be a double play ball"
        Case 20 To 40: pbpString$ = "they might turn two"
        Case 41 To 60: pbpString$ = "they could get two"
        Case 61 To 80: pbpString$ = "could be two"
        Case 81 To 100: pbpString$ = "could be a double play"
    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'           DPPIVOT Subroutine
'----------------------------------------
'This subroutine handles variables and
'Play-by-Play for a double-play pivot
Sub DPPIVOT (gameW%, D)

    ' *** double play pivot ***
    If gameW% = 4 Then ww% = 6
    If gameW% = 6 Then ww% = 4
    If gameW% = 3 Then ww% = 1

    Call GetPBPString(20, 1910, fileString$, D)
    Call PBP(fileString$ + " " + batters$(D, B9%(D, ww%)))

End Sub


'----------------------------------------
'           DPRELAY Subroutine
'----------------------------------------
'Double-play relay ?!?!
Sub DPRELAY ()

    If A5%(0) = 11 Then
        Call d100(RN)

        If RN <= 10 Then
            Call PBP("Give me the Hoover!")
        Else

            Call d100(RN)

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

    Call d100(RN)

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
        pbpString$ = "You must replace " + batters$(D, B9%(D, po%))
        Call PBP(pbpString$)
    Else

        For I2 = 1 To 9
            If B7%(D, I2) = po% Then Exit For
        Next

        For idx = 0 To 22

            If batRating(D, idx, 21) <= 0 And batters$(D, idx) <> "XXX" Then

                If B7%(D, I2) = batRating(D, idx, 22) Or B7%(D, I2) = batRating(D, idx, 23) Or B7%(D, I2) = batRating(D, idx, 24) Or B7%(D, I2) = batRating(D, idx, 25) Then

                    pbpString$ = batters$(D, B9%(D, po%)) + " will be replaced by " + batters$(D, idx)
                    Call PBP(pbpString$)

                    gameD0%(D) = gameD0%(D) + 1
                    X0%(D, 0, gameD0%(D)) = idx
                    X0%(D, 1, gameD0%(D)) = B7%(D, I2)
                    X0%(D, 2, gameD0%(D)) = (I2 * 10) + (gameD0%(D) - 9)
                    batRating(D, idx, 21) = 1
                    B3%(D, I2) = idx
                    I7% = D
                    I8% = I2
                    i9% = idx

                    Call REPLACEMENTS(I7%, I8%, i9%)

                    B9%(D, B7%(D, I2)) = idx
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
Sub FLDAVG (D, gameW%, I3)

    If batRating(D, B9%(D, gameW%), 19) >= 999 Then

        I3 = 999

    Else

        If gameW% >= 7 Then
            I3 = batRating(D, B9%(D, gameW%), 19)
        Else
            If gameW% = 3 Or gameW% = 2 Then I3 = (batRating(D, B9%(D, gameW%), 19)) * .96
        End If

    End If

    I4 = batRating(D, B9%(D, gameW%), 22)
    I5 = batRating(D, B9%(D, gameW%), 23)
    I6 = batRating(D, B9%(D, gameW%), 24)
    I7 = batRating(D, B9%(D, gameW%), 25)

    If gameW% <> I4 And gameW% <> I5 And gameW% <> I6 And gameW% <> I7 Then
        If gameW% = 4 Or gameW% > 6 Then I3 = 982
        If gameW% = 2 Then I3 = 987
        If gameW% = 3 Then I3 = 980
        If gameW% = 6 Or gameW% = 5 Then I3 = 964

        If gameW% >= 7 Then
            I3 = I3 - 10
        Else
            I3 = I3 - 20
        End If

        If gameW% = 2 Or gameW% >= 4 And gameW% <= 6 Then I3 = I3 - 80: Exit Sub

    End If

    If gameW% = 4 Or gameW% = 6 Then I3 = (batRating(D, B9%(D, gameW%), 19)) * .995
    If gameW% = 5 Then I3 = (batRating(D, B9%(D, gameW%), 19)) * .985

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
        Color tmRat_GAME(team, 11), tmRat_GAME(team, 12)
        Print Left$(gameTeams$(team), 3); teamAbbreviatons$(team);

        Color 14, 0

        For I2 = inningStart% To inningEnd%
            If P = 0 And team = 0 And I2 > INNING% - 1 And inningScore(team, INNING%) = 0 Or P = 0 And team = 0 And I2 > INNING% And inningScore(team, INNING%) > 0 Or P = 1 And I2 > INNING% And team = 0 Then Exit For
            If P = 0 And team = 1 And I2 > INNING% - 1 Or P = 1 And team = 1 And inningScore(team, INNING%) = 0 And I2 > INNING% - 1 Or P = 1 And team = 1 And inningScore(team, INNING%) > 0 And I2 > INNING% Then Exit For
            Call INNDISP(I2, team)
        Next I2

        Locate , 40: Print Using "## ## ## "; S6%(team, 0); S6%(team, 1); S6%(team, 2);

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
    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12)
    Locate 5, 58: Print Left$(batters$(D, B9%(D, 8)), 10)

    'CF Stats
    Color 15, 2
    Locate 6, 59: Print Using "A:# R:#"; batRating(D, B9%(D, 8), 15); batRating(D, B9%(D, 8), 17)
    If batRating(D, B9%(D, 8), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 8), 19)), "###", 1)
    End If
    Locate 7, 59: Print "FA:" + fldPct$

    'RF / LF Names
    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12)
    Locate 6, 46: Print Left$(batters$(D, B9%(D, 7)), 10);
    Locate , 70: Print Left$(batters$(D, B9%(D, 9)), 10)

    'RF / LF Stats
    Color 15, 2
    Locate 7, 47: Print Using "A:# R:#"; batRating(D, B9%(D, 7), 15); batRating(D, B9%(D, 7), 17);
    Locate , 71: Print Using "A:# R:#"; batRating(D, B9%(D, 9), 15); batRating(D, B9%(D, 9), 17)

    If batRating(D, B9%(D, 7), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 7), 19)), "###", 1)
    End If
    Locate 8, 47: Print "FA:" + fldPct$

    If batRating(D, B9%(D, 9), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 9), 19)), "###", 1)
    End If
    Locate 8, 71: Print "FA:" + fldPct$

    'SS / 2B Names
    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12)
    Locate 10, 49: Print Left$(batters$(D, B9%(D, 6)), 10);
    Locate , 68: Print Left$(batters$(D, B9%(D, 4)), 10)

    'SS / 2B Stats
    Color 15, 2
    Locate 11, 50: Print Using "R:#"; batRating(D, B9%(D, 6), 17);
    Locate 11, 69: Print Using "R:#"; batRating(D, B9%(D, 4), 17)

    If batRating(D, B9%(D, 6), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 6), 19)), "###", 1)
    End If
    Locate 12, 50: Print "FA:" + fldPct$

    If batRating(D, B9%(D, 4), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 4), 19)), "###", 1)
    End If
    Locate 12, 69: Print "FA:" + fldPct$

    '2nd Base "Graphic"
    Locate 12, 62: Print Chr$(219);

    '1B / 3B Names
    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12)
    Locate 14, 46: Print Left$(batters$(D, B9%(D, 5)), 10);
    Locate , 70: Print Left$(batters$(D, B9%(D, 3)), 10)

    '1B / 3B Stats
    Color 15, 2
    Locate 15, 47: Print Using "R:#"; batRating(D, B9%(D, 5), 17);
    Locate 15, 71: Print Using "R:#"; batRating(D, B9%(D, 3), 17)

    If batRating(D, B9%(D, 5), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 5), 19)), "###", 1)
    End If
    Locate 16, 47: Print "FA:" + fldPct$

    If batRating(D, B9%(D, 3), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 3), 19)), "###", 1)
    End If
    Locate 16, 71: Print "FA:" + fldPct$

    '1st / 3rd Base "Graphic"
    Locate 17, 49: Print Chr$(219);
    Locate 17, 78: Print Chr$(219);

    'Pitcher Name
    Locate 17, 55: Print "["; B1$(pitchRating(D, P1%(D), 0) + 2); "] "; Left$(pitchers$(D, P1%(D)), 10)

    'Pitcher Stats
    Color 15, 2
    Locate 18, 59: Print Using "H:#"; pitchRating(D, P1%(D), 18)
    If pitchRating(D, P1%(D), 17) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(pitchRating(D, P1%(D), 17)), "###", 1)
    End If
    Locate 19, 59: Print "FA:" + fldPct$

    'Home Plate "Graphic
    Locate 21, 62: Print Chr$(219)

    'Catcher Name
    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12):
    Locate 23, 58: Print Left$(batters$(D, B9%(D, 2)), 10)

    'Catcher Stats
    Color 15, 2
    Locate 24, 59: Print Using "   A:#"; batRating(D, B9%(D, 2), 15);

    If batRating(D, B9%(D, 2), 19) = 1000 Then
        fldPct$ = "1.000"
    Else
        fldPct$ = "." + FUsing$(Str$(batRating(D, B9%(D, 2), 19)), "###", 1)
    End If
    Locate 25, 59: Print "FA:" + fldPct$;

    'Graphics / Lines
    Color 7, 0
    For X = 5 To 25: Locate X, 1: Print String$(44, 32);: Next

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
Sub FLDERR (F%, gameW%, D, I3, SB%, S2%, P)

    F% = 0

    If gameW% = 1 Then
        If SB% = 1 Then
            I3 = 995
        Else
            I3 = pitchRating(D, P1%(D), 17) + 30
        End If
    Else
        Call FLDAVG(D, gameW%, I3)
    End If

    If I3 >= 999 Then
        I6 = 1
    Else
        If S2% >= 12 And S2% <= 15 Then I6 = 1009 - I3
        If S2% >= 7 And S2% <= 9 Then I6 = 1006 - I3
        If S2% = 16 Or S2% >= 22 Then I6 = 1000 - I3
        If S2% >= 17 And S2% <= 21 Then I6 = (1000 - I3) * .05

    End If

    If gameW% = 7 Then I6 = I6 - 4
    If gameW% = 8 Then I6 = I6 - 10
    If gameW% = 9 Then I6 = I6 - 15
    If gameW% = 5 Then I6 = I6 - 15

    H6% = Int(Rnd(1) * 1000) + 1

    If H6% <= I6 Then

        F% = 1
        rand = Int(Rnd(1) * 2) + 1
        I1 = 2

        If gameW% = 1 Then
            I5 = P1%(D)
            I6 = 34
            Call ADDPIT(D, I5, I6)
        Else
            I5 = B9%(D, gameW%)
            I6 = 11
            Call INCBATDF(D, I5, I6)
        End If

        If S2% = 12 Then

            Call PBP(player$(0) + " beats out an infield single")
            If B7%(P, B1!(P)) <> 1 Then
                I5 = B3%(P, B1!(P))
                I6 = 0
                Call INCBATOFF(P, I5, I6)
                I6 = 1
                Call INCBATOFF(P, I5, I6)
            End If

            I5 = P1%(D)
            I6 = 1
            Call ADDPIT(D, I5, I6)

            If B7%(P, B1!(P)) = 1 Then
                I5 = P1%(P)
                I6 = 23
                Call INCPITOFF(P, I5, I6)
                I6 = 24
                Call INCPITOFF(P, I5, I6)
            End If

        End If

        If S2% >= 17 And S2% <= 21 Then I1 = 3
        If S2% >= 17 And S2% <= 20 Then Call OUTFIELDERR
        If S2% = 8 Or S2% = 9 Or S2% >= 22 Or S2% = 16 Then I1 = 1
        If S2% >= 12 And S2% <= 14 And rand = 1 Or S2% = 15 Then I1 = 1

        Call d100(RN)

        If I1 = 1 And gameW% = 1 And S2% = 16 Then pbpString$ = "*** WILD PICK OFF THROW BY" + " " + player$(4) + " ***"
        If I1 = 1 And RN <= 50 Then pbpString$ = "*** THROWING ERROR ON" + " " + player$(4) + " ***"
        If I1 = 1 And RN > 50 And RN <= 55 Then pbpString$ = "*** BALL KICKED BY" + " " + player$(4) + " ***"
        If I1 = 1 Then pbpString$ = "*** " + player$(4) + " THROWS IT AWAY ***"

        If I1 = 2 And RN <= 30 Then pbpString$ = "*** FIELDING ERROR ON" + " " + player$(4) + " ***"
        If I1 = 2 And RN > 30 And RN <= 50 Then pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"
        If I1 = 2 And RN > 50 And RN <= 75 Then pbpString$ = "*** BALL IS BOOTED BY" + " " + player$(4) + " ***"
        If I1 = 2 And RN > 75 And RN <= 95 Then pbpString$ = "*** BALL IS BOBBLED BY" + " " + player$(4) + " ***"
        If I1 = 2 And RN > 95 Then pbpString$ = "*** BALL IS THROUGH THE LEGS OF" + " " + player$(4) + " ***"

        If I1 = 3 And RN <= 33 Then pbpString$ = "*** BALL DROPPED BY" + " " + player$(4) + " ***"
        If I1 = 3 And RN > 33 And RN <= 66 Then pbpString$ = "*** OFF THE GLOVE OF" + " " + player$(4) + " ***"
        If I1 = 3 Then pbpString$ = "*** BALL IS MISPLAYED BY" + " " + player$(4) + " ***"

        Call PBP(pbpString$)

        If S2% = 16 Or S2% = 22 Then

            I8! = Int(Rnd(1) * 10) + 1

            If I8! < 5 Then
                A5%(0) = 2
                A5%(1) = 2
                A5%(2) = 2
                A5%(3) = 1
                Call DELAY
            End If

        Else

            Call d100(RN)
            A5%(0) = 1
            A5%(1) = 1
            A5%(2) = 1
            A5%(3) = 1

            If Not (S2% > 17 Or S2% <= 20) Then

                If RN <= 17 And S2% <> 8 Or S2% = 12 Then
                    A5%(0) = 2
                    A5%(1) = 2
                    A5%(2) = 2
                End If
            End If

        End If

        If S2% >= 17 And S2% <= 20 And nbrOuts(0) = 2 Or S2% >= 23 Or S2% = 16 And gameW% = 2 Then
            A5%(3) = 1
            A5%(2) = 2
            A5%(1) = 2
            A5%(0) = 2
        End If

        If S2% >= 17 And S2% <= 19 And RN <= 33 And nbrOuts(0) < 2 And gameB0%(1, 0) = 0 Then A5%(0) = 2

        If S2% = 16 Or S2% = 22 Then A5%(0) = 0

        If I1 > 0 And S2% <> 7 And S2% <> 8 And S2% <> 9 And S2% <> 16 And S2% <> 22 Then nbrOuts(1) = nbrOuts(1) + 1

        If S2% = 16 Or S2% = 7 Or S2% = 8 Or S2% = 9 Or S2% = 22 Then

            A5%(0) = 0

            If I8! > 4 And gameW% <> 2 Then
                A5%(1) = 1
                A5%(2) = 1
                A5%(3) = 1
            End If

        End If

        S6%(D, 2) = S6%(D, 2) + 1

        Call ADVANCEMENT(P, D, S2%, F%, I3)

    End If

End Sub


'----------------------------------------
'         GETFIELDER Subroutine
'----------------------------------------
'This subroutine returns the index? of the
'target fielder.
Sub GETFIELDER (gameW%, P, I1, D)

    gameW% = 0

    If B7%(P, B1!(P)) <> 1 Then

        If batRating(P, B3%(P, B1!(P)), 0) < 0 Then I1 = -1
        If batRating(P, B3%(P, B1!(P)), 0) > 0 Then I1 = 1
        If batRating(P, B3%(P, B1!(P)), 0) = 0 Then I1 = -1 * pitchRating(D, P1%(D), 0)

    Else

        I1 = pitchRating(P, P1%(P), 0)

    End If

End Sub


'----------------------------------------
'        GETFIELDNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'target fielder.
Sub GETFIELDNAME (gameW%, D)

    If gameW% <> 1 Then
        player$(4) = batters$(D, B9%(D, gameW%))
    Else
        player$(4) = pitchers$(D, P1%(D))
    End If

End Sub


'----------------------------------------
'        GETINFIELDER Subroutine
'----------------------------------------
'Get infielder
Sub GETINFIELDER (gameW%, P, I1, D, S2%, P2)

    Call GETFIELDER(gameW%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then gameW% = 3
    If I1 = 1 And RN <= 92 Then gameW% = 4
    If I1 = 1 And RN <= 72 Then gameW% = 6
    If I1 = 1 And RN <= 32 Then gameW% = 5

    If I1 = -1 Then gameW% = 5
    If I1 = -1 And RN <= 88 Then gameW% = 6
    If I1 = -1 And RN <= 68 Then gameW% = 4
    If I1 = -1 And RN <= 28 Then gameW% = 3

    If RN <= 12 And S2% <> 20 Then gameW% = 1
    If RN <= 4 Or RN <= 12 And S2% = 20 Then gameW% = 2

    If S2% = 21 And gameW% = 2 Then gameW% = 1
    If P2 = 4 And gameW% = 6 Then gameW% = 1
    If P2 = 4 And gameW% = 4 Or S2% = 20 And gameW% = 1 Then gameW% = 2

    Call GETFIELDNAME(gameW%, D)

End Sub

'----------------------------------------
'        GETOUTFIELDER Subroutine
'----------------------------------------
'Get outfielder
Sub GETOUTFIELDER (gameW%, P, I1, D)

    Call GETFIELDER(gameW%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then gameW% = 7
    If I1 = 1 And RN <= 50 Then gameW% = 9
    If I1 = -1 Then gameW% = 9
    If I1 = -1 And RN <= 50 Then gameW% = 7
    If RN <= 30 Then gameW% = 8

    player$(4) = batters$(D, B9%(D, gameW%))

End Sub


'----------------------------------------
'         GROUNDERPBP Subroutine
'----------------------------------------
'This subroutine manages variables and
'Play-by-Play display for grounders to
'the plate.
Sub GROUNDERPBP (gameW%, D)

    If gameW% <> 2 Then

        Call d100(RN)

        range = 140
        min = 1930

        If RN > 35 Then
            If gameW% = 1 Then range = 160: min = 2070
            If gameW% = 3 Then range = 150: min = 2230
            If gameW% = 4 Then range = 160: min = 2380
            If gameW% = 6 Then range = 160: min = 2540
            If gameW% = 5 Then range = 150: min = 2700
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
Sub GROUNDERS (gameW%, D)

    X$ = player$(4)

    If gameW% <> 2 Then
        Call GetPBPString(120, 1420, fileString$, D)
        Call PBP(X$ + " " + fileString$)
    Else

        'Field grounders in front of the plate
        Call GetPBPString(13, 2850, fileString$, D)
        If Left$(fileString$, 2) = "QQ" Then fileString$ = X$ + Right$(fileString$, Len(fileString$) - 2)
        Call PBP(fileString$)
    End If

End Sub


'----------------------------------------
'          HITS2GAP Subroutine
'----------------------------------------
' ...explanation...
Sub HITS2GAP (gameW%, P, I1, D)

    Call GETFIELDER(gameW%, P, I1, D)

    Call d100(RN)

    If I1 = 1 Then
        'should be 6 to 15 ?
        If RN <= 15 Then
            gameW% = 9
        Else
            gameW% = 7
        End If
    End If

    If I1 = -1 Then
        'should be 6 to 15 ?
        If RN <= 15 Then
            gameW% = 7
        Else
            gameW% = 9
        End If
    End If

    If RN <= 5 Then gameW% = 8

    player$(4) = batters$(D, B9%(D, gameW%))

End Sub

'----------------------------------------
'          HOLDSTATUS Subroutine
'----------------------------------------
'This subroutine displays the hold status
'of the infield for traditional text mode.
Sub HOLDSTATUS (H0%)

    Locate 17, 77

    Color 15, 2
    Print H0$(H0%)
    Color , 0

End Sub


'----------------------------------------
'          INCBATDF Subroutine
'----------------------------------------
'Increment Batter Defense
Sub INCBATDF (D, I5, I6)

    B8%(D, I5, I6) = B8%(D, I5, I6) + 1
    B5%(D, I5, I6) = B5%(D, I5, I6) + 1

End Sub

'----------------------------------------
'          INCBATOFF Subroutine
'----------------------------------------
'Increment Batter Offense
Sub INCBATOFF (P, I5, I6)

    B8%(P, I5, I6) = B8%(P, I5, I6) + 1
    B5%(P, I5, I6) = B5%(P, I5, I6) + 1

    If I6 >= 4 And I6 <= 8 And I6 <> 1 And pitchRating(1 - P, P1%(1 - P), 0) <> 1 Then

        If I6 >= 4 Then
            CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) + 1
        Else
            CSS%(P, I5, I6) = CSS%(P, I5, I6) + 1
        End If

    End If

End Sub

'----------------------------------------
'          INCPITOFF Subroutine
'----------------------------------------
'Increment Pitcher Offense
Sub INCPITOFF (P, I5, I6)

    P8%(P, I5, I6) = P8%(P, I5, I6) + 1
    P5%(P, I5, I6) = P5%(P, I5, I6) + 1

End Sub

'----------------------------------------
'        INCREMENTHIT Subroutine
'----------------------------------------
'Increment Hit
Sub INCREMENTHIT (P, D, MO)

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 15
        Call INCBATOFF(P, I5, I6)
    Else
        I5 = P1%(P)
        I6 = 37
        Call INCPITOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 13
    Call ADDPIT(D, I5, I6)

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
        Call PBP("You must replace " + pitchers$(D, P1%(D)))
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
Sub INJURYLENGTH (IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

    doneLength = 0

    IJL% = IJL% + 1
    IJ$(IJL%) = batters$(D, B9%(D, po%))

    pbpString$ = batters$(D, B9%(D, po%)) + " is injured!"
    Call PBP(pbpString$)

    pbpString$ = "He may be out for "

    ' *** Determine Injury Length ***
    il = 0

    If batRating(D, B9%(D, po%), 3) = 162 Then

        pbpString$ = pbpString$ + " the rest of the game"

    Else

        ct = 0

        For I = 0 To 22

            For I1 = 22 To 25
                If batRating(D, I, I1) = po% And batRating(D, I, 32) = 0 And I <> B9%(D, po%) Then ct = ct + 1
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

                If batRating(D, B9%(D, po%), 3) = 161 Then
                    pbpString$ = pbpString$ + " this game and next game"
                    il = 1
                Else
                    il = Int(Rnd(1) * (162 - batRating(D, B9%(D, po%), 3))) + 1
                    pbpString$ = pbpString$ + Str$(il) + " games"
                End If

            End If

        End If

    End If

    Call PBP(pbpString$)
    batRating(D, B9%(D, po%), 32) = batRating(D, B9%(D, po%), 32) + il

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

    Call d100(RN)

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
Sub LONGFLY (D, gameW%)

    Call GetPBPString(60, 1310, fileString$, D)
    Call PBP(fileString$ + " " + fieldPos$(gameW%))

    If WF < 1 And Rnd(1) > WF Then

        Call PBP("The wind is bringing it back..."):

    Else

        'Adjust for the wind factor
        If WF > 1 And Rnd(1) <= 1 - WF Then Call PBP("It's carrying well in the breeze...")

        Call GetPBPString(50, 1370, fileString$, D)
        Call PBP(fileString$)

        Call d100(RN)

        If Not ((tmRat_GAME(1, 13) = 21 Or tmRat_GAME(1, 13) = 27) And gameW% = 7) Then

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

    For I5 = 5 To 13
        Locate I5, 1
        Print Space$(44);
    Next I5

    Locate 5, 1
    Locate 21, 48

    Color 15, 2: Print Tab(62); Chr$(219); Tab(79);

    Locate 10, 1

    Color tmRat_GAME(D, 11), tmRat_GAME(D, 12)
    Print pitchers$(D, P1%(D)); " "; B1$(pitchRating(D, P1%(D), 0) + 2)

    Color 15, 0

    Call DISPBAVG(D)

    Color 1

    For X = 5 To 13: Locate X, 21: Print Chr$(179);: Next

    Locate 11, 1: Color 2, 0: Print "W-L ERA ";

    Color 15, 0

    Print Using "##"; pitchRating(D, P1%(D), 1);: Print "-";: Print Using "##"; pitchRating(D, P1%(D), 2);
    Print " ";: Print Using "##.##"; pitchRating(D, P1%(D), 10) / 100

    Color 2, 0

    Print "  IP  H  BB  K GAME"

    Color 15, 0

    If P8%(D, P1%(D), 0) = 0 Then
        Print " 0.0";
    Else
        Print Using "##"; Int(P8%(D, P1%(D), 0) / 3);
        Print ".";: Print Using "#"; P8%(D, P1%(D), 0) - (Int(P8%(D, P1%(D), 0) / 3) * 3);
    End If

    Print Using " ##  ## ##"; P8%(D, P1%(D), 1); P8%(D, P1%(D), 4); P8%(D, P1%(D), 5)
    Locate 5, 1
    J = B3%(P, B1!(P))
    Color tmRat_GAME(P, 11), tmRat_GAME(P, 12)

    If B7%(P, B1!(P)) = 1 Then
        X$ = pitchers$(P, P1%(P))
        Print X$; " "; B1$(pitchRating(P, P1%(P), 0) + 2);
        Call PITPLACEMENT(P, X$)
    Else
        X$ = batters$(P, J)
        Print X$; " "; B1$(batRating(P, J, 0) + 2);
        If batRating(P, J, 0) < 0 Or batRating(P, J, 0) = 0 And pitchRating(D, P1%(D), 0) > 0 Then Locate 21, 64
        X = Len(X$)
        If batRating(P, J, 0) > 0 Or batRating(P, J, 0) = 0 And pitchRating(D, P1%(D), 0) < 0 Then Locate 21, 51 + 10 - X
        Print X$
    End If

    Locate 6, 1
    Color 2, 0: Print " AVG HR  BB   K"
    Color 15, 0

    If B7%(P, B1!(P)) <> 1 Then
        If batRating(P, J, 4) = 0 Then
            Print ".000  0   0   0";
        Else
            bAvg! = batRating(P, J, 6) / batRating(P, J, 4) * 1000
            Print "."; Using "###"; bAvg!
            Print Using " ## ### ###"; batRating(P, J, 9); batRating(P, J, 11); batRating(P, J, 12)
        End If
    Else

        If pitchRating(P, P1%(P), 11) <> 999 Then
            pAvg! = pitchRating(P, P1%(P), 11)
            If pAvg! = 0 Then
                Print ".000 0"
            Else
                Print "."; Using "###"; pAvg! * 1000
            End If
        Else
            If pitchRating(P, P1%(P), 20) = 0 Then
                Print ".000 0";
            Else
                pAvg! = pitchRating(P, P1%(P), 22) / pitchRating(P, P1%(P), 20) * 1000
                Print "."; Using "###"; pAvg!
                Print Using " ## ### ###"; pitchRating(P, P1%(P), 25); pitchRating(P, P1%(P), 27); pitchRating(P, P1%(P), 28)
            End If
        End If
    End If

    Locate 8, 2

    If B7%(P, B1!(P)) <> 1 Then
        Print B8%(P, B3%(P, B1!(P)), 1); "-"; B8%(P, B3%(P, B1!(P)), 0);
    Else
        Print P8%(P, P1%(P), 24); "-"; P8%(P, P1%(P), 23);
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

    Call HOLDSTATUS(H0%)

    Locate 13, 58
    Color 15, 2: Print Tab(68);
    Locate 14, 58: Print Space$(8)

    Locate 13, 58

    I2 = 2
    X = 58

    Call DisplayRun(I2, P, X)

    If nbrOuts(0) <> 3 Then MO = 0

    For WK = 1 To 3
        If gameB0%(WK, 0) > 0 Then MO = MO + 1
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


        PB% = 0

        If useVGA = 1 Then

            Color 4294967295
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

        Erase G$

    End If

End Sub

'----------------------------------------
'        NEWPITCHER Subroutine
'----------------------------------------
'NewPitcher
Sub NEWPITCHER (pIdx, PPH, pz)

    V0 = pIdx

    If S1%(D) <> -1 And P6%(D) > 1 Then I5 = P1%(D): I6 = 22: Call ADDPIT(D, I5, I6)

    P1%(D) = V0
    P6%(D) = P6%(D) + 1
    pitchRating(D, V0, 15) = P6%(D)
    P9 = D

    Call PITSTAM(P9)

    For V0 = 1 To 9

        If B7%(D, V0) = 1 Or B7%(D, V0) = 10 Then
            B3%(D, V0) = P1%(D)
            B7%(D, V0) = 1
            B9%(D, 1) = P1%(D)
            P9 = D
            W5 = P9
            W6 = P9
            VV(P9) = V0
            VV1(P9) = V0
            Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)
        End If

    Next V0

    VV(P9) = 1
    VV1(P9) = 9
    B9%(D, 1) = P1%(D)

    Call CHECKSAVE(D, P, S4%)

    W5 = 1 - P9
    W6 = 1 - P9
    VV(1 - P9) = 1
    VV1(1 - P9) = 9

    Call WINDEX

    Locate 15, 1

    Call d100(RN)

    If RN <= 50 Then Call PBP("Hold the phone...")

    Call PBP(gameManagers$(D) + " is making a pitching change")
    Call DELAY
    Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

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

    P8%(P9, I5, I6) = P8%(P9, I5, I6) + 1
    P5%(P9, I5, I6) = P5%(P9, I5, I6) + 1

End Sub

'----------------------------------------
'        OUTATFIRST Subroutine
'----------------------------------------
'This subroutine handles the stats and
'Play-by-Play for an out at first base.
Sub OUTATFIRST (gameW%, D, fbun%, FB2P%)

    Call PITCHERNAME(gameW%, D)
    Call GROUNDERS(gameW%, D)
    Call d100(RN)

    If gameW% = 3 And RN <= 94 Then
        Call PBP("he takes it to the bag unassisted")
        fbun% = 1
    Else
        If gameW% = 3 Then
            gameW% = 1
            Call PITCHERNAME(gameW%, D)
            Call PBP("He flips it to " + player$(4) + " covering")
            FB2P% = 1
            gameW% = 3
        End If

        If gameW% = 4 Then Call SECOND2FIRST(D)

        If gameW% = 5 Or gameW% = 6 Then Call THROW2FIRST(D)

    End If

End Sub

'----------------------------------------
'        OUTFIELDERR Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'for an outfield error.
Sub OUTFIELDERR ()

    Call d100(RN)

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
Function OUTFLDLOCATION$ (gameW%)

    Call d100(RN)

    If gameW% = 7 Or gameW% = 8 And RN > 50 Then OUTFLDLOCATION$ = "lcf"
    If gameW% = 9 Or gameW% = 8 And RN <= 50 Then OUTFLDLOCATION$ = "rcf"

End Function

'----------------------------------------
'         OUTFLDNAME Subroutine
'----------------------------------------
'This subroutine returns the name of the
'specified outfielder.
Sub OUTFLDNAME (gameW%, D)

    gameW% = 9

    Call d100(RN)

    Select Case RN
        Case 1 To 5: gameW% = 7
        Case 6 To 25: gameW% = 8
    End Select

    player$(4) = batters$(D, B9%(D, gameW%))

End Sub


'----------------------------------------
'          PICKEDOFF Subroutine
'----------------------------------------
'This subroutine covers the stats and the
'Play-by-Play for a runner who is picked
'off by the pitcher or catcher.
Sub PICKEDOFF (D, i)

    ' *** picked off ***
    Call WINDEX
    Call DELIVERY(D)

    Call d100(RN)
    Select Case RN
        Case 1 To 33: pbpString$ = "NO, " + pitchers$(D, P1%(D)) + " throws to " + baseName$(i - 1)
        Case 34 To 66: pbpString$ = "quick move to " + baseName$(i - 1)
        Case 67 To 100: pbpString$ = "pickoff throw to " + baseName$(i - 1)
    End Select

    Call PBP(pbpString$)

    Call d100(RN)

    If RN <= 50 Then
        pbpString$ = player$(i - 1) + " is picked off base!"
    Else
        pbpString$ = player$(i - 1) + " is caught napping"
    End If

    Call PBP(pbpString$)

    Call d100(RN)

    Select Case RN
        Case 1 To 25: pbpString$ = "he can't believe it"
        Case 26 To 50: pbpString$ = "what a teriffic move by " + pitchers$(D, P1%(D))
    End Select

    Call PBP(pbpString$)

End Sub

'----------------------------------------
'         PINCHRUNNER Subroutine
'----------------------------------------
' ...explanation...
Sub PINCHRUNNER (MS%, A1, S4%, I2, P, PR%, nbrBalls, nbrStrikes, D, MO)

    Dim pinchRunners(24)

    ' *** CHECK FOR PINCH RUNNER ***
    i0% = 0

    If A1 <= 1 And A1 >= -2 And INNING% >= 8 Then

        I2 = 1

        If S4% = 1 Or S4% = 5 Then I2 = 1
        If S4% = 2 Or S4% = 4 Or S4% = 6 Then I2 = 2

        ' J REPRESENTS THE SPOT IN THE BATTING ORDER
        ' J1 represents the base
        J = gameB0%(I2, 0)
        J1 = I2

        If J <> 0 Then

            If MG%(P, 771) = 999 Then

                For I1 = 772 To 777
                    If MG%(P, I1) <> -1 Then
                        If batRating(P, MG%(P, I1), 21) = 0 Then
                            I1 = MG%(P, I1)
                            If B7%(P, J) = 1 Then
                                'PINCH RUNNING FOR THE PITCHER
                                If MG%(P, 803) = 999 Then
                                    Call d100(RN)
                                    If pitchRating(P, P1%(P), 31) <= MG%(P, 804) And INNING% >= MG%(P, 805) And RN <= MG%(P, 806) Then 8993
                                    If pitchRating(P, P1%(P), 31) <= MG%(P, 807) And INNING% >= MG%(P, 808) And RN <= MG%(P, 809) Then 8993
                                    GoTo 8999
                                End If
                            End If

                            For I = 0 To 5
                                If MG%(P, I * 4 + 779) = B3%(P, J) Then
                                    'FOUND A MATCH FOR THE RUNNER ON BASE
                                    Call d100(RN)
                                    If INNING% >= MG%(P, I * 4 + 780) And batRating(P, MG%(P, I * 4 + 781), 21) = 0 And RN <= MG%(P, I * 4 + 782) Then batRating(P, MG%(P, I * 4 + 781), 21) = 40 + J: GoTo 8993
                                    GoTo 8999
                                End If
                            Next

                        End If
                        MG%(P, I1) = -1
                    End If

                Next

                GoTo 8999

            End If

            If B7%(P, J) = 1 And pitchRating(P, P1%(P), 31) > 4 Then 8999
            If B7%(P, J) <> 1 And batRating(P, B3%(P, J), 16) > 3 Then 8999

            I1 = -1

            For I = 0 To 22
                If batters$(P, I) <> "XXX" And batRating(P, I, 21) <= 0 Then
                    For I2 = 22 To 25
                        If batRating(P, I, I2) <> -1 And batRating(P, I, I2) = B7%(P, J) And B7%(P, J) <> 1 Then I1 = I
                    Next
                    If batRating(P, I, 16) > 5 Then
                        pinchRunners(i0%) = I
                        i0% = i0% + 1
                    End If
                End If
            Next

            If i0% = 0 Or I1 = -1 And B7%(P, J) <> 1 Then 8999
            If B7%(P, J) <> 1 Then batRating(P, I1, 21) = 40 + J

            RN = Int(Rnd(1) * i0%)
            I1 = pinchRunners(RN)

            8993 '
            I2 = J
            player$(J1) = batters$(P, I1)
            batRating(P, I1, 21) = 1
            B3%(P, I2) = I1
            B9%(P, B7%(P, I2)) = I1

            If B7%(P, I2) = 1 Or B7%(P, I2) = 10 Then B9%(P, 1) = 99: B9%(P, 0) = I2: B7%(P, I2) = 10

            gameD0%(P) = gameD0%(P) + 1
            X0%(P, 0, gameD0%(P)) = B3%(P, I2)
            X0%(P, 1, gameD0%(P)) = 11
            X0%(P, 2, gameD0%(P)) = (I2 * 10) + (gameD0%(P) - 9)

            W5 = P
            W6 = P
            VV(P) = I2
            VV1(P) = I2

            For xx = 22 To 25
                If B7%(P, I2) = batRating(P, I1, xx) Then batRating(P, I1, 15) = batRating(P, I1, 33 + (xx - 22) * 2): batRating(P, I1, 17) = batRating(P, I1, 34 + (xx - 22) * 2): batRating(P, I1, 19) = batRating(P, I1, xx + 4): Exit For
            Next

            Call WINDEX
            Locate 15, 1
            Call PBP(gameManagers$(P) + " is sending in a pinch runner")
            PR% = 1

            Call DELAY
            Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

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
Sub PITCHERNAME (gameW%, D)

    If gameW% = 1 Then
        player$(4) = pitchers$(D, P1%(D))
    Else
        player$(4) = batters$(D, B9%(D, gameW%))
    End If

End Sub


'----------------------------------------
'          PITCHOUT Subroutine
'----------------------------------------
'This subroutine handles the stats and
'Play-by-Play for a pitchout.
Sub PITCHOUT (S4%, TGTBASE%, Q0)

    If S4% <> 5 And TGTBASE% = 2 Then

        Call d100(PJ)

        If PJ <= 8 Then
            Call PBP("...PITCH OUT!!!")
            Q0 = Q0 - 200
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
    Color tmRat_GAME(P9, 11), tmRat_GAME(P9, 12): Print gameTeams$(P9);
    Color 14, 0
    Print " T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"

    For pIdx = 0 To 21:

        If pitchRating(P9, pIdx, 15) <= 0 Then
            Color 2, 0

            If pIdx <= 9 Then
                Print Using "#"; pIdx;
            Else
                Print alpha$(pIdx - 10);
            End If

            Color 15, 0

            Locate , 3: Print pitchers$(P9, pIdx);
            Locate , 18: Print B1$(pitchRating(P9, pIdx, 0) + 2);

            Print Using " ### ### ### ## ## ## ## ### ### ##"; pitchRating(P9, pIdx, 6); pitchRating(P9, pIdx, 7); pitchRating(P9, pIdx, 4); pitchRating(P9, pIdx, 5); pitchRating(P9, pIdx, 16); pitchRating(P9, pIdx, 3); pitchRating(P9, pIdx, 13); pitchRating(P9, pIdx, 8); pitchRating(P9, pIdx, 9); pitchRating(P9, pIdx, 1);
            Print "-";
            Print Using "##"; pitchRating(P9, pIdx, 2);

            Locate , 59: Print Using "##.## # ##"; pitchRating(P9, pIdx, 10) / 100; pitchRating(P9, pIdx, 18); pitchRating(P9, pIdx, 12)

        End If

    Next pIdx

End Sub


'----------------------------------------
'           PITOFF Subroutine
'----------------------------------------
' ...explanation...
Sub PITOFF (P, I5, I6)

    P8%(P, I5, I6) = P8%(P, I5, I6) - 1
    P5%(P, I5, I6) = P5%(P, I5, I6) - 1

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
        Color 4294967295

    End If

    If pitchRating(P, P1%(P), 0) < 0 Then
        If useVGA = 1 Then
            PrintOnGfx 42, 25, X$, imageScreen&
        Else
            Locate 21, 64
            Print X$
        End If
    End If

    If pitchRating(P, P1%(P), 0) > 0 Then
        If useVGA = 1 Then
            PrintOnGfx 30, 25, X$, imageScreen&
        Else
            Locate 21, 51
            Print X$
        End If
    End If

    If useVGA = 1 Then
        'Printing white on black
        Color 4294967295
        _PrintMode _FillBackground , imageScreen&
    End If

End Sub


'----------------------------------------
'           PITSTAM Subroutine
'----------------------------------------
' ...explanation...
Sub PITSTAM (P9)

    If pitchRating(P9, P1%(P9), 4) = 0 Then
        S8%(P9, 0) = 3
    Else
        S8%(P9, 0) = Int(((pitchRating(P9, P1%(P9), 8) + pitchRating(P9, P1%(P9), 7)) / pitchRating(P9, P1%(P9), 4)) + .5)
    End If

    If S8%(P9, 0) < 3 Then S8%(P9, 0) = 3
    S8%(P9, 1) = 3

    If P6%(P9) > 1 Then S8%(P9, 1) = 0

    If pitchEraOpt > 0 Then
        If P6%(P9) > 1 Then
            S8%(P9, 1) = 1
        Else
            S8%(P9, 0) = S8%(P9, 0) + 1
        End If
    End If

    If INNING% <= 5 And S6%(P9, 0) - S6%(1 - P9, 0) > 0 Then P2%(P9) = P1%(P9)

End Sub


'----------------------------------------
'          PITSTRAT Subroutine
'----------------------------------------
'This subroutine loads a pitcher strategy
'from a manager profile.
Sub PITSTRAT (PPH, pz)

    Shared SU()

    A1 = S6%(D, 0) - S6%(P, 0)

    pz = 0

    ' if there are no middle relievers in the profile,
    ' and it's less than the 7th inning, then go back
    ' and find one the old fashioned way (which is?)

    If MG%(D, 60) = 999 Or INNING% > 7 Then

        'if no closers set up or no closing strategy defined, skip out
        If Not (INNING% >= 8 And MG%(D, 520) <> 999 And MG%(D, 90) <> 999 Or MG%(D, 581) <> 999) Then

            'if still needing middle relief
            If INNING% <= 7 Then
                GoTo 8115
            End If

            ' we have defined a closing/setup possibility
            ' identify the stratgey
            I2 = 0
            For pIdx = 582 To 587
                I2 = I2 + MG%(D, pIdx)
            Next
            I3 = Int(Rnd(1) * I2) + 1
            I2 = 0
            For pIdx = 582 To 587
                I2 = I2 + MG%(D, pIdx)
                If I3 <= I2 Then
                    SN% = pIdx

                    8111 SF% = 0

                    If A1 < 1 Then 8195

                    If A1 > 0 And A1 <= 2 Then SF% = 1
                    If S4% = 0 And A1 <= 2 Then SF% = 1
                    If S4% > 0 And S4% < 4 And A1 <= 3 Then SF% = 1
                    If S4% > 3 And S4% < 7 And A1 <= 4 Then SF% = 1
                    If S4% = 7 And A1 <= 5 Then SF% = 1
                    If S4% = 0 And nbrOuts(0) = 0 And A1 <= 3 Then SF% = 1
                    If B1!(P) <= 7 Then For pIdx = 0 To 2: I3 = I3 + batRating(P, B3%(P, B1!(P) + pIdx), 0): Next
                    If B1!(P) = 8 Then For pIdx = 0 To 1: I3 = I3 + batRating(P, B3%(P, B1!(P) + pIdx), 0): Next: I3 = I3 + batRating(P, B3%(P, 1), 0)
                    If B1!(P) = 9 Then For pIdx = 1 To 2: I3 = I3 + batRating(P, B3%(P, pIdx), 0): Next: I3 = I3 + batRating(P, B3%(P, 9), 0)

                    If I3 = 0 Then
                        Do
                            I3 = Int(Rnd(1) * 3) - 1
                        Loop Until I3 > 0
                        'Why not just do a random # from 1 to 2???
                    End If

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113
                    If INNING% >= 9 And SF% = 1 And (SN% = 582 Or SN% = 587) Then 8113

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 > 0 And SN% = 583 Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 > 0 And SN% = 583 Then 8113
                    If INNING% >= 9 And A1 > 0 And SN% = 583 Then 8113

                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 >= 0 And SN% = 584 Then 8114
                    If INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 >= 0 And SN% = 584 Then 8113
                    If INNING% >= 9 And A1 >= 0 And SN% = 584 Then 8113

                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 > 0 And SN% = 585 Then 8114
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 > 0 And SN% = 585 Then 8113
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% >= 9 And A1 > 0 And SN% = 585 Then 8113

                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 0 And A1 >= 0 And SN% = 586 Then 8114
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% = 8 And MG%(D, 588) = 8 And MG%(D, 589) = 1 And A1 >= 0 And SN% = 586 Then 8113
                    If S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) <= P8%(D, P1%(D), 2) And INNING% >= 9 And A1 >= 0 And SN% = 586 Then 8113

                    Exit For

                End If
            Next

        End If

    End If

    GoTo 8195

    'strategy identified, now

    8114
    I2 = 0
    If I3 < 0 Then I1 = 81
    If I3 > 0 Then I1 = 84
    For pIdx = I1 To I1 + 2
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = I1 To I1 + 2
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
        If I3 <= I2 Then
            V0 = MG%(D, pIdx)
            MG%(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, PPH, pz)
            SU(D, 0) = -1
            GoTo 8195
        End If
    Next
    'NO SETUP MAN FOUND
    'GOTO 8195

    8113
    I2 = 0
    If I3 < 0 Then I1 = 521
    If I3 > 0 Then I1 = 524

    For pIdx = I1 To I1 + 2
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = I1 To I1 + 2
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
        If I3 <= I2 Then
            V0 = MG%(D, pIdx)
            MG%(D, pIdx) = -1
            pIdx = V0
            Call NEWPITCHER(pIdx, PPH, pz)
            SU(D, 1) = -1
            GoTo 8195
        End If
    Next

    'NO CLOSER FOUND
    SU(D, 0) = -1
    SU(D, 1) = -1

    8115 '
    I2 = 0

    For pIdx = 61 To 70
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
    Next

    I3 = Int(Rnd(1) * I2) + 1
    I2 = 0

    For pIdx = 61 To 70
        If MG%(D, pIdx) >= 0 And pitchRating(D, MG%(D, pIdx), 15) = 0 Then I2 = I2 + MG%(D, pIdx + 10)
        If I3 <= I2 Then V0 = MG%(D, pIdx): MG%(D, pIdx) = -1: pIdx = V0: Call NEWPITCHER(pIdx, PPH, pz): GoTo 8195
    Next

    'NO middle RELIEVER FOUND

    8195 '
End Sub


'----------------------------------------
'           PITSWAP Subroutine
'----------------------------------------
' ...explanation...
Sub PITSWAP (i)

    If i = 4 Then
        V = 3
        Call SOMESTATS(V, P, F%)
        V = 2
        Call SOMESTATS(V, P, F%)
        V = 1
        Call SOMESTATS(V, P, F%)
    End If

    If i = 3 Then
        V = 2
        Call SOMESTATS(V, P, F%)
        V = 1
        Call SOMESTATS(V, P, F%)
    End If

    If i = 2 Then
        V = 2
        Call SOMESTATS(V, P, F%)
        V = 1
        Call SOMESTATS(V, P, F%)
    End If

End Sub


'----------------------------------------
'         PLAYERNAME Subroutine
'----------------------------------------
' ...explanation...
Sub PLAYERNAME (P)

    If B7%(P, B1!(P)) = 1 Then
        player$(0) = pitchers$(P, P1%(P))
    Else
        player$(0) = batters$(P, B3%(P, B1!(P)))
    End If

    For pIdx = 1 To 3

        If gameB0%(pIdx, 0) > 0 Then
            If B7%(P, gameB0%(pIdx, 0)) <> 1 Then
                player$(pIdx) = batters$(P, B3%(P, gameB0%(pIdx, 0)))
            Else
                player$(pIdx) = pitchers$(P, P1%(P))
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

            If batRating(P, pIdx, 21) <= 0 And batters$(P, pIdx) <> "XXX" Then

                If B7%(P, B1!(P)) = batRating(P, pIdx, 22) Or B7%(P, B1!(P)) = batRating(P, pIdx, 23) Or B7%(P, B1!(P)) = batRating(P, pIdx, 24) Or B7%(P, B1!(P)) = batRating(P, pIdx, 25) Then

                    Call PinchHitter(P, P9, W5, W6, pIdx)

                    If useVGA = 1 Then
                        Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                    Else
                        Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                        Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO): MS% = 1
                    End If

                    Locate 15, 1

                    player$(0) = batters$(P, B3%(P, B1!(P)))
                    If B7%(P, B1!(P)) = 1 Then player$(0) = pitchers$(P, P1%(D))

                    Exit Sub

                End If

            End If

        Next pIdx


        For pIdx = 0 To 22

            If batRating(P, pIdx, 21) <= 0 And batters$(P, pIdx) <> "XXX" Then

                Call PinchHitter(P, P9, W5, W6, pIdx)

                If useVGA = 1 Then
                    Call DisplayBallField_Gfx(nbrBalls, P, nbrStrikes, D)
                    Call MAINSCREEN_G(D, P, nbrBalls, nbrStrikes, MO)
                Else
                    Call DisplayBallField(nbrBalls, P, nbrStrikes, D)
                    Call MAINSCREEN(D, P, nbrBalls, nbrStrikes, MO): MS% = 1
                End If

                Locate 15, 1

                player$(0) = batters$(P, B3%(P, B1!(P)))
                If B7%(P, B1!(P)) = 1 Then player$(0) = pitchers$(P, P1%(D))

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

    gameD0%(P) = gameD0%(P) + 1
    X0%(P, 0, gameD0%(P)) = idx
    X0%(P, 1, gameD0%(P)) = B7%(P, B1!(P))
    X0%(P, 2, gameD0%(P)) = (B1!(P) * 10) + (gameD0%(P) - 9)
    batRating(P, idx, 21) = 1
    B3%(P, B1!(P)) = idx
    B9%(P, B7%(P, B1!(P))) = idx

    If B7%(P, B1!(P)) = 1 Or B7%(P, B1!(P)) = 10 Then
        B9%(P, 1) = 99
        B9%(P, 0) = B1!(P)
        B7%(P, B1!(P)) = 10
    End If

    P9 = P
    W5 = P9
    W6 = P9
    VV(P9) = B1!(P)
    VV1(P9) = B1!(P)
    Call HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

    VV(P9) = 1
    VV1(P9) = 9

End Sub


'----------------------------------------
'        PopUp_Lineup Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team lineup
Sub PopUp_Lineup ()

    If useVGA = 1 Then Screen 0

    Call FRAMEOPTION(11, 1)

    Print gameTeams$(0); Tab(40); gameTeams$(1)

    For J = 0 To 1

        For X = 1 To 9:

            Locate X + 9, 10 + J * 26
            If B1!(J) = X Then Print "->";

            Locate , 12 + J * 26
            If B7%(J, X) = 1 Then

                Print B1$(pitchRating(J, P1%(J), 0) + 2); Tab(14 + J * 26); Left$(pitchers$(J, P1%(J)), 10); " P"; Tab(28 + J * 26);

                If pitchRating(J, P1%(J), 11) <> 999 Then
                    Print " ."; Using "###"; pitchRating(J, P1%(J), 11) '/ 1000
                Else

                    If pitchRating(J, P1%(J), 20) = 0 Then
                        Print " .000"
                    Else
                        If pitchRating(J, P1%(J), 20) = pitchRating(J, P1%(J), 22) Then
                            Print "1.000"
                        Else
                            Print " ."; Using "###"; pitchRating(J, P1%(J), 22) / pitchRating(J, P1%(J), 20)
                        End If
                    End If

                End If

            Else

                Print B1$(batRating(J, B3%(J, X), 0) + 2); Tab(14 + J * 26); Left$(batters$(J, B3%(J, X)), 10); " "; fieldPos$(B7%(J, X)); Tab(28 + J * 26);

                If batRating(J, B3%(J, X), 4) = 0 Then
                    Print " .000"
                Else
                    If batRating(J, B3%(J, X), 6) = batRating(J, B3%(J, X), 4) Then
                        Print "1.000"
                    Else
                        Print Using " .###"; batRating(J, B3%(J, X), 6) / batRating(J, B3%(J, X), 4)
                    End If
                End If

            End If

        Next

    Next

    Color 15, 0
    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$

End Sub


'----------------------------------------
'        PopUp_Batter Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team batters
Sub PopUp_Batter (WK, M)

    If TS%(WK, 0) = 999 Then

        Locate 4, 56: Print "VS LHP  .000";
        If batRating(WK, M, 42) <> 0 Then
            statPCT! = batRating(WK, M, 43) / batRating(WK, M, 42)
            Locate 4, 64: Print Using ".###"; statPCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If batRating(WK, M, 49) <> 0 Then
            statPCT! = batRating(WK, M, 50) / batRating(WK, M, 49)
            Locate 5, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 8, 6: Print "SEASON";
    Locate , 15

    If batRating(WK, M, 4) = 0 Then
        Print ".---  .---"
    Else
        statPCT! = batRating(WK, M, 6) / batRating(WK, M, 4)
        statPCT2! = (batRating(WK, M, 6) + batRating(WK, M, 11)) / (batRating(WK, M, 4) + batRating(WK, M, 11))
        Print Using ".###  .### ### ### ### ### ## ## ## ### ### ### "; statPCT!; statPCT2!; batRating(WK, M, 3); batRating(WK, M, 4); batRating(WK, M, 5); batRating(WK, M, 6); batRating(WK, M, 7); batRating(WK, M, 8); batRating(WK, M, 9); batRating(WK, M, 10);
        Print Using "### ### ### ##"; batRating(WK, M, 11); batRating(WK, M, 12); batRating(WK, M, 13); batRating(WK, M, 14)
    End If

    Locate , 6: Print "LEAGUE";

    If B5%(WK, M, 0) = 0 Or B5%(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If B5%(WK, M, 0) = B5%(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = B5%(WK, M, 1) / B5%(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If B5%(WK, M, 0) + B5%(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If B5%(WK, M, 1) + B5%(WK, M, 7) = B5%(WK, M, 0) + B5%(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            statPCT! = (B5%(WK, M, 1) + B5%(WK, M, 7)) / (B5%(WK, M, 0) + B5%(WK, M, 7))
            Locate , 21: Print Using ".###"; statPCT!;
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B5%(WK, M, 14); B5%(WK, M, 0); B5%(WK, M, 2); B5%(WK, M, 1); B5%(WK, M, 4); B5%(WK, M, 5); B5%(WK, M, 6); B5%(WK, M, 3); B5%(WK, M, 7); B5%(WK, M, 8); B5%(WK, M, 9); B5%(WK, M, 10); B5%(WK, M, 12); B5%(WK, M, 13)

    Locate , 6: Print "GAME";

    If B8%(WK, M, 0) = 0 Or B8%(WK, M, 1) = 0 Then
        Locate , 15: Print ".000";
    Else
        If B8%(WK, M, 0) = B8%(WK, M, 1) Then
            Locate , 14: Print "1.000";
        Else
            statPCT! = B8%(WK, M, 1) / B8%(WK, M, 0)
            Locate , 15: Print Using ".###"; statPCT!;
        End If
    End If

    If B8%(WK, M, 0) + B8%(WK, M, 7) = 0 Then
        Locate , 21: Print ".000";
    Else
        If B8%(WK, M, 1) + B8%(WK, M, 7) = B8%(WK, M, 0) + B8%(WK, M, 7) Then
            Locate , 20: Print "1.000";
        Else
            Locate , 21: Print Using ".###"; (B8%(WK, M, 1) + B8%(WK, M, 7)) / (B8%(WK, M, 0) + B8%(WK, M, 7));
        End If
    End If

    Locate , 26
    Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B8%(WK, M, 14); B8%(WK, M, 0); B8%(WK, M, 2); B8%(WK, M, 1); B8%(WK, M, 4); B8%(WK, M, 5); B8%(WK, M, 6); B8%(WK, M, 3); B8%(WK, M, 7); B8%(WK, M, 8); B8%(WK, M, 9); B8%(WK, M, 10); B8%(WK, M, 12); B8%(WK, M, 13)

End Sub


'----------------------------------------
'        PopUp_Pitcher Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team pitchers
Sub PopUp_Pitcher (WK, M)

    Locate 14, 3
    Color tmRat_GAME(WK, 11), tmRat_GAME(WK, 12)

    Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7:
        Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
    Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    If TS%(WK, 0) = 999 Then

        Locate 15, 56: Print "VS LHB  .000";
        If pitchRating(WK, M, 51) <> 0 Then
            statPCT! = pitchRating(WK, M, 52) / pitchRating(WK, M, 51)
            Locate 15, 64: Print Using ".###"; statPCT!
        End If

        Locate 16, 56: Print "VS RHB  .000";
        If pitchRating(WK, M, 58) <> 0 Then
            statPCT! = pitchRating(WK, M, 59) / pitchRating(WK, M, 58)
            Locate 16, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 15, 6: Print "Stats for "; pitchers$(WK, M)
    Locate , 6: Print "THROWS: "; B1$(pitchRating(WK, M, 0) + 2); "    ";: Print Using "G/A%: ##     HOLD: #"; pitchRating(WK, M, 12); pitchRating(WK, M, 18)
    Locate , 3: Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
    Locate , 6: Print "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"

    Locate , 6: Print "SEASON   ";: Print Using "## ## ## ##.## ## ## ## ###   ### "; pitchRating(WK, M, 1); pitchRating(WK, M, 2); pitchRating(WK, M, 3); pitchRating(WK, M, 10) / 100; pitchRating(WK, M, 4); pitchRating(WK, M, 5); pitchRating(WK, M, 16); pitchRating(WK, M, 6); pitchRating(WK, M, 7);
    Print Using "### ###         ##"; pitchRating(WK, M, 8); pitchRating(WK, M, 9); pitchRating(WK, M, 13)

    Locate , 6: Print "LEAGUE   ";: Print Using "## ## ## "; P5%(WK, M, 6); P5%(WK, M, 7); P5%(WK, M, 12);

    If P5%(WK, M, 3) = 0 Or P5%(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; P5%(WK, M, 3) / (P5%(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; P5%(WK, M, 8); P5%(WK, M, 9); P5%(WK, M, 10);

    If P5%(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(P5%(WK, M, 0) / 3);
        Print ".";: Print Using "#"; P5%(WK, M, 0) - (Int(P5%(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; P5%(WK, M, 1); P5%(WK, M, 4); P5%(WK, M, 5); P5%(WK, M, 2); P5%(WK, M, 3); P5%(WK, M, 14)
    Locate , 6: Print "GAME     ";: Print Using "## ## ## "; P8%(WK, M, 6); P8%(WK, M, 7); P8%(WK, M, 12);
    If P8%(WK, M, 3) = 0 Or P8%(WK, M, 0) = 0 Then
        Print " 0.00";
    Else
        Print Using "##.##"; P8%(WK, M, 3) / (P8%(WK, M, 0) / 3) * 9;
    End If

    Print Using " ## ## ## "; P8%(WK, M, 8); P8%(WK, M, 9); P8%(WK, M, 10);

    If P8%(WK, M, 0) = 0 Then
        Print "  0.0";
    Else
        Print Using "###"; Int(P8%(WK, M, 0) / 3);
        Print ".";: Print Using "#"; P8%(WK, M, 0) - (Int(P8%(WK, M, 0) / 3) * 3);
    End If

    Print Using " ### ### ### ### ### ##"; P8%(WK, M, 1); P8%(WK, M, 4); P8%(WK, M, 5); P8%(WK, M, 2); P8%(WK, M, 3); P8%(WK, M, 14)

End Sub

'----------------------------------------
'         POPUPSTATS Subroutine
'----------------------------------------
'This subroutine displays a quick window
'that contains a view of the team stats.
'It also gives an interface for viewing
'batters and pitchers.
Sub POPUPSTATS ()

    If useVGA = 1 Then Screen 0

    J = B3%(P, B1!(P))

    'Stats for Active Batter
    Locate 3, 3: Color tmRat_GAME(P, 11), tmRat_GAME(P, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

    For X = 1 To 7:
        Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " "
    Next

    Locate , 3: Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

    Locate 4, 6: Print "Stats for ";

    If B7%(P, B1!(P)) = 1 Then
        If pitchRating(P, P1%(P), 11) <> 999 Then
            Print pitchers$(P, P1%(P))
        Else
            Print pitchers$(P, P1%(P)): Locate , 6: Print "BATS: "; B1$(pitchRating(P, P1%(P), 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; 52; pitchRating(P, P1%(P), 32); pitchRating(P, P1%(P), 31):
        End If
    Else
        Print batters$(P, J)
        Locate , 6: Print "BATS: "; B1$(batRating(P, J, 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; batRating(P, J, 18); batRating(P, J, 30); batRating(P, J, 16)
    End If

    If B7%(P, B1!(P)) = 1 And TS%(P, 0) = 999 Then

        Locate 4, 56: Print "VS LHP  .000";
        If pitchRating(P, P1%(P), 37) <> 0 Then
            statPCT! = pitchRating(P, P1%(P), 38) / pitchRating(P, P1%(P), 37)
            Locate 4, 64: Print Using ".###"; statPCT!
        End If

        Locate 5, 56: Print "VS RHP  .000";
        If pitchRating(P, P1%(P), 44) <> 0 Then
            statPCT! = pitchRating(P, P1%(P), 45) / pitchRating(P, P1%(P), 44)
            Locate 5, 64: Print Using ".###"; statPCT!
        End If

    End If

    Locate 6, 3
    Print " "; Chr$(204); String$(71, 205); Chr$(185); " "

    Locate , 6
    Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

    skipLeague = 0
    If B7%(P, B1!(P)) = 1 And pitchRating(P, P1%(P), 11) <> 999 Then

        statPCT! = pitchRating(P, P1%(P), 11) '/ 1000
        Locate , 6: Print "SEASON";
        Locate , 15: Print Using ".###"; statPCT!

    Else

        If B7%(P, B1!(P)) <> 1 Then

            WK = P
            M = J
            Call PopUp_Batter(WK, M)
            skipLeague = 1

        Else

            Locate , 6: Print "SEASON";: Locate , 15

            If pitchRating(P, P1%(P), 20) = 0 Then
                Print ".---  .---"
            Else
                Print Using ".###  .### ### ### ### ### ## "; pitchRating(P, P1%(P), 22) / pitchRating(P, P1%(P), 20); (pitchRating(P, P1%(P), 22) + pitchRating(P, P1%(P), 27)) / (pitchRating(P, P1%(P), 27) + pitchRating(P, P1%(P), 20)); pitchRating(P, P1%(P), 19); pitchRating(P, P1%(P), 20); pitchRating(P, P1%(P), 21); pitchRating(P, P1%(P), 22); pitchRating(P, P1%(P), 23);
                Print Using "## ## ### ### ### ### ##"; pitchRating(P, P1%(P), 24); pitchRating(P, P1%(P), 25); pitchRating(P, P1%(P), 26); pitchRating(P, P1%(P), 27); pitchRating(P, P1%(P), 28); pitchRating(P, P1%(P), 29); pitchRating(P, P1%(P), 30)
            End If

        End If

    End If

    If skipLeague <> 1 Then

        Locate , 6: Print "LEAGUE";

        J = P1%(P)

        If P5%(P, J, 23) = 0 Or P5%(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If P5%(P, J, 23) = P5%(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statPCT! = P5%(P, J, 24) / P5%(P, J, 23)
                Locate , 15: Print Using ".###"; statPCT!;
            End If
        End If

        If P5%(P, J, 23) + P5%(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If P5%(P, J, 24) + P5%(P, J, 30) = P5%(P, J, 23) + P5%(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statPCT! = (P5%(P, J, 24) + P5%(P, J, 30)) / (P5%(P, J, 23) + P5%(P, J, 30))
                Locate , 21: Print Using ".###"; statPCT!;
            End If
        End If

        Locate , 26: Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P5%(P, J, 8); P5%(P, J, 23); P5%(P, J, 25); P5%(P, J, 24); P5%(P, J, 27); P5%(P, J, 28); P5%(P, J, 29); P5%(P, J, 26); P5%(P, J, 30); P5%(P, J, 31); P5%(P, J, 32); P5%(P, J, 33); P5%(P, J, 35); P5%(P, J, 36)
        Locate , 6: Print "GAME";

        If P8%(P, J, 23) = 0 Or P8%(P, J, 24) = 0 Then
            Locate , 15: Print ".000";
        Else
            If P8%(P, J, 23) = P8%(P, J, 24) Then
                Locate , 14: Print "1.000";
            Else
                statPCT! = P8%(P, J, 24) / P8%(P, J, 23)
                Locate , 15: Print Using ".###"; statPCT!;
            End If
        End If

        If P8%(P, J, 23) + P8%(P, J, 30) = 0 Then
            Locate , 21: Print ".000";
        Else
            If P8%(P, J, 24) + P8%(P, J, 30) = P8%(P, J, 23) + P8%(P, J, 30) Then
                Locate , 20: Print "1.000";
            Else
                statPCT! = (P8%(P, J, 24) + P8%(P, J, 30)) / (P8%(P, J, 23) + P8%(P, J, 30))
                Locate , 21: Print Using ".###"; statPCT!;
            End If
        End If

        Locate , 26
        Print Using "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P8%(P, J, 8); P8%(P, J, 23); P8%(P, J, 25); P8%(P, J, 24); P8%(P, J, 27); P8%(P, J, 28); P8%(P, J, 29); P8%(P, J, 26); P8%(P, J, 30); P8%(P, J, 31); P8%(P, J, 32); P8%(P, J, 33); P8%(P, J, 35); P8%(P, J, 36)

    End If

    WK = D
    M = P1%(D)
    Call PopUp_Pitcher(WK, M)

    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
    I$ = GetKeyPress$
    Color 7, 0

End Sub

'----------------------------------------
'           PUTOUT Subroutine
'----------------------------------------
' ...explanation...
Sub PUTOUT (i, P)

    If B7%(P, gameB0%(i - 1, 0)) = 1 Then
        I5 = P1%(P)
        I6 = 33
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, gameB0%(i - 1, 0))
        I6 = 10
        Call INCBATOFF(P, I5, I6)
    End If

End Sub


'----------------------------------------
'          RecordOut Subroutine
'----------------------------------------
'This subroutine handles the recording of
'an out, along with the Play-by-Play
Sub RecordOut (i, I3)

    Call PBP(player$(i) + " out @ " + baseName$(I3))

    gameB0%(i, 0) = 0
    gameB0%(i, 1) = 0
    gameB0%(i, 2) = 0
    player$(i) = ""

End Sub


'----------------------------------------
'          RecordRun Subroutine
'----------------------------------------
' ...explanation...
Sub RecordRun (sah%, i, D, F%, PQ, S2%, gameW%, INFPOS%)

    If sah% = 0 Then Call PBP(player$(i) + " scores")
    sah% = 0

    I5 = gameB0%(i, 1)
    I6 = 2
    Call ADDPIT(D, I5, I6)

    If P1%(D) <> gameB0%(i, 1) Then I5 = P1%(D): I6 = 18: Call ADDPIT(D, I5, I6)

    If P6%(P) = 1 Then P8%(P, P1%(P), 20) = P8%(P, P1%(P), 20) + 1

    If B7%(P, gameB0%(i, 0)) = 1 Then
        I5 = P1%(P)
        I6 = 25
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, gameB0%(i, 0))
        I6 = 2
        Call INCBATOFF(P, I5, I6)
    End If

    If F% = 1 Then
        If nbrOuts(0) = 2 Then
            PQ = 8
        Else
            Select Case gameW%
                Case Is <= 3:
                    If S2% <> 23 Or F% = 1 And gameW% = 5 And S2% <> 23 Then PQ = 8
                Case 4, 6
                    If INFPOS% = 2 Then PQ = 8
                Case Else
                    If S2% >= 4 And S2% <= 6 Then PQ = 8
            End Select

        End If
    End If

    If PQ <> 8 Then
        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 26
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B1!(P))
            I6 = 3
            Call INCBATOFF(P, I5, I6)
        End If

        If F% = 1 Then PQ = 8

    End If

    inningScore(P, INNING%) = inningScore(P, INNING%) + 1
    S6%(P, 0) = S6%(P, 0) + 1

    If S6%(P, 0) - S6%(D, 0) = 1 Then
        P2%(D) = gameB0%(i, 1)
        P2%(P) = P1%(P)
        S1%(1) = -1
        S1%(0) = -1
    Else
        If S6%(P, 0) - S6%(D, 0) = 0 And S1%(D) <> -1 Then
            I5 = P1%(D)
            I6 = 19
            Call ADDPIT(D, I5, I6)
            S1%(D) = -1
        End If
    End If

    gameB0%(i, 0) = 0
    gameB0%(i, 1) = 0
    gameB0%(i, 2) = 0
    player$(i) = ""

    Call d100(RN0)
    If inningScore(P, INNING%) = 4 And RN0 <= 25 Then Call PBP("They're tearin' up the peapatch!")
    If INNING% >= 9 And P = 1 And S6%(P, 0) - S6%(D, 0) = 1 Then
        If F% = 0 And nbrOuts(1) < 3 And S2% <> 10 Then Call CHARGERUN(D, i)
        If S2% <> 10 Then
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

    If INNING% > 5 And S8%(D, 0) < P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) And S8%(D, 1) < P8%(D, P1%(D), 2) Then
        Locate 10, 17
        Color 30, 0: Print "-T-"
        Color 15, 0
    End If

    Locate 5, 1

    For X = 1 To 9

        Color 3, 0
        Locate X + 4, 22

        If B1!(P) = X Then Color 11, 0

        Locate , 22

        If B7%(P, X) = 1 Then

            Print B1$(pitchRating(P, P1%(P), 0) + 2); Tab(25); Left$(pitchers$(P, P1%(P)), 10); " P"; Tab(39);

            If pitchRating(P, P1%(P), 11) <> 999 Then
                If pitchRating(P, P1%(P), 11) = 0 Then
                    Print " .000"
                Else
                    Print " ."; Using "###"; pitchRating(P, P1%(P), 11)
                End If

            Else

                If pitchRating(P, P1%(P), 20) = 0 Then
                    Print " .000"
                Else
                    If pitchRating(P, P1%(P), 20) = pitchRating(P, P1%(P), 22) Then
                        Print "1.000"
                    Else
                        pAvg! = (pitchRating(P, P1%(P), 22) / pitchRating(P, P1%(P), 20)) * 1000
                        Print " ."; Using "###"; pAvg!
                    End If

                End If

            End If

        Else

            Print B1$(batRating(P, B3%(P, X), 0) + 2); Tab(25); Left$(batters$(P, B3%(P, X)), 10); " "; fieldPos$(B7%(P, X)); Tab(39);

            If batRating(P, B3%(P, X), 4) = 0 Then
                Print " .000"
            Else
                If batRating(P, B3%(P, X), 6) = batRating(P, B3%(P, X), 4) Then
                    Print "1.000"
                Else
                    bAvg! = (batRating(P, B3%(P, X), 6) / batRating(P, B3%(P, X), 4)) * 1000
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
Sub HITTINGLINES '(tmRat_GAME(), pitchRating(), P1%(), B7%(), batRating(), VV(), VV1(), B3%(), B4%(), parkHRVals!(), WF!)

    Randomize Timer

    'CLS
    'PRINT "Please Wait..."
    'PRINT
    'PRINT "Calculating Pitcher/Batter Park Effects..."

    For I = 0 To 1

        I1 = 1 - I

        'CALCULATE BASE ON BALLS PERCENTAGES
        For I3 = VV(I) To VV1(I)

            I6! = tmRat_GAME(I1, 3) / 1000

            If pitchRating(I1, P1%(I1), 6) * 3 * .94 + pitchRating(I1, P1%(I1), 7) + pitchRating(I1, P1%(I1), 8) = 0 Then
                I2! = 0
            Else
                I2! = pitchRating(I1, P1%(I1), 8) / (pitchRating(I1, P1%(I1), 6) * 3 * .94 + pitchRating(I1, P1%(I1), 7) + pitchRating(I1, P1%(I1), 8))
                'I2! = pitchRating(I1, P1%(I1), 8) / (((pitchRating(I1, P1%(I1), 6) * 3) * .94) + pitchRating(I1, P1%(I1), 7) + pitchRating(I1, P1%(I1), 8))
            End If

            II = batRating(I, B3%(I, I3), 0)
            I4 = pitchRating(I1, P1%(I1), 0)

            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2

            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then
                I6! = gameSA%(I1, TY% * 6 + 3) / 1000
                I2! = pitchRating(I1, P1%(I1), 56 + PY% * 7) * .9 / (pitchRating(I1, P1%(I1), 51 + PY% * 7) + pitchRating(I1, P1%(I1), 56 + PY% * 7) * .9)
            End If

            If B7%(I, I3) = 1 Then

                If pitchRating(I, P1%(I), 11) <> 999 Then
                    I7! = .037
                Else
                    If (pitchRating(I, P1%(I), 27) + pitchRating(I, P1%(I), 20) = 0) Then
                        I7! = 0
                    Else
                        If TS%(I, 0) = 999 Then
                            If pitchRating(I, P1%(I), 37 + BY% * 7) + pitchRating(I, P1%(I), 42 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = pitchRating(I, P1%(I), 42 + BY% * 7) * .9 / (pitchRating(I, P1%(I), 42 + BY% * 7) * .9 + pitchRating(I, P1%(I), 37 + BY% * 7))
                            End If
                        Else
                            I7! = pitchRating(I, P1%(I), 27) / (pitchRating(I, P1%(I), 27) + pitchRating(I, P1%(I), 20))
                        End If
                    End If

                End If

            Else

                If batRating(I, B3%(I, I3), 4) + batRating(I, B3%(I, I3), 11) = 0 Then
                    I7! = 0
                Else

                    If TS%(I, 0) = 999 Then
                        If batRating(I, B3%(I, I3), 47 + BY% * 7) + batRating(I, B3%(I, I3), 42 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            If batRating(I, B3%(I, I3), 80) <> 0 Then
                                I7! = batRating(I, B3%(I, I3), 47 + BY% * 7) * .9 / (batRating(I, B3%(I, I3), 42 + BY% * 7) + batRating(I, B3%(I, I3), 47 + BY% * 7) * .9)
                            End If
                        End If
                    Else
                        I7! = batRating(I, B3%(I, I3), 11) / (batRating(I, B3%(I, I3), 4) + batRating(I, B3%(I, I3), 11))
                    End If

                End If

            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            B4%(I, 5, I3) = K8!

        Next I3

        ' CALCULATE STRIKEOUT PERCENTAGES
        For I3 = VV(I) To VV1(I)

            If pitchRating(I1, P1%(I1), 6) = 0 Then
                I2! = 0
            Else
                I2! = pitchRating(I1, P1%(I1), 9) / ((pitchRating(I1, P1%(I1), 6) * 3) * .94)
            End If

            I6! = tmRat_GAME(I1, 2) / 1000

            II = batRating(I, B3%(I, I3), 0)
            I4 = pitchRating(I1, P1%(I1), 0)
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2
            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then

                If pitchRating(I1, P1%(I1), 51 + PY% * 7) - pitchRating(I1, P1%(I1), 52 + PY% * 7) = 0 Then
                    I2! = 0
                Else
                    I2! = pitchRating(I1, P1%(I1), 57 + PY% * 7) / (pitchRating(I1, P1%(I1), 51 + PY% * 7) - pitchRating(I1, P1%(I1), 52 + PY% * 7))
                    I6! = gameSA%(I1, TY% * 6 + 2) / 1000
                End If

            End If

            If B7%(I, I3) = 1 Then

                If pitchRating(I, P1%(I), 11) <> 999 Then
                    I7! = .42
                Else

                    If pitchRating(I, P1%(I), 20) - pitchRating(I, P1%(I), 22) = 0 Then
                        I7! = 0
                    Else

                        If TS%(I, 0) = 999 Then
                            If pitchRating(I, P1%(I), 37 + BY% * 7) - pitchRating(I, P1%(I), 38 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = pitchRating(I, P1%(I), 43 + BY% * 7) / (pitchRating(I, P1%(I), 37 + BY% * 7) - pitchRating(I, P1%(I), 38 + BY% * 7))
                            End If
                        Else
                            If B7%(I, I3) = 1 Then
                                I7! = pitchRating(I, P1%(I), 28) / (pitchRating(I, P1%(I), 20) - pitchRating(I, P1%(I), 22))
                            End If
                        End If

                    End If

                End If

            Else

                If batRating(I, B3%(I, I3), 4) = 0 Or batRating(I, B3%(I, I3), 4) - batRating(I, B3%(I, I3), 6) = 0 Then
                    I7! = 0
                Else

                    If TS%(I, 0) = 999 Then

                        If batRating(I, B3%(I, I3), 80) <> 0 Then
                            If batRating(I, B3%(I, I3), 42 + BY% * 7) - batRating(I, B3%(I, I3), 43 + BY% * 7) = 0 Then
                                I7! = 0
                            Else
                                I7! = batRating(I, B3%(I, I3), 48 + BY% * 7) / (batRating(I, B3%(I, I3), 42 + BY% * 7) - batRating(I, B3%(I, I3), 43 + BY% * 7))
                            End If
                        End If
                    Else
                        I7! = batRating(I, B3%(I, I3), 12) / (batRating(I, B3%(I, I3), 4) - batRating(I, B3%(I, I3), 6))
                    End If

                End If

            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            B4%(I, 4, I3) = K8!

        Next I3

        ' CALCULATE THE BATTING AVERAGE
        For I3 = VV(I) To VV1(I)

            I2! = tmRat_GAME(I1, 1) / 1000
            I6! = pitchRating(I1, P1%(I1), 14) / 1000

            If pitchRating(I1, P1%(I1), 6) + pitchRating(I1, P1%(I1), 7) = 0 Then
                I7! = 0
            Else
                I7! = pitchRating(I1, P1%(I1), 7) / (((pitchRating(I1, P1%(I1), 6) * 3) * .94) + pitchRating(I1, P1%(I1), 7))
            End If

            Call NORMALIZE(I7!, I2!, I6!, K8!)
            i9! = K8! / 1000
            II = batRating(I, B3%(I, I3), 0)
            I4 = pitchRating(I1, P1%(I1), 0)
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2
            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then

                I6! = pitchRating(I1, P1%(I1), TY% * 6 + 65) / 1000

                If pitchRating(I1, P1%(I1), 51 + PY% * 7) = 0 Then
                    I7! = 0
                Else
                    I7! = pitchRating(I1, P1%(I1), 52 + PY% * 7) / pitchRating(I1, P1%(I1), 51 + PY% * 7)
                End If

            End If

            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then I2! = gameSA%(I1, TY% * 6 + 1) / 1000
            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then Call NORMALIZE(I7!, I2!, I6!, K8!)
            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then i9! = K8! / 1000

            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 11) <> 999 Then

                I7! = pitchRating(I, P1%(I), 11) / 1000: I6! = pitchRating(I, P1%(I), 14) / 1000

            Else

                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 20) = 0 Then
                    I7! = 0
                Else

                    If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then
                        If pitchRating(I, P1%(I), 37 + BY% * 7) = 0 Then
                            I7! = 0
                        Else
                            I7! = pitchRating(I, P1%(I), 38 + BY% * 7) / pitchRating(I, P1%(I), 37 + BY% * 7)
                        End If
                    Else
                        If B7%(I, I3) = 1 Then I7! = pitchRating(I, P1%(I), 22) / pitchRating(I, P1%(I), 20)
                    End If

                End If

                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then

                    I6! = pitchRating(I, P1%(I), TY% * 6 + 65) / 1000
                Else

                    If B7%(I, I3) = 1 Then
                        I6! = pitchRating(I, P1%(I), 14) / 1000
                    Else
                        If batRating(I, B3%(I, I3), 4) = 0 Then
                            I7! = 0
                        Else

                            If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then
                                If batRating(I, B3%(I, I3), 42 + BY% * 7) = 0 Then
                                    I7! = 0
                                Else
                                    I7! = batRating(I, B3%(I, I3), 43 + BY% * 7) / batRating(I, B3%(I, I3), 42 + BY% * 7)
                                End If
                            Else
                                I7! = batRating(I, B3%(I, I3), 6) / batRating(I, B3%(I, I3), 4)
                            End If

                        End If

                        I6! = batRating(I, B3%(I, I3), 20) / 1000
                        If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then I6! = batRating(I, B3%(I, I3), 56 + TY% * 6) / 1000

                    End If

                End If

            End If

            I2! = tmRat_GAME(I, 1) / 1000
            If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then I2! = gameSA%(I, TY% * 6 + 1) / 1000
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            I7! = K8! / 1000
            I6! = tmRat_GAME(I, 1) / 1000

            If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then I6! = gameSA%(I, TY% * 6 + 1) / 1000

            I2! = i9!
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            B4%(I, 0, I3) = K8!

        Next I3

        ' CALCULATE THE EXTRA BASE HITS RATIOS
        For I3 = VV(I) To VV1(I)

            II = batRating(I, B3%(I, I3), 0)
            I4 = pitchRating(I1, P1%(I1), 0)

            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = 1 Then TY% = 1
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And I4 = -1 Then TY% = 0
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = -1 Then TY% = 2
            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And I4 = 1 Then TY% = 3

            If B7%(I, I3) <> 1 And II < 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II < 0 And I4 = -1 Then TY% = 0
            If B7%(I, I3) <> 1 And II > 0 And I4 = -1 Then TY% = 2
            If B7%(I, I3) <> 1 And II > 0 And I4 = 1 Then TY% = 3
            If B7%(I, I3) <> 1 And II = 0 And I4 = 1 Then TY% = 1
            If B7%(I, I3) <> 1 And II = 0 And I4 = -1 Then TY% = 2

            If TY% = 0 Or TY% = 1 Then PY% = 0
            If TY% = 2 Or TY% = 3 Then PY% = 1
            If TY% = 0 Or TY% = 2 Then BY% = 0
            If TY% = 1 Or TY% = 3 Then BY% = 1

            If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 11) <> 999 Then
                B4%(I, 1, I3) = (130 * (i9! * 1000)) / tmRat_GAME(I1, 1)
                B4%(I, 2, I3) = (19 * (i9! * 1000)) / tmRat_GAME(I1, 1)
                B4%(I, 3, I3) = 37
            Else

                E2! = (i9! * 1000) / tmRat_GAME(I1, 1)

                If TS%(I1, 0) = 999 Then E2! = (i9! * 1000) / gameSA%(I1, TY% * 6 + 1)

                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 22) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) <> 1 And batRating(I, B3%(I, I3), 6) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 And pitchRating(I, P1%(I), 38 + BY% * 7) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 1, I3) = Int((pitchRating(I, P1%(I), 39 + BY% * 7) / pitchRating(I, P1%(I), 38 + BY% * 7)) * 1000) * E2!: ' doubles times  * .95
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 2, I3) = Int((pitchRating(I, P1%(I), 40 + BY% * 7) / pitchRating(I, P1%(I), 38 + BY% * 7)) * 1000) * E2!
                If B7%(I, I3) = 1 And TS%(I, 0) = 999 Then B4%(I, 3, I3) = Int((pitchRating(I, P1%(I), 41 + BY% * 7) / pitchRating(I, P1%(I), 38 + BY% * 7)) * 1000): GoTo 235
                If B7%(I, I3) = 1 Then B4%(I, 1, I3) = Int((pitchRating(I, P1%(I), 23) / pitchRating(I, P1%(I), 22)) * 1000) * E2!: ' doubles times  * .95
                If B7%(I, I3) = 1 Then B4%(I, 2, I3) = Int((pitchRating(I, P1%(I), 24) / pitchRating(I, P1%(I), 22)) * 1000) * E2!
                If B7%(I, I3) = 1 Then B4%(I, 3, I3) = Int((pitchRating(I, P1%(I), 25) / pitchRating(I, P1%(I), 22)) * 1000): GoTo 235

                If batRating(I, B3%(I, I3), 6) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235

                If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 And batRating(I, B3%(I, I3), 43 + BY% * 7) = 0 Then B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GoTo 235
                If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then B4%(I, 1, I3) = Int((batRating(I, B3%(I, I3), 44 + BY% * 7) / batRating(I, B3%(I, I3), 43 + BY% * 7)) * 1000) * E2!: 'see note on pitchers * .95
                If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then B4%(I, 2, I3) = Int((batRating(I, B3%(I, I3), 45 + BY% * 7) / batRating(I, B3%(I, I3), 43 + BY% * 7)) * 1000) * E2!
                If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) <> 0 Then B4%(I, 3, I3) = Int((batRating(I, B3%(I, I3), 46 + BY% * 7) / batRating(I, B3%(I, I3), 43 + BY% * 7)) * 1000): GoTo 235

                B4%(I, 1, I3) = Int((batRating(I, B3%(I, I3), 7) / batRating(I, B3%(I, I3), 6)) * 1000) * E2!: 'see note on pitchers * .95
                B4%(I, 2, I3) = Int((batRating(I, B3%(I, I3), 8) / batRating(I, B3%(I, I3), 6)) * 1000) * E2!
                B4%(I, 3, I3) = Int((batRating(I, B3%(I, I3), 9) / batRating(I, B3%(I, I3), 6)) * 1000)

            End If

            235 '
            I6! = tmRat_GAME(I1, 4) / 1000
            I7! = B4%(I, 3, I3) / 1000
            I2! = 0

            If pitchRating(I1, P1%(I1), 7) <> 0 Then I2! = pitchRating(I1, P1%(I1), 13) / pitchRating(I1, P1%(I1), 7)
            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 Then I6! = gameSA%(I1, TY% * 6 + 4) / 1000
            If TS%(I1, 0) = 999 And pitchRating(I1, P1%(I1), 89) <> 0 And pitchRating(I1, P1%(I1), 52 + PY% * 7) <> 0 Then I2! = pitchRating(I1, P1%(I1), 55 + PY% * 7) / pitchRating(I1, P1%(I1), 52 + PY% * 7)
            Call NORMALIZE(I7!, I2!, I6!, K8!)

            B4%(I, 3, I3) = K8!

        Next I3

        ' ADJUST LEFTY/RIGHTY THE OLD WAY
        I4 = pitchRating(I1, P1%(I1), 0)

        For I3 = VV(I) To VV1(I)

            If TS%(I, 0) = 999 And batRating(I, B3%(I, I3), 80) = 0 Then

                I2 = batRating(I, B3%(I, I3), 0)

                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And pitchRating(I1, P1%(I1), 0) = 1 Then A = 1.04: A1 = .88: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = -1 And pitchRating(I1, P1%(I1), 0) = -1 Then A = .93: A1 = 1.16: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And pitchRating(I1, P1%(I1), 0) = -1 Then A = 1.02: A1 = .98: Call LRADJ(A!, A1!)
                If B7%(I, I3) = 1 And pitchRating(I, P1%(I), 0) = 1 And pitchRating(I1, P1%(I1), 0) = 1 Then A = .97: A1 = 1.1: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -2 And I4 = 1 Then A = 1.04: A1 = .96: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -2 And I4 = -1 Then A = .78: A1 = 1.22: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 2 And I4 = 1 Then A = .78: A1 = 1.22: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 2 And I4 = -1 Then A = 1.04: A1 = .96: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -1 And I4 = -1 Then A = .93: A1 = 1.16: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = -1 And I4 = 1 Then A = 1.04: A1 = .88: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 1 And I4 = 1 Then A = .97: A1 = 1.1: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 1 And I4 = -1 Then A = 1.02: A1 = .98: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 0 And I4 = -1 Then A = 1.01: A1 = .86: Call LRADJ(A!, A1!)
                If B7%(I, I3) <> 1 And I2 = 0 And I4 = 1 Then A = .99: A1 = .93: Call LRADJ(A!, A1!)

            End If

            B4%(I, 6, I3) = B4%(I, 3, I3)
            B4%(I, 7, I3) = B4%(I, 3, I3)

        Next I3

        ' ADJUST AVERAGES FOR THE HOME TEAM
        For I3 = VV(I) To VV1(I)
            If I = 1 Then B4%(I, 0, I3) = B4%(I, 0, I3) + 5
            If I <> 1 Then B4%(I, 0, I3) = B4%(I, 0, I3) - 5
        Next I3

        ' CALCULATE THE HOME PARK FACTORS
        If gameLoc <> 2 Then

            For I2 = VV(I) To VV1(I)
                If I = 1 Then
                    B4%(I, 1, I2) = Int(B4%(I, 1, I2) * (tmRat_GAME(I, 6) / 100) + .5)
                    B4%(I, 2, I2) = Int(B4%(I, 2, I2) * (tmRat_GAME(I, 7) / 100) + .5)
                Else
                    'I <> 1
                    B4%(I, 1, I2) = Int(B4%(I, 1, I2) * (tmRat_GAME(I1, 6) / 100) * ((200 - tmRat_GAME(I, 6)) / 100) + .5)
                    B4%(I, 2, I2) = Int(B4%(I, 2, I2) * (tmRat_GAME(I1, 7) / 100) * ((200 - tmRat_GAME(I, 7)) / 100) + .5)
                End If

                If parkHRVals!(I) > 1000 Then I3! = 100: I6! = Int(parkHRVals!(I) / 1000) * WF!: I7! = (parkHRVals!(I) - Int((parkHRVals!(I) / 1000)) * 1000) * WF!
                If parkHRVals!(I1) > 1000 Then I4! = 100: K8! = Int(parkHRVals!(I1) / 1000) * WF!: i9! = (parkHRVals!(I1) - Int((parkHRVals!(I1) / 1000)) * 1000) * WF!

                If parkHRVals!(I) < 1000 Then I3! = parkHRVals!(I) * WF!: I6! = I3!: I7! = I3!
                If parkHRVals!(I1) < 1000 Then I4! = parkHRVals!(I1) * WF!: K8! = I4!: i9! = I4!

                If I = 1 Then
                    B4%(I, 3, I2) = Int((B4%(I, 3, I2) * (I3! / 100)) + .5)
                    B4%(I, 6, I2) = Int((B4%(I, 6, I2) * (I6! / 100)) + .5)
                    B4%(I, 7, I2) = Int((B4%(I, 7, I2) * (I7! / 100)) + .5)
                Else
                    'I <> 1
                    B4%(I, 3, I2) = Int((B4%(I, 3, I2) * (I4! / 100) * ((200 - I3!) / 100)) + .5)
                    B4%(I, 6, I2) = Int((B4%(I, 6, I2) * (K8! / 100) * ((200 - I6!) / 100)) + .5)
                    B4%(I, 7, I2) = Int((B4%(I, 7, I2) * (i9! / 100) * ((200 - I7!) / 100)) + .5)
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
        B4%(I, I6, I3) = B4%(I, I6, I3) * A!
    Next I6

    B4%(I, 5, I3) = B4%(I, 5, I3) * A!
    B4%(I, 4, I3) = B4%(I, 4, I3) * A1!

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

    K8! = Int((K8! * 1000) + .0005)

End Sub

'----------------------------------------
'         BASEONBALLS Subroutine
'----------------------------------------
'This subroutine manages the stats and the
'Play-by-Play for a Base-on-Balls
Sub BASEONBALLS (S2%, D2, D, P)

    S2% = 2

    Call WINDEX

    Call d100(BV)
    Call d100(RN)

    If D2 = 2 Then

        If BV <= 50 Then
            Call PBP(player$(0) + " is walked intentionally...")
        Else
            Call PBP(player$(0) + " is given a purpose pass...")
        End If

        S8%(D, 0) = S8%(D, 0) + 1

    Else

        If RN <= 30 Then Call PBP("Full count...")

        Call DELIVERY(D)

        Call GetPBPString(80, 780, fileString$, D)
        Call PBP(fileString$)

        Call GetPBPString(30, 860, fileString$, D)
        Call PBP(fileString$)

    End If

    Call d100(RN)

    '-- Where does s4 get a value???
    '-- There is an S4 variable, but
    '--     it's passed as a parameter to
    '--     any routines that need it
    '-- From context of original source,
    '--     it can have a value of 0 to 7

    '-- is s4 the # of "BALLS"?
    '-- theny why look for it to get as high as 7?

    '-- is it a SUM of STRIKES + BALLS?
    '-- it would never get to 7
    If s4% = 7 And RN <= 30 Then Call PBP("that forces a run in")

    RN = Int(Rnd(1) * 4) + 1

    '-- here, if s4 is between 4 and 6, bases are loaded
    '-- in context, this is obviously AFTER the walk
    If s4% > 3 And s4% < 7 Then
        Select Case RN
            Case 1: Call PBP("and the bases are loaded!")
            Case 2: Call PBP("and the sacks are jammed!")
            Case 3: Call PBP("and the bases are full!")
            Case 4: Call PBP("and that loads them up!")
        End Select
    End If

    I5 = P1%(D)
    I6 = 4
    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 30
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 7
        Call INCBATOFF(P, I5, I6)
    End If

End Sub


'----------------------------------------
'         HITBYPITCH Subroutine
'----------------------------------------
'This subroutine handles the Play-by-Play
'text for a player hit by a pitch.
Sub HITBYPITCH (S2%, D)

    S2% = 1

    Call WINDEX
    Call DELIVERY(D)

    Call GetPBPString(20, 720, fileString$, D)
    Call PBP(fileString$)

    Call GetPBPString(11, 740, fileString$, D)
    Call PBP(fileString$)

    Call GetPBPString(20, 760, fileString$, D)
    Call PBP(fileString$)

    Call d100(RN)

    If RN <= 50 And s4% = 7 Then Call PBP("tough way to get an RBI")

End Sub

'----------------------------------------
'          WILDPITCH Subroutine
'----------------------------------------
' ...explanation...
Sub WILDPITCH (S2%, D)

    S2% = 6
    ruleAsWild = 0

    Call d100(RN)

    Select Case RN

        Case 1 To 16:
            Call PBP("and it gets away from " + batters$(D, B9%(D, 2)))
            ruleAsWild = 1

        Case 17 To 32:
            Call PBP("pitch is WAY high, and " + batters$(D, B9%(D, 2)) + " can't get it")
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
            Call PBP("it's over " + batters$(D, B9%(D, 2)) + "'s head")

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
            Call PBP("wild pitch by " + pitchers$(D, P1%(D)))

    End Select

    If ruleAsWild = 1 Then

        Call d100(RN)

        Select Case RN
            Case 1 To 33: pbpString$ = "It's ruled a wild pitch"
            Case 34 To 66: pbpString$ = "score it a wild pitch"
            Case 67 To 100: pbpString$ = "credit " + pitchers$(D, P1%(D)) + " with a wild pitch"
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
        A1 = S6%(D, 0) - S6%(P, 0)

        For rIdx = 0 To 21

            A2 = pitchRating(D, rIdx, 4) - pitchRating(D, rIdx, 5) - pitchRating(D, rIdx, 3) * 1.3

            If Not (pitchRating(D, rIdx, 15) > 0 Or pitchers$(D, rIdx) = "XXX" Or A2 = 0) Then
                If INNING% <= 7 And A2 > 0 Then I2 = I2 + A2
                If INNING% > 7 And A1 < 0 And A2 > 0 Then I2 = I2 + A2
                If INNING% > 7 And A1 >= 0 Then I2 = I2 + pitchRating(D, rIdx, 3)
            End If

        Next rIdx

        I3 = Int(Rnd(1) * I2) + 1
        I2 = 0

        For rIdx = 0 To 21

            A2 = pitchRating(D, rIdx, 4) - pitchRating(D, rIdx, 5) - pitchRating(D, rIdx, 3) * 1.3

            If Not (pitchRating(D, rIdx, 15) > 0 Or pitchers$(D, rIdx) = "XXX" Or A2 = 0) Then

                If INNING% <= 7 Then

                    If A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER(rIdx, PPH, pz)
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    End If

                Else

                    'INNING% > 7
                    If A1 < 0 And A2 > 0 Then
                        If I3 <= I2 + A2 Then
                            Call NEWPITCHER(rIdx, PPH, pz)
                            Exit Sub
                        Else
                            I2 = I2 + A2
                        End If
                    Else
                        If A1 >= 0 And I3 > I2 Then
                            If I3 <= I2 + pitchRating(D, rIdx, 3) Then
                                Call NEWPITCHER(rIdx, PPH, pz)
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
            If pitchRating(D, rIdx, 15) = 0 And pitchers$(D, rIdx) <> "XXX" Then Call NEWPITCHER(rIdx, PPH, pz): Exit For
        Next rIdx

    End If

End Sub


'----------------------------------------
'        REPLACEMENTS Subroutine
'----------------------------------------
' ...explanation...
Sub REPLACEMENTS (I7%, I8%, i9%)

    For xx = 22 To 25

        If B7%(I7%, I8%) = batRating(I7%, i9%, xx) Then
            batRating(I7%, i9%, 15) = batRating(I7%, i9%, 33 + (xx - 22) * 2)
            batRating(I7%, i9%, 17) = batRating(I7%, i9%, 34 + (xx - 22) * 2)
            batRating(I7%, i9%, 19) = batRating(I7%, i9%, xx + 4)
            Exit Sub
        End If

    Next

    J1 = B7%(I7%, I8%)

    Select Case J1

        Case 2
            J2 = 1
            J3 = 0
            J4 = 910 + Int(Rnd(1) * 10) + 1

        Case 3
            J2 = 0
            J3 = 3
            J4 = 960 + Int(Rnd(1) * 10) + 1

        Case 4
            J2 = 0
            J3 = 3
            J4 = 912 + Int(Rnd(1) * 10) + 1

        Case 5 To 6
            J2 = 0
            J3 = 3
            J4 = 902 + Int(Rnd(1) * 10) + 1

        Case Is >= 7:
            J2 = 2
            J3 = 2
            J4 = 972 + Int(Rnd(1) * 10) + 1

    End Select

    batRating(I7%, i9%, 15) = J2
    batRating(I7%, i9%, 17) = J3
    batRating(I7%, i9%, 19) = J4

End Sub


'----------------------------------------
'         SCOREBOARD Subroutine
'----------------------------------------
'This subroutine essentially updates the
'hit counter on the scoreboard only.
Sub SCOREBOARD ()

    S6%(P, 1) = S6%(P, 1) + 1

    If useVGA = 0 Then
        Color 14, 0
        Locate P + 2, 43
        Print Using "##"; S6%(P, 1);
        Color 15, 0
    Else
        FORMATTED$ = FUsing$(Str$(S6%(P, 1)), "##", 1)
        PrintOnGfx 56, P + 28, FORMATTED$, imageScreen&
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
Sub SINGLEPBP (S2%, D, gameW%)

    If S2% = -12 Then

        S2% = 12

    Else
        Call DELIVERY(D)
        Call d100(RN)

        If S2% = 12 Then
            Call PBP(player$(0) + " beats out an infield single")
        Else

            If RN <= 50 Then
                Call GetPBPString(50, 1610, fileString$, D)
                Call PBP(fileString$ + " " + fieldPos$(gameW%))
            Else
                If gameW% = 7 Then range = 70: min = 1660
                If gameW% = 9 Then range = 70: min = 1730
                If gameW% = 8 Then range = 90: min = 1800
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
Sub SINGLEROUTINE (gameW%, P, I1, D, S2%, P2)

    Call GETOUTFIELDER(gameW%, P, I1, D)
    Call WINDEX

    If B7%(P, B1!(P)) <> 1 Then
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
        I6 = 1
        Call INCBATOFF(P, I5, I6)
    End If

    I5 = P1%(D)
    I6 = 1
    Call ADDPIT(D, I5, I6)

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
        I6 = 24
        Call INCPITOFF(P, I5, I6)
    End If

    '-- Original code is as follows:
    'If H7% > sn Then Call SINGLEPBP(S2%, D, gameW%)
    '-- But there is no clue what sn is supposed to be
    Call SINGLEPBP(S2%, D, gameW%)

    For I = 0 To 3
        A5%(I) = 1
    Next I

    If S2% <> 12 And gameB0%(2, 0) <> 0 Then

        Call d100(H6%)

        If B7%(P, gameB0%(2, 0)) <> 1 Then
            BR% = batRating(P, B3%(P, gameB0%(2, 0)), 16)
        Else
            BR% = pitchRating(P, P1%(P), 31)
        End If

        If nbrOuts(0) = 0 Then
            If gameW% = 7 Then I3 = 16 + BR% * 3
            If gameW% = 8 Then I3 = 49 + BR% * 3
            If gameW% = 9 Then I3 = 27 + BR% * 3
        End If

        If nbrOuts(0) = 1 Then
            If gameW% = 7 Then I3 = 36 + BR% * 3
            If gameW% = 8 Then I3 = 61 + BR% * 3
            If gameW% = 9 Then I3 = 44 + BR% * 3
        End If

        If nbrOuts(0) = 2 Then
            If gameW% = 7 Then I3 = 79 + BR% * 3
            If gameW% = 8 Then I3 = 82 + BR% * 3
            If gameW% = 9 Then I3 = 79 + BR% * 3
        End If

        If H6% <= I3 Or P2 = 2 And S2% <> 12 Then A5%(2) = 2

    End If

    If Not (gameB0%(1, 0) = 0 Or S2% = 12 Or gameB0%(2, 0) > 0 And A5%(2) = 1) Then

        Call d100(H6%)

        BR% = batRating(P, B3%(P, gameB0%(1, 0)), 16)

        If B7%(P, gameB0%(1, 0)) = 1 Then BR% = pitchRating(P, P1%(P), 31)

        I3 = 0

        If nbrOuts(0) = 0 Then
            If gameW% = 7 Then I3 = 6 + BR% * 3
            If gameW% = 8 Then I3 = 25 + BR% * 3
            If gameW% = 9 Then I3 = 38 + BR% * 3
        End If

        If nbrOuts(0) = 1 Then
            If gameW% = 7 Then I3 = 10 + BR% * 3
            If gameW% = 8 Then I3 = 26 + BR% * 3
            If gameW% = 9 Then I3 = 40 + BR% * 3
        End If

        If nbrOuts(0) = 2 Then
            If gameW% = 7 Then I3 = 11 + BR% * 3
            If gameW% = 8 Then I3 = 45 + BR% * 3
            If gameW% = 9 Then I3 = 62 + BR% * 3
        End If

        If H6% <= I3 Or P2 = 2 And S2% <> 12 Then A5%(1) = 2

    End If

    Call ADVANCEMENT(P, D, S2%, F%, I3)

End Sub

'----------------------------------------
'          SOMESTATS Subroutine
'----------------------------------------
' ...explanation...
Sub SOMESTATS (V, P, F%)

    If gameB0%(V, 0) > 0 And F% = 0 Then

        If B7%(P, gameB0%(V, 0)) = 1 Then
            I5 = P1%(P)
            I6 = 32
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, gameB0%(V, 0))
            I6 = 9
            Call INCBATOFF(P, I5, I6)
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
Sub STEAL3RD (TGTBASE%, AA, Q0, H0%, D)

    If TGTBASE% = 3 Then
        ' *** SUCCESS OF 3B STEAL ***
        If AA >= 24 Then
            Q0 = 830
        Else
            Q0 = 700
        End If
    End If

    If TGTBASE% = 4 Then Q0 = Q0 * .5

    ' *** H0%=0, runner not held ***
    If H0% = 0 Then
        Q0 = Q0 + 50
    Else
        ' *** H0% = 1, runner is held ***
        Q0 = Q0 - ((pitchRating(D, P1%(D), 18) - 5) * 25): ' - 50
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

    AA = batRating(P, I2, 13)
    BB = batRating(P, I2, 13) + batRating(P, I2, 14)
    CC = batRating(P, I2, 7): ' *** CHANCE TO STEAL 2B & 3B ***

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

        FF = batRating(P, I2, 6) + batRating(P, I2, 11) - batRating(P, I2, 9)

        If FF = 0 Then
            I1 = 0
        Else
            If AA >= 12 Then I1 = Int(((BB - BB * DD) / FF) * 1500)
            If AA < 12 Then I1 = Int((BB / FF) * 1500)
            If I1 < 0 Then I1 = 0
        End If

    Else
        If BB < 10 And nbrOuts(0) = 0 Then I1 = 0
        If BB < 10 And nbrOuts(0) = 1 Then I1 = 10
        If nbrOuts(0) = 2 Or TT = 0 Then I1 = 0
        If BB >= 10 And nbrOuts(0) = 1 Then I1 = BB * DD / TT * 1000
        If BB >= 10 And nbrOuts(0) = 0 Then I1 = BB * DD / TT * 1000
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
        Call d100(RN)

        If RN <= 20 Then Call PBP("FULL COUNT...")

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

        Call d100(RN)
        If RN <= 25 Then
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

    Call d100(RN)

    Select Case RN
        Case 1 To 20: pbpString$ = "Number" + Str$(P8%(D, P1%(D), 5) + 1) + " for " + pitchers$(D, P1%(D))
        Case 21 To 40: pbpString$ = "That's" + Str$(P8%(D, P1%(D), 5) + 1) + " for " + pitchers$(D, P1%(D))
        Case 41: pbpString$ = player$(0) + " is not a happy man"
        Case 42: pbpString$ = player$(0) + " has had better AB's"
    End Select

    Call PBP(pbpString$)

End Sub


'----------------------------------------
'         STRIKEOUTCS Subroutine
'----------------------------------------
' ...explanation...
Sub STRIKEOUTCS (SO)

    If SO = 7 Then B1!(P) = B1!(P) + 1

End Sub


'----------------------------------------
'         TAGADVANCE Subroutine
'----------------------------------------
' ...explanation...
Sub TAGADVANCE (S2%, I8!, D)

    If S2% = 7 And I8! = 2 Then

        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 27
            Call INCPITOFF(P, I5, I6)
        Else
            I5 = B3%(P, B1!(P))
            I6 = 4
            Call INCBATOFF(P, I5, I6)
        End If

    End If

    If S2% = 8 And I8! = 3 Then

        If B7%(P, B1!(P)) = 1 Then
            I5 = P1%(P)
            I6 = 28
            Call INCPITOFF(P, I5, I6)

            P8%(P, P1%(P), 27) = P8%(P, P1%(P), 27) - 1
            P5%(P, P1%(P), 27) = P5%(P, P1%(P), 27) - 1
            Exit Sub
        Else
            I3 = 4
            I4 = 5
            I5 = B3%(P, B1!(P))
        End If

    End If

    If S2% = 9 And I8! = 4 Then
        I3 = 5
        I4 = 6
        I5 = B3%(P, B1!(P))
    End If

    If S2% = 8 And I8! = 3 Or S2% = 9 And I8! = 4 Then

        If pitchRating(D, P1%(D), 0) <> 1 Then
            CSS%(P, I5, I3 - 2) = CSS%(P, I5, I3 - 2) - 1
            CSS%(P, I5, I4 - 2) = CSS%(P, I5, I4 - 2) + 1
        Else
            B8%(P, I5, I3) = B8%(P, I5, I3) - 1
            B5%(P, I5, I3) = B5%(P, I5, I3) - 1
            B8%(P, I5, I4) = B8%(P, I5, I4) + 1
            B5%(P, I5, I4) = B5%(P, I5, I4) + 1
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

    If S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) And S8%(P9, 1) <= P8%(P9, P1%(P9), 2) Then

        Call d100(RN)

        If S8%(P9, 1) > 0 And INNING% < 6 Or S8%(P9, 1) = 0 And P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 Then
            If RN <= 50 Then
                Print pitchers$(P9, P1%(P9)); " just doesn't have it today";
            Else
                Print pitchers$(P9, P1%(P9)); " ain't got nuthin' on his fastball folks!";
            End If
        Else
            Print pitchers$(P9, P1%(P9)); " is tired!!";
        End If

    Else

        If S8%(P9, 1) > 0 And (S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) Or S8%(P9, 1) <= P8%(P9, P1%(P9), 2)) Or (P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 2 And P8%(P9, P1%(P9), 0) >= 5) Then
            Print pitchers$(P9, P1%(P9)); " is tiring!!";
        Else

            If S8%(P9, 1) > 0 Or (P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) < 3 And P8%(P9, P1%(P9), 2) = 0) Then
                Print pitchers$(P9, P1%(P9)); " seems to be OK!!";
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

    player$(4) = batters$(D, B9%(D, 5))

    Call PBP("grounder to " + player$(4) + " @ 3B")
    Call PBP(player$(2) + " OUT @ 3B")
    Call PBP(player$(1) + " OUT @ 2B")
    Call PBP(player$(0) + " OUT @ 1B")
    Call PBP("--- TRIPLE PLAY !!! ---"): Call DELAY

    nbrOuts(0) = 3
    I5 = P1%(D)
    I6 = 0

    For idx = 1 To 3: Call ADDPIT(D, I5, I6): Next idx

    If B7%(P, B1!(P)) = 1 Then
        I5 = P1%(P)
        I6 = 23
        Call INCPITOFF(P, I5, I6)
    Else
        I5 = B3%(P, B1!(P))
        I6 = 0
        Call INCBATOFF(P, I5, I6)
    End If

End Sub


'----------------------------------------
'       TRYFOREXTRABASE Subroutine
'----------------------------------------
'This subroutine handles all the logic and
'Play-by-Play (relying on other rouintes)
'for an attempt to steal base.
Sub TRYFOREXTRABASE (LN%, S2%, P2, Z2%, D, gameW%, sah%, SB%, baseNbr!, I9)

    If LN% = 3308 Then
        'This happens very rarely
        LN% = 0

    Else

        baseNbr! = 0
        Call d100(H6%)

        'Determine which base we're stealing
        If S2% = 8 And gameB0%(2, 0) > 0 And H6% < 6 And gameB0%(3, 0) = 0 And batRating(P, B3%(P, gameB0%(2, 0)), 8) > 0 Then baseNbr! = 3
        If S2% = 9 And gameB0%(3, 0) > 0 And H6% <= 2 And batRating(P, B3%(P, gameB0%(3, 0)), 9) > 0 Then baseNbr! = 4
        If S2% = 7 And gameB0%(3, 0) > 0 And P2 = 2 And H6% >= 6 And H6% <= batRating(P, B3%(P, gameB0%(3, 0)), 16) Then baseNbr! = 4
        If S2% = 7 And gameB0%(3, 0) > 0 And Z2% = gameB0%(3, 0) And P2 <> 2 And H6% <= batRating(P, B3%(P, gameB0%(3, 0)), 16) * 12 Then baseNbr! = 4

        E2% = 10

        If gameB0%(2, 0) > 0 Then E2% = E2% + batRating(P, B3%(P, gameB0%(2, 0)), 16)
        If S2% = 7 And gameB0%(2, 0) > 0 And P2 <> 2 And H6% <= E2% And gameB0%(3, 0) = 0 And baseNbr! = 0 Then baseNbr! = 3
        If S2% = 7 And gameB0%(1, 0) > 0 And gameB0%(2, 0) = 0 And baseNbr! = 0 And H6% <= batRating(P, B3%(P, gameB0%(1, 0)), 16) And batRating(P, B3%(P, gameB0%(1, 0)), 7) > 0 Then baseNbr! = 2

    End If

    If baseNbr! <> 0 Then
        'We shouldn't still have a 0 value!

        If Not (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            If S2% < 10 Then Call PBP(player$(baseNbr! - 1) + " try for " + baseName$(baseNbr!) + "? (Y/N) ")
            If S2% > 10 Then Call PBP(player$(baseNbr! - 1) + " tag up? (Y/N) ")
            Call PBP("against " + player$(4))
        End If

        I$ = "N"
        A1 = S6%(P, 0) - S6%(D, 0)

        If (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            If A1 > -2 And A1 < 2 And S2% <> 19 Then
                I$ = "Y"
            Else
                If A1 < 0 And (INNING% >= 7 Or S2% = 19) Then
                    I$ = "N"
                End If
            End If

            If S2% = 8 And nbrOuts(0) = 2 And baseNbr! = 3 Then
                I$ = "N"
            End If
        End If

        skipThrow% = 0
        If Not (playerMode = 2 Or playerMode = 1 And P = compTeam) Then
            I$ = GetKeyPress$
        End If

        If UCase$(I$) <> "Y" Then
            I9 = 1
        Else
            pbpString$ = player$(baseNbr! - 1) + " is "

            If S2% < 10 Then
                Call PBP(pbpString$ + "going for " + baseName$(baseNbr!))
            Else
                'S2% >= 10
                Call PBP(pbpString$ + "tagging")
            End If

            If Not (S2% = 7 And baseNbr! = 2 Or S2% > 10 And S4% = 3) Then

                If Not (playerMode = 2 Or playerMode = 1 And D = compTeam) Then
                    Call PBP(player$(4) + " throw to " + baseName$(baseNbr!) + "? (Y/N) ")
                End If

                Call d100(RN)

                If (playerMode = 2 Or playerMode = 1 And D = compTeam) Then
                    If MG%(D, 766) = 999 And baseNbr! = 4 Then
                        If INNING% >= MG%(D, 767) And MG%(D, 767) <> 0 And MG%(D, 768) <= S6%(D, 0) - S6%(P, 0) Then I$ = "N"
                        If INNING% >= MG%(D, 769) And MG%(D, 769) <> 0 And MG%(D, 770) <= S6%(D, 0) - S6%(P, 0) Then I$ = "N"
                    Else
                        I$ = "Y"
                    End If
                Else
                    I$ = GetKeyPress$
                End If

                I9 = 0

                If UCase$(I$) <> "Y" Then
                    I9 = 1
                    Call PBP("No throw is made")
                    skipThrow% = 1
                End If

            End If

            If skipThrow% = 0 Then

                Call WINDEX
                Call PBP("Here comes the throw by " + player$(4))
                'For i = 1 To 3: Call PBP("."): Next i: Call PBP(" ")
                Call PBP("...")

                Call FLDERR(F%, gameW%, D, I3, SB%, S2%, P)

                If F% <> 1 Then

                    I4 = Int(Rnd(1) * 1000) + 1
                    A = batRating(P, B3%(P, gameB0%(baseNbr! - 1, 0)), 16)

                    If B7%(P, gameB0%((baseNbr! - 1), 0)) = 1 Then A = pitchRating(P, P1%(P), 31)
                    A = A - 5
                    A = A * 50

                    If S2% = 17 Then b = ((batRating(D, B9%(D, gameW%), 15) - 5) * 25): I3 = 1000 + A - b: ' WAS 800
                    If S2% = 18 Then b = ((batRating(D, B9%(D, gameW%), 15) - 5) * 25): I3 = 800 + A - b: ' WAS 650
                    If S2% = 19 Or S2% < 10 Then b = ((batRating(D, B9%(D, gameW%), 15) - 5) * 25): I3 = 500 + A - b

                    If I4 > I3 Then

                        A5%(baseNbr! - 1) = 11
                        nbrOuts(0) = nbrOuts(0) + 1
                        nbrOuts(1) = nbrOuts(1) + 1
                        I5 = P1%(D)
                        I6 = 0
                        Call ADDPIT(D, I5, I6)
                        Call d100(RN)

                        Select Case RN
                            Case 1 To 4
                                If baseNbr! = 4 Then
                                    Call PBP(player$(baseNbr! - 1) + " collides with " + batters$(D, B9%(D, 2))): Call PBP(batters$(D, B9%(D, 2)) + " hangs on!!")
                                End If

                            Case 5 To 50
                                If baseNbr! <> 4 Then Call PBP(player$(baseNbr! - 1) + " doesn't make it!")

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

                        Call CREDITASSIST(gameW%, D)
                        Call ADVANCEMENT(P, D, S2%, F%, I3)
                        Call CREDITPUTOUT(2, FB2P%, AST%, D)
                        gameW% = 2

                        If nbrOuts(0) = 3 Or S2% > 10 Then Exit Sub

                        3371 '
                        Call d100(I5)

                        A1 = S6%(P, 0) - S6%(D, 0)

                        If Not (A1 < -2 Or A1 < 0 And INNING% > 8) Then

                            ' *** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
                            If baseNbr! - 1 > 2 And I5 <= 15 And gameB0%(2, 0) > 0 Then
                                Call CREDITASSIST(gameW%, D)
                                Call PBP("runner is trying for 3rd...")
                                A5%(2) = 11
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                gameW% = 5
                                Call CREDITREGPO(gameW%, D)
                                I5 = P1%(D)
                                I6 = 0
                                Call ADDPIT(D, I5, I6)
                            End If

                            ' *** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***
                            Call d100(I5)

                            If baseNbr! - 1 > 1 And I5 <= 15 And gameB0%(1, 0) > 0 And A5%(2) <> 11 Then
                                Call CREDITASSIST(gameW%, D)
                                gameW% = 6
                                Call CREDITREGPO(gameW%, D)
                                Call PBP("runner is trying for 2nd...")
                                A5%(1) = 11
                                nbrOuts(0) = nbrOuts(0) + 1
                                nbrOuts(1) = nbrOuts(1) + 1
                                I5 = P1%(D)
                                I6 = 0
                                Call ADDPIT(D, I5, I6)
                            End If

                            Call d100(I5)

                            If gameB0%(2, 0) > 0 And baseNbr! - 1 > 2 And A5%(2) <> 11 And I5 > (80 - batRating(P, B3%(P, gameB0%(2, 0)), 16) * 2) And I9 = 0 Then
                                A5%(2) = 1
                                Call PBP("runner is trying for 3rd...")
                                Call PBP("He's safe!")
                            End If

                            I5 = Int(Rnd(1) * 10) + 1

                            If gameB0%(1, 0) > 0 And A5%(1) <> 11 And (S4% = 4 Or S4% = 7 And baseNbr! - 1 > 1 And I5 > (10 - batRating(P, B3%(P, gameB0%(1, 0)), 16)) And I9 = 0) Then
                                A5%(1) = 1
                                Call PBP("runner is trying for 2nd...")
                                Call PBP("He's in there!")
                            Else

                                Call d100(I5)

                                If gameB0%(1, 0) > 0 And baseNbr! - 1 > 1 And I5 > (80 - batRating(P, B3%(P, gameB0%(1, 0)), 16) * 2) And I9 = 0 And A5%(1) <> 11 Then
                                    A5%(1) = 1
                                    Call PBP("runner safe at 2nd on the throw")
                                End If

                            End If

                        End If

                        Call ADVANCEMENT(P, D, S2%, F%, I3)
                        I9 = 0

                        Exit Sub

                    End If

                End If

                A5%(baseNbr! - 1) = 1

                If I9 <> 1 Then

                    Call d100(RN)

                    Select Case RN
                        Case 1 To 4:
                            If baseNbr! = 4 Then
                                Call PBP(player$(baseNbr! - 1) + " collides with " + batters$(D, B9%(D, 2)))
                                Call PBP("He knocks the ball loose!!")
                            End If

                        Case 5 To 50:
                            If baseNbr! <> 4 Then Call PBP(player$(baseNbr! - 1) + " beats the throw!")

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
                If baseNbr! = 4 Then sah% = 1

                Call TAGADVANCE(S2%, baseNbr!, D)

                If S2% < 10 Then 3371

                If S2% > 10 Then

                    If B7%(P, B1!(P)) = 1 Then

                        P8%(P, P1%(P), 23) = P8%(P, P1%(P), 23) - 1
                        P5%(P, P1%(P), 23) = P5%(P, P1%(P), 23) - 1
                        I5 = P1%(P)
                        I6 = 41
                        Call INCPITOFF(P, I5, I6)
                        I5 = P1%(D)
                        I6 = 16
                        Call ADDPIT(D, I5, I6)

                    Else

                        B8%(P, B3%(P, B1!(P)), 0) = B8%(P, B3%(P, B1!(P)), 0) - 1
                        B5%(P, B3%(P, B1!(P)), 0) = B5%(P, B3%(P, B1!(P)), 0) - 1

                        If pitchRating(D, P1%(D), 0) <> 1 Then
                            CSS%(P, B3%(P, B1!(P)), 0) = CSS%(P, B3%(P, B1!(P)), 0) - 1
                        End If

                        I5 = B3%(P, B1!(P))
                        I6 = 19
                        Call INCBATOFF(P, I5, I6)

                        I5 = P1%(D)
                        I6 = 16
                        Call ADDPIT(D, I5, I6)

                        GoTo 3371

                    End If

                End If

                'Loop Until S2% >= 10 And B7%(P, B1!(P)) = 1

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

    If useVGA = 1 Then Screen 0

    Do
        Cls
        Color 12: Print Tab(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:"
        Print
        Print
        Color 14, 0: Print "(0) ";: Color tmRat_GAME(0, 11), tmRat_GAME(0, 12): Print gameTeams$(0)
        Print
        Color 14, 0: Print "(1) ";: Color tmRat_GAME(1, 11), tmRat_GAME(1, 12): Print gameTeams$(1)
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
                            If batRating(WK, idx, 21) = 0 Then Color 11
                            Locate idx + 5, 5: Print Using "##"; idx;
                            Print " "; batters$(WK, idx); Tab(40);
                            Color 3
                            If batRating(WK, idx + 12, 21) = 0 Then Color 11
                            Print Using "##"; idx + 12;
                            Print " "; batters$(WK, idx + 12)
                            Color 3
                        Next

                        If batRating(WK, 11, 21) = 0 Then Color 11

                        Locate , 5: Print Using "##"; 11;: Print " "; batters$(WK, 11)

                        Color 3

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-22) (23 TO CONTINUE) ";: Input M
                        Loop Until M >= 0 And M <= 23

                        If M <> 23 Then

                            Locate 3, 3
                            Color tmRat_GAME(WK, 11), tmRat_GAME(WK, 12): Print " "; Chr$(201); String$(71, 205); Chr$(187); " "

                            For X = 1 To 7: Locate , 3: Print " "; Chr$(186); String$(71, 32); Chr$(186); " ": Next

                            Locate , 3
                            Print " "; Chr$(200); String$(71, 205); Chr$(188); " "

                            Locate 4, 6: Print "Stats for "; batters$(WK, M)
                            Locate , 6: Print "BATS: "; B1$(batRating(WK, M, 0) + 2); "    ";: Print Using "G/A%: ##       BUNT: #       RUN: #"; batRating(WK, M, 18); batRating(WK, M, 30); batRating(WK, M, 16)
                            Locate , 3: Print " "; Chr$(204); String$(71, 205); Chr$(185); " "
                            Locate , 6: Print "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"

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

                            If pitchRating(WK, idx, 15) = 0 Then Color 11

                            Locate idx + 5, 5: Print Using "##"; idx;
                            Print " "; pitchers$(WK, idx); Tab(40);

                            Color 3

                            If pitchRating(WK, idx + 11, 15) = 0 Then Color 11

                            Print Using "##"; idx + 11;
                            Print " "; pitchers$(WK, idx + 11)

                            Color 3

                        Next

                        Do
                            Color 15
                            Locate 20, 1: Print "# TO VIEW (0-21) (22 TO CONTINUE) ";: Input M
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
Sub WALLDOUBLE (HW%, gameW%)

    Call d100(BV)
    Call d100(RN)

    Select Case BV

        Case 1 To 40:

            If HW% = 0 Then pbpString$ = "It's in the gap and to the wall"

        Case 41 To 65

            Select Case RN
                Case 1 To 25
                    If (tmRat_GAME(1, 13) = 8 Or tmRat_GAME(1, 13) = 40) Then pbpString$ = "It's off the ivy!"
                Case 26 To 50
                    If tmRat_GAME(1, 13) = 21 And gameW% = 7 Then pbpString$ = "It hits off the Green Monster!"
            End Select

            If tmRat_GAME(1, 13) = 27 And gameW% = 7 Then
                pbpString$ = "It hits off the screen!"
            Else
                pbpString$ = "It's off the wall"
            End If

        Case 66 To 85

            If HW% = 0 Then
                If gameW% = 8 Then
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
            If RN <= 25 And tmRat_GAME(1, 13) = 21 Then
                pbpString$ = "It's off the Green Monster!"
            Else
                If tmRat_GAME(1, 13) = 27 And gameW% = 7 Then
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
Sub WHICHRUNNER (i, S4%, D5)

    If i = 4 Then A5%(3) = 1

    If S4% = 4 Then

        Call PBP("WHICH RUNNER DO YOU WANT TO GO AFTER")

        Call PBP("1 - " + batters$(P, B3%(P, gameB0%(2, 0))))

        If B7%(P, gameB0%(1, 0)) <> 1 Then
            Call PBP("2 - " + batters$(P, B3%(P, gameB0%(1, 0))))
        Else
            Call PBP("2 - " + pitchers$(P, P1%(P)))
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
            I2 = B3%(P, gameB0%(1, 0))
            If B7%(P, gameB0%(1, 0)) = 1 Then player$(i - 1) = pitchers$(P, P1%(P))
        End If

        D5 = 0

    End If

End Sub


'----------------------------------------
'          WHOSON1ST Subroutine
'----------------------------------------
' ...explanation...
Sub WHOSON1ST (S4%)

    S4% = 0
    I1 = gameB0%(1, 0)
    I2 = gameB0%(2, 0)
    I3 = gameB0%(3, 0)

    If I1 > 0 Then
        If I2 = 0 Then
            If I3 = 0 Then
                S4% = 1
            Else
                ' *** 1ST AND 3RD ***
                S4% = 5
            End If
        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** 1ST AND 2ND ***
                S4% = 4
            Else
                'I3 > 0
                ' *** BASES LOADED ***
                S4% = 7
            End If
        End If
    Else
        'I1 = 0
        If I2 = 0 Then
            If I3 > 0 Then
                ' *** RUNNER ON 3RD ***
                S4% = 3
            End If

        Else
            'I2 > 0
            If I3 = 0 Then
                ' *** RUNNER ON 2ND ***
                S4% = 2
            Else
                'I3 > 0
                ' *** 2ND AND 3RD ***
                S4% = 6
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

    Call d100(RN)

    If RN <= 50 Then
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
        Call d100(H6%)

        Select Case H6%

            Case 1

                Call PBP(batters$(D, B9%(D, 2)) + " is arguing ball and strike calls")
                Call PBP(gameUmpire$(1) + " has heard enough")
                Call PBP(batters$(D, B9%(D, 2)) + " has been ejected!!")

                X$ = batters$(D, B9%(D, 2))

                Call EJECTINC(ejectCnt, X$)
                Call DELAY

                po% = 2

                Call EJECTIONINJURY(playerMode, P, compTeam, D, po%, P9, W5, W6)

            Case 2 To 3

                Call PBP(pitchers$(D, P1%(D)) + " steps off the mound")
                Call PBP("after that last call")
                Call PBP("He is shouting at " + gameUmpire$(1))
                Call PBP(gameUmpire$(1) + " does not appreciate what he hears...")
                Call PBP(pitchers$(D, P1%(D)) + " has been ejected!!")

                X$ = pitchers$(D, P1%(D))

                Call EJECTINC(ejectCnt, X$)
                Call DELAY
                Call INJPIT(P, D, IP%, PPH)

            Case 4
                If B7%(P, B1!(P)) <> 1 Then
                    Call PBP(player$(0) + " is unhappy with that last call"): Call PBP("He is venting his opinion to " + gameUmpire$(1)): Print "Thats it!! "; gameUmpire$(1); " has ejected "; player$(0); "!!"
                    X$ = player$(0)
                    Call EJECTINC(ejectCnt, X$)
                    Call DELAY
                    Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)
                Else
                    Call PBP(player$(0) + " did not like that"): Call PBP("last call at all!!")
                End If

            Case 5

                Call PBP(player$(0) + " hits a slow roller to first")
                Call PBP(batters$(D, B9%(D, 3)) + " fields it cleanly")
                Call PBP("He flips it to " + pitchers$(D, P1%(D)) + " covering just in time!")
                Call PBP("Wait... " + gameUmpire$(2) + " rules safe!!")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                For idx = 0 To 3: A5%(idx) = 1: Next idx

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                Call PBP(gameManagers$(D) + " charges from the dugout")
                Call PBP(gameManagers$(D) + ", " + pitchers$(D, P1%(D)) + ",")
                Call PBP(batters$(D, B9%(D, 2)) + ", and " + batters$(D, B9%(D, 3)))
                Call PBP("are going at it with " + gameUmpire$(2))
                Call DELAY
                Call DELAY

                Call PBP(gameUmpire$(3) + " and " + gameUmpire$(1) + " come in to help out")
                Call PBP("This argument is out of control now!!")
                Call PBP("What is this?!")
                Call PBP("All of them have been ejected!!!")

                X$ = pitchers$(D, P1%(D))

                Call EJECTINC(ejectCnt, X$)

                X$ = batters$(D, B9%(D, 2))

                Call EJECTINC(ejectCnt, X$)

                X$ = batters$(D, B9%(D, 3))

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

                Call PBP(gameUmpire$(1) + " has just ejected " + pitchers$(D, P1%(D)))
                Call PBP("for wetting the ball!!")
                Call EJECTINC(ejectCnt, X$)

                ejections$(ejectCnt) = pitchers$(D, P1%(D))

                Call DELAY
                Call INJPIT(P, D, IP%, PPH)

            Case 9

                Call PBP("Here's the 3-2 pitch...")
                Call PBP("Time called...")
                Call PBP(gameUmpire$(1) + " is calling it ball 4")
                Call PBP("He says " + pitchers$(D, P1%(D)) + " went to his mouth")

            Case 10 To 15

                Call PBP(player$(0) + " hits the deck on a high")
                Call PBP("and tight pitch from " + pitchers$(D, P1%(D)))
                Call PBP(player$(0) + " charges the mound!")
                Call PBP("Both dugouts have emptied...")
                Call PBP("It's a bench-clearing brawl!")
                Call WINDEX
                Call PBP("It will take a few minutes to clean this up")

                RN = Int(Rnd(1) * 3) + 1

                Select Case RN

                    Case 1:
                        Call PBP(pitchers$(D, P1%(D)) + " has been ejected")
                        X$ = pitchers$(D, P1%(D))
                        Call EJECTINC(ejectCnt, X$)
                        Call INJPIT(P, D, IP%, PPH)

                    Case 2:
                        Call PBP(player$(0) + " has been ejected")
                        X$ = player$(0)
                        Call EJECTINC(ejectCnt, X$)
                        Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                    Case 3:
                        Call PBP(player$(0) + " and " + pitchers$(D, P1%(D)) + " have been ejected")
                        X$ = player$(0)
                        Call EJECTINC(ejectCnt, X$)
                        X$ = pitchers$(D, P1%(D))
                        Call EJECTINC(ejectCnt, X$)
                        Call INJPIT(P, D, IP%, PPH)
                        Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                End Select

            Case 16 To 21

                H6% = 21

                Call WINDEX

                If rainyDay = 0 And (precipType = 0 Or precipType = 4) Then
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
                        For idx = 0 To 1: S8%(idx, 0) = S8%(idx, 0) - 2: Next
                    End If

                End If

            Case 22 To 24

                Select Case s4%
                    Case 0
                        If tmRat_GAME(1, 13) = 14 Then
                            Call PBP(player$(0) + " steps out of the box as a jet passes over")
                        Else
                            Call PBP(gameUmpire$(1) + " calls time to get some new baseballs")
                        End If

                    Case 1
                        Call PBP(batters$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("1st...")
                        i = 2

                    Case 2, 4
                        Call PBP(batters$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("2nd base...")
                        i = 3

                    Case 3
                        Call PBP(batters$(D, B9%(D, 2)) + " rifles the ball to ")
                        Call PBP("3rd base...")
                        i = 4

                End Select

                If s4% <> 0 Then

                    Call PBP(player$(i - 1) + " is picked off base!")

                    A5%(i - 1) = 10
                    nbrOuts(0) = nbrOuts(0) + 1
                    nbrOuts(1) = nbrOuts(1) + 1
                    gameW% = 2

                    Call CREDITASSIST(gameW%, D)
                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call CREDITPUTOUT(I3, FB2P%, AST%, D)

                    I5 = P1%(D)
                    I6 = 0

                    Call ADDPIT(D, I5, I6)

                    If nbrOuts(0) >= 3 Then B1!(P) = B1!(P) - 1: H6% = 5

                End If

            Case 25 To 29

                Call PBP(player$(0) + " pops it up near the 3rd base stands")
                Call PBP("A fan interferes...")
                Call PBP(batters$(D, B9%(D, 6)) + " makes a great catch!!")

                gameW% = 6
                Call CREDITREGPO(gameW%, D)

                nbrOuts(0) = nbrOuts(0) + 1
                nbrOuts(1) = nbrOuts(1) + 1
                Call DELAY

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6)

                I5 = P1%(D)
                I6 = 0
                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) = 1 Then
                    I5 = P1%(P)
                    I6 = 23
                    Call INCPITOFF(P, I5, I6)
                End If

                Call DELAY

            Case 30

                Call PBP(player$(0) + " pops it up foul near the stands")
                Call PBP(batters$(D, B9%(D, 2)) + " runs into the stands!!")

                po% = 2

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 30 To 31

                Call PBP(player$(0) + " pops it up foul near the stands")
                Call PBP(batters$(D, B9%(D, 3)) + " gives chase")
                Call PBP("he runs into the stands!!")

                po% = 3

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 32

                Call PBP(player$(0) + " pops it up foul down the rf line")
                Call PBP(batters$(D, B9%(D, 4)) + " crashes into the wall")

                po% = 4

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 33

                Call PBP(player$(0) + " pops it up foul by")
                Call PBP("the 3rd base dugout")
                Call PBP(batters$(D, B9%(D, 5)) + " falls in trying to make the catch!!")

                po% = 5

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 34

                Call PBP(player$(0) + " pops it up foul")
                Call PBP("down the lf line")
                Call PBP(batters$(D, B9%(D, 6)) + " tumbles over the railing")
                Call PBP("trying to make the catch!")
                Call DELAY

                po% = 6

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 35 To 37

                Call PBP(player$(0) + " lines one to deep left field")
                Call PBP(batters$(D, B9%(D, 7)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                op% = 7

                Call PBP(batters$(D, B9%(D, 8)) + " comes over and gets the ball in")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For idx = 0 To 3: A5%(idx) = 2: Next idx

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 38 To 40

                Call PBP(player$(0) + " drives a sinking liner")
                Call PBP("into deep center")
                Call PBP(batters$(D, B9%(D, 8)) + " gets a good jump on it")
                Call PBP("He crashes into the wall and")
                Call PBP("the ball bounces away!!")
                Call PBP("The runners are circling the bases")
                Call PBP(batters$(D, B9%(D, 9)) + " retrieves the ball")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                I6 = 14

                Call ADDPIT(D, I5, I6)

                gameW% = 8

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 6: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 29: Call INCPITOFF(P, I5, I6)

                A5%(0) = 4
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("It's an inside the park HOME RUN!!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 8

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 41 To 43

                Call PBP(player$(0) + " lifts a deep drive into right")
                Call PBP(batters$(D, B9%(D, 9)) + " races back...")
                Call PBP("He slams into the wall and is down!!")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                gameW% = 9

                Call PBP(batters$(D, B9%(D, 8)) + " comes over to get the ball back in")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 5: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 28: Call INCPITOFF(P, I5, I6)

                A5%(0) = 3
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("Triple!!")
                Call DELAY
                Call SCOREBOARD
                Call WINDEX

                po% = 9

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 44

                I5 = P1%(D)
                I6 = 1
                Call ADDPIT(D, I5, I6)

                gameW% = Int(Rnd(1) * 3) + 7

                Call PBP(player$(0) + " hits a sinking liner into " + fieldPos$(gameW%))
                Call PBP(batters$(D, B9%(D, gameW%)) + " dives for the catch...")
                Call PBP("the ball rolls free!")
                Call PBP(batters$(D, B9%(D, gameW% - 1)) + " gets the loose ball in")

                op% = gameW%

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For idx = 0 To 3: A5%(idx) = 2: Next idx

                A5%(3) = 1
                A5%(1) = 3

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = op%

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 45

                Call PBP(player$(0) + " is hit by a pitch and injured")
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

            Case 46 To 49

                Call PBP(batters$(D, B9%(D, 2)) + " is injured by a foul tip!")

                po% = 2

                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 50 To 58

                i = Int(Rnd(1) * 3) + 7

                If i = 9 Then hi1 = 3: hi2 = 9: hi3 = 8
                If i = 8 Then hi1 = 4: hi2 = 8: hi3 = 7
                If i = 7 Then hi1 = 6: hi2 = 7: hi3 = 8

                Call PBP(player$(0) + " hits a weak fly into short " + fieldPos$(i))
                Call PBP(batters$(D, B9%(D, hi1)) + " goes out")
                Call PBP(batters$(D, B9%(D, hi2)) + " is charging in")
                Call PBP("They collide and the ball rolls free!!")
                Call PBP(batters$(D, B9%(D, hi3)) + " gets it back in")

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)

                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                For idx = 0 To 3: A5%(idx) = 2: Next idx

                A5%(3) = 1
                A5%(1) = 3
                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call SCOREBOARD
                Call WINDEX

                po% = hi1
                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                po% = hi2
                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 5

            Case 59 To 61
                Call PBP(player$(0) + " fouls the pitch off his foot")
                Call PBP("He is injured")
                Call PLYRHRT(P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)

                H6% = 1

                '62 to 64
            Case 62 To 64

                Call PBP(player$(0) + " pops it up in foul along the 3b line"): Call PBP("It drifts into foul territory")
                Call PBP(batters$(D, B9%(D, 5)) + " and " + pitchers$(D, P1%(D)) + " chase after it and collide!!")
                Call PBP(batters$(D, B9%(D, 5)) + " is down and injured")

                po% = 5
                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                H6% = 1

            Case 65 To 70

                I5 = P1%(D)
                I6 = 1

                Call ADDPIT(D, I5, I6)

                gameW% = 8

                Call LONGFLY(D, gameW%)
                Call PBP(batters$(D, B9%(D, 7)) + " collides with " + batters$(D, B9%(D, 8)) + "!!")
                Call PBP("both fielders are down... ")
                Call PBP(batters$(D, B9%(D, 6)) + " gets the ball back in")

                If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6): I6 = 4: Call INCBATOFF(P, I5, I6)
                If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6): I6 = 27: Call INCPITOFF(P, I5, I6)

                A5%(0) = 2
                A5%(1) = 3
                A5%(2) = 2
                A5%(3) = 1

                Call ADVANCEMENT(P, D, S2%, F%, I3)
                Call PBP("Double!!")
                Call SCOREBOARD
                Call WINDEX

                po% = 7
                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6)

                po% = 8
                Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6): H6% = 5

            Case 71 To 85

                pbpString$ = player$(0) + " lines it off "

                i = Int(Rnd(1) * 5) + 1

                If i = 1 Then
                    pbpString$ = pbpString$ + "the leg of " + pitchers$(D, P1%(D))
                    Call PBP(pbpString$)

                    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)

                    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                    I5 = P1%(D)
                    I6 = 1

                    Call ADDPIT(D, I5, I6)

                    For idx = 0 To 3: A5%(idx) = 1: Next

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP("He reaches on a single")
                    Call PBP(pitchers$(D, P1%(D)) + " is down and in pain!")
                    Call INJPIT(P, D, IP%, PPH)
                    H6% = 5
                Else
                    'i = 2 to 5
                    po% = i + 1
                    pbpString$ = pbpString$ + "of " + batters$(D, B9%(D, po%)) + " for a single"

                    Call PBP(pbpString$)
                    Call DELAY

                    If B7%(P, B1!(P)) <> 1 Then I5 = B3%(P, B1!(P)): I6 = 0: Call INCBATOFF(P, I5, I6): I6 = 1: Call INCBATOFF(P, I5, I6)
                    If B7%(P, B1!(P)) = 1 Then I5 = P1%(P): I6 = 23: Call INCPITOFF(P, I5, I6): I6 = 24: Call INCPITOFF(P, I5, I6)

                    I5 = P1%(D)
                    I6 = 1

                    Call ADDPIT(D, I5, I6)

                    For idx = 0 To 3: A5%(idx) = 1: Next

                    Call ADVANCEMENT(P, D, S2%, F%, I3)
                    Call SCOREBOARD
                    Call WINDEX
                    Call PBP(batters$(D, B9%(D, po%)) + " is down and injured")
                    Call INJURYLENGTH(IJL%, D, po%, playerMode, P, compTeam, P9, W5, W6): H6% = 5

                End If

            Case 86 To 89

                Call PBP(pitchers$(D, P1%(D)) + " is motioning for " + gameManagers$(D) + " to come out")
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
        Screen 0
    End If

    Call FRAMEOPTION(5, 1)

    If tmRat_GAME(1, 9) = 1 Then
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
    Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
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

    If batRating(P9, I, 21) >= 98 Then Color 0, 7

End Sub


'----------------------------------------
'         GETWEATHER Subroutine
'----------------------------------------
'This subroutine opens the PARKS.DBF file
'to read in attendance data, wind factor,
'weather and other ballpark calculations.
Sub GETWEATHER (windDir, windSpeed, gameMonth, WF!, skyType, gameTemp, rainyDay)

    Shared avgAttendance&

    If tmRat_GAME(1, 13) > 82 Then
        ID = Int(Rnd(1) * 79) + 1
    Else
        ID = tmRat_GAME(1, 13)
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

    Call d100(RN0)

    If RN0 <= 50 Then
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

    Call d100(RN0)

    If RN0 <= MON(SM%, 14) Then

        precipType = Int(Rnd(1) * 3) + 1
        skyType = 3

        Call d100(RN)
        If RN <= 5 Then
            rainyDay = 1
            rainStartInning = Int(Rnd(1) * 3) + 6
        End If

    End If

    If tmRat_GAME(1, 13) = 2 Or tmRat_GAME(1, 13) = 4 Or tmRat_GAME(1, 13) = 10 Or tmRat_GAME(1, 13) = 13 Or tmRat_GAME(1, 13) = 24 Then precipType = 4: skyType = 4

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
    Loop Until K1 >= 1 And K1 <= 9 Or B7%(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or B7%(P9, K2) <> 1

    Swap B7%(P9, K2), B7%(P9, K1)
    Swap B3%(P9, K1), B3%(P9, K2)

    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)

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

        Call POSITION(I)

        Print Using "# "; I;

        If B7%(P9, I) <> 1 Then
            Print batters$(P9, B3%(P9, I));
        Else
            If I5 <> 66 Then Print pitchers$(P9, B3%(P9, I));
        End If

        Print Tab(74);

        Print fieldPos$(B7%(P9, I));

    Next I

    I5 = 0

End Sub


'----------------------------------------
'          POSITION Subroutine
'----------------------------------------
' This subroutine "positions" the cursor
'on the screen at the appropriate row
'in order to print out the line positions.
Sub POSITION (I)

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
    Loop Until K1 >= 1 And K1 <= 9 Or B7%(P9, K1) <> 1

    Locate 19, 59: Print "B.O. PLYR 2 (1-9)";

    Do
        I$ = GetKeyPress$
        K2 = Val(I$)
    Loop Until K2 >= 1 And K2 <= 9 Or B7%(P9, K2) <> 1

    Swap B7%(P9, K2), B7%(P9, K1)

    B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
    B9%(P9, B7%(P9, K2)) = B3%(P9, K2)

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
    Color tmRat_GAME(P9, 11), tmRat_GAME(P9, 12): Print gameTeams$(P9);

    Color 14, 0

    Locate , 15: Print "B  POS  AB  2B 3B HR  BB  SO SRA SB G%  BA"

    Locate 2

    For I = 0 To 22: Locate , 1

        If batters$(P9, I) = "XXX" Or Left$(batters$(P9, I), 1) = " " Or batters$(P9, I) = "" Then
            batRating(P9, I, 21) = 98
        Else
            Color 2, 0

            If I < 10 Then Print Using "#"; I;
            If I > 9 Then Print alpha$(I - 10);

            Color 15, 0
            Call COLCHK(P9, I)

            'Name / Position
            Locate , 3: Print batters$(P9, I);
            Color 15, 0
            Locate , 15: Print B1$(batRating(P9, I, 0) + 2);

            Locate , 18: If batRating(P9, I, 22) <> -1 Then Print Using "#"; batRating(P9, I, 22);
            Locate , 19: If batRating(P9, I, 23) <> -1 Then Print Using "#"; batRating(P9, I, 23);
            Locate , 20: If batRating(P9, I, 24) <> -1 Then Print Using "#"; batRating(P9, I, 24);
            Locate , 21: If batRating(P9, I, 25) <> -1 Then Print Using "#"; batRating(P9, I, 25);

            Locate , 23

            Print Using "### ## ## ## ### ###"; batRating(P9, I, 4); batRating(P9, I, 7); batRating(P9, I, 8); batRating(P9, I, 9); batRating(P9, I, 11); batRating(P9, I, 12);

            Locate , 44: Print Using "#"; batRating(P9, I, 16);: Print Using "#"; batRating(P9, I, 34);: Print Using "#"; batRating(P9, I, 33);

            Locate , 47
            '           SB   G%
            Print Using "### ##"; batRating(P9, I, 13); batRating(P9, I, 18);

            'BA
            If batRating(P9, I, 6) = batRating(P9, I, 4) And batRating(P9, I, 6) <> 0 Then
                Print "1.000";
            Else
                If batRating(P9, I, 4) = 0 Then
                    Print " .---";
                Else
                    bAvg! = batRating(P9, I, 6) / batRating(P9, I, 4)
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
            Locate , 16: If batRating(P9, I, 22) <> -1 Then Print Using "# #### # #"; batRating(P9, I, 22); batRating(P9, I, 26); batRating(P9, I, 33); batRating(P9, I, 34);
            Locate , 28: If batRating(P9, I, 23) <> -1 Then Print Using "# #### # #"; batRating(P9, I, 23); batRating(P9, I, 27); batRating(P9, I, 35); batRating(P9, I, 36);
            Locate , 40: If batRating(P9, I, 24) <> -1 Then Print Using "# #### # #"; batRating(P9, I, 24); batRating(P9, I, 28); batRating(P9, I, 37); batRating(P9, I, 38);
            Locate , 52: If batRating(P9, I, 25) <> -1 Then Print Using "# #### # #"; batRating(P9, I, 25); batRating(P9, I, 29); batRating(P9, I, 39); batRating(P9, I, 40);
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

            Locate , 20: Print Using "### ## ## ## ### ### "; batRating(P9, I, 42); batRating(P9, I, 44); batRating(P9, I, 45); batRating(P9, I, 46); batRating(P9, I, 47); batRating(P9, I, 48);

            If batRating(P9, I, 42) = 0 Or batRating(P9, I, 43) = 0 Then
                Print " .000";
            Else
                If batRating(P9, I, 42) = batRating(P9, I, 43) Then
                    Print "1.000";
                Else
                    Print Using " .###"; batRating(P9, I, 43) / batRating(P9, I, 42);
                End If
            End If

            Locate , 53: Print Using "### ## ## ## ### ### "; batRating(P9, I, 49); batRating(P9, I, 51); batRating(P9, I, 52); batRating(P9, I, 53); batRating(P9, I, 54); batRating(P9, I, 55);

            'AVG
            If batRating(P9, I, 49) = 0 Or batRating(P9, I, 50) = 0 Then
                Print " .000";
            Else
                If batRating(P9, I, 49) = batRating(P9, I, 50) Then
                    Print "1.000";
                Else
                    Print " ."; Using "###"; batRating(P9, I, 50) / batRating(P9, I, 49);
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
        Screen 0
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
            'Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
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
'          GETSTATS Subroutine
'----------------------------------------
'This subroutine loads in stats for the
'each team
Sub GETSTATS ()

    For I = 0 To 1

        Color 15, 0
        Cls

        statFile$ = nameStatFile$(gameTeams$(I))

        If _FileExists(diskPaths$(1) + statFile$) Then

            Open diskPaths$(1) + statFile$ For Input As #1

            Input #1, A$
            Input #1, W2%
            Input #1, L2%

            For I1 = 0 To 22: Input #1, K$: Input #1, K%: Next I1

            For I1 = 0 To 21: Input #1, K$: Input #1, K%: Next I1

            For I1 = 0 To 22: For I2 = 0 To 21: Input #1, B5%(I, I1, I2): Next: Next

            For I1 = 0 To 21: For I2 = 0 To 41: Input #1, P5%(I, I1, I2): Next: Next

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

        If INNING% > eventSettings(1) Or S6%(1, 0) < eventSettings(2) Or S6%(0, 0) < eventSettings(2) Or S6%(1, 0) > eventSettings(3) Or S6%(0, 0) > eventSettings(3) Or S6%(1, 1) > eventSettings(5) Or S6%(0, 1) > eventSettings(5) Or S6%(1, 1) < eventSettings(4) Or S6%(0, 1) < eventSettings(4) Then
            checkSpecial% = 1
        Else

            For I = 0 To 1

                For I1 = 0 To 22

                    If B8%(I, I1, 1) > eventSettings(6) Or B8%(I, I1, 2) > eventSettings(7) Or B8%(I, I1, 3) > eventSettings(8) Or B8%(I, I1, 4) > eventSettings(9) Or B8%(I, I1, 5) > eventSettings(10) Or B8%(I, I1, 6) > eventSettings(11) Or B8%(I, I1, 9) > eventSettings(12) Then
                        checkSpecial% = 1
                        Exit For
                    End If

                    If checkSpecial% = 1 Then Exit For

                Next

            Next

            If checkSpecial% <> 1 Then

                For I = 0 To 1

                    For I1 = 0 To 21

                        If P8%(I, I1, 5) > eventSettings(13) Then
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

    'Beep
    Spec$ = diskPaths$(1) + "*.BSS"
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
    Shared DB$(), eventDesc$(), homeRuns$(), SB$(), TR$()

    For I = 0 To 1:
        For I1 = 0 To 21:
            If P8%(I, I1, 1) < 0 Then P8%(I, I1, 1) = 0
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 21
            If pitchRating(I, I1, 15) = 1 Then
                If P8%(I, I1, 0) >= 18 And P8%(I, I1, 3) <= 3 Then P8%(I, I1, 21) = 1
            End If
        Next I1

    Next I

    eventNbr = 0

    For I = 0 To 1:

        For I1 = 0 To 22

            If B8%(I, I1, 11) <> 0 Then

                If B8%(I, I1, 11) = 1 Then
                    eventDesc$(eventNbr) = batters$(I, I1)
                Else
                    eventDesc$(eventNbr) = batters$(I, I1) + Str$(B8%(I, I1, 11))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(B5%(I, I1, 11)), (Len(Str$(B5%(I, I1, 11))) - 1)) + ")"

                eventNbr = eventNbr + 1

            End If

            '        ***CHECKING DOUBLES***
            If B8%(I, I1, 4) <> 0 Then
                If B8%(I, I1, 4) = 1 Then
                    DB$(DB) = batters$(I, I1)
                Else
                    DB$(DB) = batters$(I, I1) + Str$(B8%(I, I1, 4))
                End If

                DB$(DB) = DB$(DB) + " (" + Right$(Str$(B5%(I, I1, 4)), (Len(Str$(B5%(I, I1, 4))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If B8%(I, I1, 5) <> 0 Then
                If B8%(I, I1, 5) = 1 Then
                    TR$(TR) = batters$(I, I1)
                Else
                    TR$(TR) = batters$(I, I1) + Str$(B8%(I, I1, 5))
                End If

                TR$(TR) = TR$(TR) + " (" + Right$(Str$(B5%(I, I1, 5)), (Len(Str$(B5%(I, I1, 5))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If B8%(I, I1, 6) <> 0 Then
                If B8%(I, I1, 6) = 1 Then
                    homeRuns$(HR) = batters$(I, I1)
                Else
                    homeRuns$(HR) = batters$(I, I1) + Str$(B8%(I, I1, 6))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(B5%(I, I1, 6)), (Len(Str$(B5%(I, I1, 6))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If B8%(I, I1, 9) <> 0 Then
                If B8%(I, I1, 9) = 1 Then
                    SB$(SB) = batters$(I, I1) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1
                Else
                    SB$(SB) = batters$(I, I1) + Str$(B8%(I, I1, 9)) + " (" + Right$(Str$(B5%(I, I1, 9)), (Len(Str$(B5%(I, I1, 9))) - 1)) + ")": SB = SB + 1
                End If
            End If

        Next I1

    Next I

    For I = 0 To 1

        For I1 = 0 To 21

            If P8%(I, I1, 34) <> 0 Then

                If P8%(I, I1, 34) = 1 Then
                    eventDesc$(eventNbr) = pitchers$(I, I1)
                Else
                    eventDesc$(eventNbr) = pitchers$(I, I1) + Str$(P8%(I, I1, 34))
                End If

                eventDesc$(eventNbr) = eventDesc$(eventNbr) + " (" + Right$(Str$(P5%(I, I1, 34)), (Len(Str$(P5%(I, I1, 34))) - 1)) + ")"
                eventNbr = eventNbr + 1

            End If

            '        ***CHECKING DOUBLES***
            If P8%(I, I1, 27) <> 0 Then

                If P8%(I, I1, 27) = 1 Then
                    DB$(DB) = pitchers$(I, I1)
                Else
                    DB$(DB) = pitchers$(I, I1) + Str$(P8%(I, I1, 27))
                End If

                DB$(DB) = DB$(DB) + " (" + Right$(Str$(P5%(I, I1, 27)), (Len(Str$(P5%(I, I1, 27))) - 1)) + ")"
                DB = DB + 1

            End If

            '        ***CHECKING TRIPLES***
            If P8%(I, I1, 28) <> 0 Then

                If P8%(I, I1, 28) = 1 Then
                    TR$(TR) = pitchers$(I, I1)
                Else
                    TR$(TR) = pitchers$(I, I1) + Str$(P8%(I, I1, 28))
                End If

                'Error #9 = index ???
                TR$(TR) = TR$(TR) + " (" + Right$(Str$(P5%(I, I1, 28)), (Len(Str$(P5%(I, I1, 28))) - 1)) + ")"
                TR = TR + 1

            End If

            '        ***CHECKING HOME RUNS***
            If P8%(I, I1, 29) <> 0 Then

                If P8%(I, I1, 29) = 1 Then
                    homeRuns$(HR) = pitchers$(I, I1)
                Else
                    homeRuns$(HR) = pitchers$(I, I1) + Str$(P8%(I, I1, 29))
                End If

                homeRuns$(HR) = homeRuns$(HR) + " (" + Right$(Str$(P5%(I, I1, 29)), (Len(Str$(P5%(I, I1, 29))) - 1)) + ")"
                HR = HR + 1

            End If

            '        ***CHECKING STOLEN BASES ***
            If P8%(I, I1, 32) <> 0 Then
                If P8%(I, I1, 32) = 1 Then
                    SB$(SB) = pitchers$(I, I1) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")"
                    SB = SB + 1
                Else
                    SB$(SB) = pitchers$(I, I1) + Str$(P8%(I, I1, 32)) + " (" + Right$(Str$(P5%(I, I1, 32)), (Len(Str$(P5%(I, I1, 32))) - 1)) + ")"
                    SB = SB + 1
                End If
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
            If pitchRating(I, I1, 15) >= 98 Then pitchRating(I, I1, 15) = 0
        Next I1
    Next I

    For I = 0 To 1:

        For I1 = 0 To 22:
            If batRating(I, I1, 21) >= 98 Then batRating(I, I1, 21) = 0
            If batRating(I, I1, 21) <> 0 Then B8%(I, I1, 14) = 1
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

    If I2 < 4 Then Locate , ((2 * I2) + 18)
    If I2 > 3 And I2 < 7 Then Locate , ((2 * I2) + 19)
    If I2 > 6 And I2 < 10 Then Locate , ((2 * I2) + 20)

    If I2 > 9 And I2 < 13 Then Locate , ((3 * (I2 - 9)) + 18)
    If I2 > 12 And I2 < 16 Then Locate , ((3 * (I2 - 9)) + 19)
    If I2 > 15 And I2 < 19 Then Locate , ((3 * (I2 - 9)) + 20)

    If I2 > 18 And I2 < 22 Then Locate , ((3 * (I2 - 18)) + 18)
    If I2 > 21 And I2 < 25 Then Locate , ((3 * (I2 - 18)) + 19)
    If I2 > 24 And I2 < 28 Then Locate , ((3 * (I2 - 18)) + 20)

End Sub

'----------------------------------------
'        InningPrintFile routine
'----------------------------------------
' ...explanation...
Sub InningPrintFile (I2)

    If I2 < 4 Then Print #2, Tab((2 * I2) + 15);
    If I2 > 3 And I2 < 7 Then Print #2, Tab((2 * I2) + 16);
    If I2 > 6 And I2 < 10 Then Print #2, Tab((2 * I2) + 17);

    If I2 > 9 And I2 < 13 Then Print #2, Tab((3 * I2) + 8);
    If I2 > 12 And I2 < 16 Then Print #2, Tab((3 * I2) + 9);
    If I2 > 15 And I2 < 19 Then Print #2, Tab((3 * I2) + 10);

    If I2 > 18 And I2 < 22 Then Print #2, Tab((3 * I2) + 11);
    If I2 > 21 And I2 < 25 Then Print #2, Tab((3 * I2) + 12);
    If I2 > 24 And I2 < 28 Then Print #2, Tab((3 * I2) + 13);

End Sub


'----------------------------------------
'          LDRNRCTDN Subroutine
'----------------------------------------
'Lead Runner Countdown? Caught Down?
Sub LDRNRCTDN ()

    If S4% = 7 Or S4% = 6 Or S4% = 5 Or S4% = 3 Then
        A5%(3) = 11
        For I = 0 To 2: A5%(I) = 1: Next
    End If

    If S4% = 2 Or S4% = 4 Then
        A5%(0) = 1
        A5%(1) = 1
        A5%(2) = 11
    End If

    If S4% = 1 Then
        A5%(1) = 11
        A5%(0) = 1
    End If

    If S4% = 0 Then A5%(0) = 11

    If PLB = 0 Then Call PBP(player$(0) + " bunts it hard!")
    If PLB = 0 Then Call PBP(player$(4) + " makes an easy play")

    PLB = 0
    nbrOuts(0) = nbrOuts(0) + 1
    nbrOuts(1) = nbrOuts(1) + 1

    Call ADDSTTS(P, D)
    Call CREDITASSIST(gameW%, D)
    Call ADVANCEMENT(P, D, S2%, F%, I3)
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
    endGame = 1

    Do
        I3 = 0

        If S6%(1, 0) - S6%(0, 0) > 0 Then I3 = 1
        If P2%(I3) <> P1%(I3) And S1%(I3) = -1 And P8%(I3, P1%(I3), 0) >= 9 And P8%(I3, P1%(I3), 0) <= 12 And P6%(I3) > 0 And P8%(I3, P1%(I3), 3) < 3 Then S1%(I3) = P1%(I3)
        If P2%(I3) = S1%(I3) Then S1%(I3) = -1

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

    endGame = 0
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
            Color tmRat_GAME(0, 11), tmRat_GAME(0, 12): Print gameTeams$(0); " ONLY"
            Color , 0
            Print
            Color 14: Print "(2) ";
            Color tmRat_GAME(1, 11), tmRat_GAME(1, 12): Print gameTeams$(1); " ONLY"
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
                If pitchRating(PX, I, 15) = 1 Then SX%(PX) = I: Exit For
            Next I
        Next PX

        If _FileExists("GAME82") Then

            Open diskPaths$(0) + scheduleFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

            For X = 0 To 18
                Field #1, X * 2 As X$, 2 As player$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
            Next

            Field #1, 38 As X$, 2 As player$(22), 2 As player$(23), 3 As player$(20), 3 As player$(21)

            NG%(13) = 1

            For X = 0 To 18:
                LSet player$(X + 1) = MKI$(NG%(X))
            Next

            For X = 0 To 1:
                LSet player$(20 + X) = diskIDs$(X)
            Next

            LSet player$(22) = MKI$(S6%(0, 0))
            LSet player$(23) = MKI$(S6%(1, 0))

            Put #1, schedGame(0) + schedGame(2)

            Close 1

            schedGame(2) = schedGame(2) + 1
            autoPlay = 1

            If schedGame(1) < schedGame(0) + schedGame(2) Then
                Kill "GAME82"
                endAllGames = 1
            Else

                Open "GAME82" For Output As #2

                Print #2, scheduleFile$

                For I = 0 To 2:
                    Print #2, schedGame(I)
                Next

                Close 2

            End If

        End If

        Cls
        Q = 0
        W2% = 0
        L2% = 0

        Do
            C% = 0

            If autoPlay = 1 Then
                C% = 0
            Else
                If X1 = 0 Or X1 = 1 And Q = 0 Or X1 = 2 And Q = 1 Then
                    Cls
                Else
                    If X1 = 2 And Q = 0 Or X1 = 1 And Q = 1 Then C% = 1
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

    Shared losePitcher$(), winPitcher$()
    Shared oppName$(), locIndicator$()

    Shared B6%(), DYS%(), eventSettings()
    Shared gameD1%(), nbrInnings()
    Shared teamScore(), oppScore()
    Shared statP0%(), statSS%(), statT0%(), SX%()

    statFile$ = nameStatFile$(gameTeams$(teamIdx))

    If _FileExists(diskPaths$(1) + statFile$ + "D") Then

        Open diskPaths$(1) + statFile$ + "D" For Input As #1

        For I1 = 0 To 22
            Input #1, INJ%(teamIdx, I1)
        Next

        For I1 = 0 To 21
            Input #1, DYS%(teamIdx, I1)
        Next

        Close 1

    End If

    If useInj = 1 Or useRest = 1 Then

        If useInj = 1 Then
            For I1 = 0 To 22
                If INJ%(teamIdx, I1) > 0 Then INJ%(teamIdx, I1) = INJ%(teamIdx, I1) - 1
                If batRating(teamIdx, I1, 32) > 0 Then INJ%(teamIdx, I1) = batRating(teamIdx, I1, 32)
            Next
        End If

        If useRest = 1 Then

            For I1 = 0 To 21

                If DYS%(teamIdx, I1) > 0 And DYS%(teamIdx, I1) < 90 Then DYS%(teamIdx, I1) = DYS%(teamIdx, I1) - 1

                If P8%(teamIdx, I1, 0) = 0 And DYS%(teamIdx, I1) > 0 And DYS%(teamIdx, I1) < 90 Then 7667
                If P8%(teamIdx, I1, 0) = 0 And (DYS%(teamIdx, I1) = 0 Or DYS%(teamIdx, I1) = 1 Or DYS%(teamIdx, I1) = 91 Or DYS%(teamIdx, I1) = 92) Then DYS%(teamIdx, I1) = 0: GoTo 7667
                If P8%(teamIdx, I1, 0) > 0 And P8%(teamIdx, I1, 0) <= 3 And DYS%(teamIdx, I1) = 0 Then DYS%(teamIdx, I1) = 91: GoTo 7667
                If P8%(teamIdx, I1, 0) > 0 And P8%(teamIdx, I1, 0) <= 3 And DYS%(teamIdx, I1) = 91 Then DYS%(teamIdx, I1) = 911: GoTo 7667
                If P8%(teamIdx, I1, 0) > 0 And P8%(teamIdx, I1, 0) <= 3 And DYS%(teamIdx, I1) = 911 Then DYS%(teamIdx, I1) = 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 0 And P8%(teamIdx, I1, 0) <= 3 And DYS%(teamIdx, I1) = 92 Then DYS%(teamIdx, I1) = 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 0 And P8%(teamIdx, I1, 0) <= 3 And DYS%(teamIdx, I1) > 0 Then DYS%(teamIdx, I1) = DYS%(teamIdx, I1) + 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 3 And P8%(teamIdx, I1, 0) <= 6 And DYS%(teamIdx, I1) = 0 Then DYS%(teamIdx, I1) = 92: GoTo 7667
                If P8%(teamIdx, I1, 0) > 3 And P8%(teamIdx, I1, 0) <= 6 And DYS%(teamIdx, I1) = 91 Then DYS%(teamIdx, I1) = 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 3 And P8%(teamIdx, I1, 0) <= 6 And DYS%(teamIdx, I1) = 92 Then DYS%(teamIdx, I1) = 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 3 And P8%(teamIdx, I1, 0) <= 6 And DYS%(teamIdx, I1) = 911 Then DYS%(teamIdx, I1) = 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 3 And P8%(teamIdx, I1, 0) <= 6 And DYS%(teamIdx, I1) > 0 Then DYS%(teamIdx, I1) = DYS%(teamIdx, I1) + 1: GoTo 7667
                If P8%(teamIdx, I1, 0) > 6 Then DYS%(teamIdx, I1) = Int(P8%(teamIdx, I1, 0) / 3) + 1 - 2

                If MG%(teamIdx, 1) <= 0 Then DYS%(teamIdx, I1) = 3: GoTo 7667

                If DYS%(teamIdx, I1) > MG%(teamIdx, 1) - 1 Then DYS%(teamIdx, I1) = MG%(teamIdx, 1) - 1
                If DYS%(teamIdx, I1) < 0 Then DYS%(teamIdx, I1) = 0

            7667 Next

        End If

        Open diskPaths$(1) + statFile$ + "D" For Output As #1
        For I1 = 0 To 22
            Print #1, INJ%(teamIdx, I1)
        Next
        For I1 = 0 To 21
            Print #1, DYS%(teamIdx, I1)
        Next
        Close 1

    End If

    If _FileExists(diskPaths$(1) + statFile$) Then

        Open diskPaths$(1) + statFile$ For Input As #1

        Input #1, gameTeams$(teamIdx)
        Input #1, W2%
        Input #1, L2%

        For I = 0 To 22:
            Input #1, batters$(teamIdx, I)
            Input #1, B2%(teamIdx, I)
        Next I

        For I = 0 To 21:
            Input #1, pitchers$(teamIdx, I)
            Input #1, P3%(teamIdx, I)
        Next I

        For I = 0 To 22:
            For I1 = 0 To 21:
                Input #1, B6%(I, I1)
            Next I1
        Next I

        For I = 0 To 21:
            For I1 = 0 To 41:
                Input #1, statP0%(I, I1)
            Next I1
        Next I

        Input #1, gameD0%(teamIdx)

        Input #1, gameD1%(teamIdx)

        For I = 0 To 21:
            Input #1, statT0%(I)
        Next I

        For I = 0 To 22:
            Input #1, P4%(I)
        Next I

        Close #1

    End If

    For I = 0 To 22

        B2%(teamIdx, I) = batRating(teamIdx, I, 4)

        For I1 = 0 To 11
            B6%(I, I1) = B6%(I, I1) + B8%(teamIdx, I, I1)
            statT0%(I1) = statT0%(I1) + B8%(teamIdx, I, I1)
        Next I1

        If B8%(teamIdx, I, 0) > 0 Then
            If B8%(teamIdx, I, 1) = 0 Then
                B6%(I, 12) = 0
            Else
                B6%(I, 12) = B6%(I, 12) + 1
            End If
        End If

        If B8%(teamIdx, I, 14) > 0 Then B6%(I, 14) = B6%(I, 14) + 1

        For I1 = 15 To 21
            B6%(I, I1) = B6%(I, I1) + B8%(teamIdx, I, I1)
            statT0%(I1) = statT0%(I1) + B8%(teamIdx, I, I1)
        Next I1

        If B6%(I, 12) >= B6%(I, 13) Then B6%(I, 13) = B6%(I, 12)

    Next I

    For I = 0 To 21

        For I1 = 23 To 34
            statP0%(I, I1) = statP0%(I, I1) + P8%(teamIdx, I, I1)
            statT0%(I1 - 23) = statT0%(I1 - 23) + P8%(teamIdx, I, I1)
        Next I1

        If P8%(teamIdx, I, 23) > 0 Then
            If P8%(teamIdx, I, 24) = 0 Then
                statP0%(I, 35) = 0
            Else
                statP0%(I, 35) = statP0%(I, 35) + 1
            End If
        End If

        For I1 = 37 To 41
            statP0%(I, I1) = statP0%(I, I1) + P8%(teamIdx, I, I1)
            statT0%(I1 - 22) = statT0%(I1 - 22) + P8%(teamIdx, I, I1)
        Next I1

        If statP0%(I, 35) >= statP0%(I, 36) Then statP0%(I, 36) = statP0%(I, 35)

    Next I

    statT0%(12) = statT0%(12) + L0%(teamIdx)
    INNING% = 0

    For I = 0 To 21
        INNING% = INNING% + P8%(teamIdx, I, 0)
        P3%(teamIdx, I) = pitchRating(teamIdx, I, 6)

        For I1 = 0 To 5
            statP0%(I, I1) = statP0%(I, I1) + P8%(teamIdx, I, I1)
            P4%(I1) = P4%(I1) + P8%(teamIdx, I, I1)
        Next I1

        For I1 = 13 To 22:
            statP0%(I, I1) = statP0%(I, I1) + P8%(teamIdx, I, I1)
            P4%(I1) = P4%(I1) + P8%(teamIdx, I, I1)
        Next I1

    Next I

    If S6%(teamIdx, 0) - S6%(1 - teamIdx, 0) > 0 Then statP0%(P2%(teamIdx), 6) = statP0%(P2%(teamIdx), 6) + 1
    If S6%(teamIdx, 0) - S6%(1 - teamIdx, 0) < 0 Then statP0%(P2%(teamIdx), 7) = statP0%(P2%(teamIdx), 7) + 1
    If S6%(teamIdx, 0) - S6%(1 - teamIdx, 0) > 0 And S1%(teamIdx) >= 0 Then statP0%(S1%(teamIdx), 12) = statP0%(S1%(teamIdx), 12) + 1: P4%(12) = P4%(12) + 1

    For I = 0 To 21
        If pitchRating(teamIdx, I, 15) <> 0 Then statP0%(I, 8) = statP0%(I, 8) + 1: P4%(8) = P4%(8) + 1
        If INNING% = P8%(teamIdx, I, 0) Then statP0%(I, 10) = statP0%(I, 10) + 1: P4%(10) = P4%(10) + 1
        If INNING% = P8%(teamIdx, I, 0) And S6%(1 - teamIdx, 0) = 0 Then statP0%(I, 11) = statP0%(I, 11) + 1: P4%(11) = P4%(11) + 1
    Next I

    statP0%(SX%(teamIdx), 9) = statP0%(SX%(teamIdx), 9) + 1
    P4%(9) = P4%(9) + 1

    gameD0%(teamIdx) = gameD0%(teamIdx) + D3%(teamIdx)
    gameD1%(teamIdx) = gameD1%(teamIdx) + D3%(1 - teamIdx)

    If S6%(teamIdx, 0) - S6%(1 - teamIdx, 0) > 0 Then W2% = W2% + 1
    If S6%(teamIdx, 0) - S6%(1 - teamIdx, 0) < 0 Then L2% = L2% + 1

    Cls

    Color 15, 0

    Print ">>>>>"
    Print "THE STATS ARE BEING SAVED UNDER"
    Print "THE FOLLOWING NAME ::: ";
    Color tmRat_GAME(teamIdx, 11), tmRat_GAME(teamIdx, 12): Print statFile$

    Color 15, 0

    Open diskPaths$(1) + statFile$ For Output As #1

    Print #1, gameTeams$(teamIdx)
    Print #1, W2%
    Print #1, L2%

    For I = 0 To 22:
        Print #1, batters$(teamIdx, I)
        Print #1, B2%(teamIdx, I)
    Next I

    For I = 0 To 21:
        Print #1, pitchers$(teamIdx, I)
        Print #1, P3%(teamIdx, I)
    Next I

    For I = 0 To 22:
        For I1 = 0 To 21:
            Print #1, B6%(I, I1)
        Next I1
    Next I

    For I = 0 To 21:
        For I1 = 0 To 41:
            Print #1, statP0%(I, I1)
        Next I1
    Next I

    Print #1, gameD0%(teamIdx)
    Print #1, gameD1%(teamIdx)

    For I = 0 To 21:
        Print #1, statT0%(I)
    Next I

    For I = 0 To 22:
        Print #1, P4%(I)
    Next I

    Close #1

    Print "<<<<<"

    If _FileExists(diskPaths$(1) + statFile$ + "R") Then

        Open diskPaths$(1) + statFile$ + "R" For Input As #1

        For I = 0 To 22
            For I1 = 0 To 6
                Input #1, statSS%(I, I1)
            Next
        Next

        Close 1

    End If

    For I = 0 To 22
        For I1 = 0 To 6
            statSS%(I, I1) = statSS%(I, I1) + CSS%(teamIdx, I, I1)
        Next
    Next

    Open diskPaths$(1) + statFile$ + "R" For Output As #1

    For I = 0 To 22
        For I1 = 0 To 6
            Print #1, statSS%(I, I1)
        Next
    Next

    Close 1

    numberGames = 0

    If _FileExists(diskPaths$(1) + statFile$ + "S") Then

        Open diskPaths$(1) + statFile$ + "S" For Input As #1

        Input #1, numberGames

        For I = 1 To MAX_SCHED_STATS:
            Input #1, teamScore(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppName$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, oppScore(I)
        Next I

        For I = 1 To MAX_SCHED_STATS
            Input #1, locIndicator$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, winPitcher$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, losePitcher$(I)
        Next I

        For I = 1 To MAX_SCHED_STATS:
            Input #1, nbrInnings(I)
        Next I

        Close #1

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

    If numberGames < MAX_SCHED_STATS Then numberGames = numberGames + 1
    teamScore(numberGames) = S6%(teamIdx, 0)
    oppName$(numberGames) = gameTeams$(1 - teamIdx)
    oppScore(numberGames) = S6%(1 - teamIdx, 0)
    winPitcher$(numberGames) = gameW$
    losePitcher$(numberGames) = gameL$
    nbrInnings(numberGames) = LYN%

    If teamIdx = 0 Then
        locIndicator$(numberGames) = "A"
    Else
        locIndicator$(numberGames) = "H"
    End If

    Open diskPaths$(1) + statFile$ + "S" For Output As #1

    Print #1, numberGames

    For I = 1 To MAX_SCHED_STATS:
        Print #1, teamScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, oppName$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, oppScore(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, locIndicator$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, winPitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, losePitcher$(I)
    Next I

    For I = 1 To MAX_SCHED_STATS:
        Print #1, nbrInnings(I)
    Next I

    Close #1

    numberGames = 0

    Erase B2%, B6%, nbrInnings
    Erase oppScore, P3%, P4%
    Erase statSS%, statP0%, statT0%
    Erase teamScore

    Erase locIndicator$, oppName$
    Erase losePitcher$, winPitcher$

    W2% = 0
    L2% = 0

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
                    Print "ENTER 8-CHARACTER FILENAME FOR BOXSCORE (NO SPACES OR PERIODS)";: Input boxName$
                    boxName$ = boxName$ + ".BSS"
                Else
                    'ScheduleName_GameXXX.BS
                    boxName$ = Mid$(scheduleFile$, 1, Len(scheduleFile$) - 4) + "_GAME" + Mid$(Str$(schedGame(0) + schedGame(2)), 2, Len(Str$(schedGame(0) + schedGame(2)))) + ".BS"
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
                If I = 0 Then Print #2, Using "##"; inningScore(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And inningScore(1, INNING%) = 0 Then Print #2, " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And inningScore(1, INNING%) > 0 Then Print #2, Using "##"; inningScore(I, I2);
                If I = 1 And I2 < INNING% Then Print #2, Using "##"; inningScore(I, I2);
            Next I2

            Print #2, " -";
            Print #2, Using " ##"; S6%(I, 0);
            Print #2, Using " ##"; S6%(I, 1);
            Print #2, Using " #"; S6%(I, 2)

        Next I

    End If

    If INNING% = 10 And P = 0 Or INNING% = 9 Then

        For I = 0 To 1

            Print #2, gameTeams$(I);

            For I2 = 1 To 9
                Call InningPrintFile(I2)
                If I = 0 Then Print #2, Using "##"; inningScore(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And inningScore(1, 9) = 0 Then Print #2, " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = 9 Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = 9 And inningScore(1, 9) > 0 Then Print #2, Using "##"; inningScore(I, I2);
                If I = 1 And I2 < 9 Then Print #2, Using "##"; inningScore(I, I2);
            Next I2

            If INNING% = 10 Then Print #2, "   ";
            Print #2, " -";
            Print #2, Using " ##"; S6%(I, 0);
            Print #2, Using " ##"; S6%(I, 1);
            Print #2, Using " #"; S6%(I, 2)

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

            If (INNING% Mod 3 = 0) Then Print #2, " "

            Print #2, " -";
            Print #2, Using " ##"; S6%(I, 0);
            Print #2, Using " ##"; S6%(I, 1);
            Print #2, Using " #"; S6%(I, 2)

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
            Print #2, Using " ##"; S6%(I, 0);
            Print #2, Using " ##"; S6%(I, 1);
            Print #2, Using " #"; S6%(I, 2)

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
                Print #2, Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);

                Print #2, "   ";

                'BA
                If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then
                    Print #2, " .000   ";
                Else
                    If B5%(I, I1, 0) = B5%(I, I1, 1) Then
                        Print #2, "1.000   ";
                    Else
                        Print #2, " ."; Using "###   "; (B5%(I, I1, 1) / B5%(I, I1, 0) * 1000);
                    End If
                End If

                'HR / RBI
                Print #2, Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + B8%(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21: For I2 = 0 To 13: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23): Next

        For I2 = 14 To 17: T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22): Next: Next

        For I1 = 1 To P6%(I)
            For I2 = 0 To 21

                If pitchRating(I, I2, 15) = I1 Then

                    Print #2, pitchers$(I, I2); ", P";
                    Print #2, Tab(18);
                    Print #2, Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
                    Print #2, "   ";

                    If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then
                        Print #2, " .000   ";
                    Else
                        If P5%(I, I2, 24) = P5%(I, I2, 23) Then
                            Print #2, "1.000   ";
                        Else
                            Print #2, Using " .###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
                        End If
                    End If

                    Print #2, Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)
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

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, 15) = I1 Then

                    I4 = 1 - I: Print #2, pitchers$(I, I2); " ";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then
                        Print #2, "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then
                        Print #2, "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If P8%(I, I2, 19) > 0 Then Print #2, "(BS,";: Print #2, Using "##"; P5%(I, I2, 19);: Print #2, ")";
                    If P8%(I, I2, 22) > 0 Then Print #2, "(H,";: Print #2, Using "##"; P5%(I, I2, 22);: Print #2, ")";

                    Print #2, Tab(29);: Print #2, Using "##"; Int(P8%(I, I2, 0) / 3);

                    If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print #2, ".";: Print #2, Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);

                    Print #2, Tab(34);
                    Print #2, Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);
                    Print #2, "   ";

                    If P5%(I, I2, 3) > 0 And P5%(I, I2, 0) = 0 Then
                        Print #2, "99.99 ";
                    Else
                        If P5%(I, I2, 0) = 0 Then
                            Print #2, "--.-- ";
                        Else
                            If P5%(I, I2, 3) = 0 Then
                                Print #2, " 0.00 ";
                            Else
                                Print #2, Using "##.## "; P5%(I, I2, 3) / (P5%(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 6) + 1; Else Print #2, Using "## "; P5%(I, I2, 6);
                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print #2, Using "## "; P5%(I, I2, 7) + 1; Else Print #2, Using "## "; P5%(I, I2, 7);
                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print #2, Using "## "; P5%(I, I2, 12) + 1; Else Print #2, Using "## "; P5%(I, I2, 12);

                    Print #2, Using "###"; P5%(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Print #2, "TOTALS"; Tab(28);
        Print #2, Using "##"; Int(T3%(0) / 3);

        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then Print #2, ".";: Print #2, Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);

        Print #2, Tab(34);
        Print #2, Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)

        LYN% = CInt(T3%(0) / 3)

        For I1 = 0 To 5: T3%(I1) = 0: Next I1

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
        For X1 = 0 To (DB - 1): Print #2, DB$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "3B-";
    If TR = 0 Then
        Print #2, "NONE"
    Else
        For X1 = 0 To (TR - 1): Print #2, TR$(X1); ",";: Next
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
        For X1 = 0 To (SB - 1): Print #2, SB$(X1); ",";: Next
        Print #2,
    End If

    Print #2, "LOB-"; gameTeams$(0); Str$(L0%(0)); ","; gameTeams$(1); Str$(L0%(1))
    Print #2, "DP-"; gameTeams$(0); Str$(DP%(0)); ","; gameTeams$(1); Str$(DP%(1))
    Print #2, "UMPIRES: HP-"; gameUmpire$(1); "; 1B-"; gameUmpire$(2); "; 2B-"; gameUmpire$(3); "; 3B-"; gameUmpire$(4)
    Print #2, "TIME: "; gameTime$
    Print #2, "ATTENDANCE: ";: Print Using "###,###"; actualAttendance&
    Print #2,
    Print #2, "INJURIES-";

    If IJL% = 0 Then
        Print #2, "none"
    Else
        For X1 = 1 To IJL%: Print #2, IJ$(X1); ",";: Next
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
    Shared DB$(), eventDesc$(), homeRuns$(), SB$(), TR$()

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
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To INNING%
                Call InningPrintBox(I2)
                If I = 0 Then Print Using "##"; inningScore(I, I2);
                If I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And inningScore(1, INNING%) = 0 Then Print , " X";
                If I = 1 And S6%(1, 0) < S6%(0, 0) And I2 = INNING% Or I = 1 And S6%(1, 0) > S6%(0, 0) And I2 = INNING% And inningScore(1, INNING%) > 0 Then Print Using "##"; inningScore(I, I2);
                If I = 1 And I2 < INNING% Then Print Using "##"; inningScore(I, I2);

            Next I2

            Print " -";
            Print Using "## "; S6%(I, 0);
            Print Using "## "; S6%(I, 1);
            Print Using "#"; S6%(I, 2)

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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
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

            Print Using "## "; S6%(I, 0);
            Print Using "## "; S6%(I, 1);
            Print Using "#"; S6%(I, 2)

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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 1 To 9
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print

        Next I

        If P = 0 And INNING% < 19 Or INNING% = 19 Then INNING% = INNING% - 1

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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 10 To INNING%
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print " - ";
            Print Using "## "; S6%(I, 0);
            Print Using "## "; S6%(I, 1);
            Print Using "#"; S6%(I, 2)

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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
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

            Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
            Print gameTeams$(I);
            Color 15, 0

            For I2 = 19 To INNING%
                Call InningPrintBox(I2)
                Print Using "##"; inningScore(I, I2);
            Next I2

            Print " -";
            Print Using "## "; S6%(I, 0);
            Print Using "## "; S6%(I, 1);
            Print Using "#"; S6%(I, 2)

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

        Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
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
                Print Using BK$; B8%(I, I1, 0); B8%(I, I1, 2); B8%(I, I1, 1); B8%(I, I1, 3); B8%(I, I1, 4); B8%(I, I1, 5); B8%(I, I1, 6); B8%(I, I1, 8); B8%(I, I1, 7); B8%(I, I1, 9); B8%(I, I1, 10); B8%(I, I1, 11); B8%(I, I1, 16); B8%(I, I1, 17);
                Print "   ";

                'BA
                If B5%(I, I1, 0) = 0 Or B5%(I, I1, 1) = 0 Then
                    Print " .000   ";
                Else
                    If B5%(I, I1, 0) = B5%(I, I1, 1) Then
                        Print "1.000   ";
                    Else
                        Print " ."; Using "###   "; (B5%(I, I1, 1) / B5%(I, I1, 0) * 1000);
                    End If
                End If

                '           HR   RBI
                Print Using "##  ###"; B5%(I, I1, 6); B5%(I, I1, 3)

                For I2 = 0 To 17
                    T3%(I2) = T3%(I2) + B8%(I, I1, I2)
                Next I2

            End If

        Next I3

        For I1 = 0 To 21:
            For I2 = 0 To 13:
                T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 23)
            Next

            For I2 = 14 To 17
                T3%(I2) = T3%(I2) + P8%(I, I1, I2 + 22)
            Next
        Next

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, 15) = I1 Then

                    Print pitchers$(I, I2); ", P";
                    Locate , 18
                    '                AB              R               H               BI              2               3               HR              K               W               SB              CS              E               PO              A
                    Print Using BK$; P8%(I, I2, 23); P8%(I, I2, 25); P8%(I, I2, 24); P8%(I, I2, 26); P8%(I, I2, 27); P8%(I, I2, 28); P8%(I, I2, 29); P8%(I, I2, 31); P8%(I, I2, 30); P8%(I, I2, 32); P8%(I, I2, 33); P8%(I, I2, 34); P8%(I, I2, 38); P8%(I, I2, 39);
                    Print "   ";

                    'BA
                    If P5%(I, I2, 24) = 0 Or P5%(I, I2, 23) = 0 Then
                        Print " .000   "
                    Else
                        If P5%(I, I2, 24) = P5%(I, I2, 23) Then
                            Print "1.000   ";
                        Else
                            Print Using "#.###   "; P5%(I, I2, 24) / P5%(I, I2, 23);
                        End If

                        '           HR   RBI
                        Print Using "##  ###"; P5%(I, I2, 29); P5%(I, I2, 26)

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
            Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Cls

        For I1 = 0 To 22: T3%(I1) = 0: Next I1

    Next I

    'GoTo 7313

    For I = 0 To 1

        Cls

        Color tmRat_GAME(I, 11), tmRat_GAME(I, 12)
        Print gameTeams$(I);

        Color 14, 0: Locate , 23
        Print "       IP   H  R  ER BB SO    ERA   W  L  S  SO": Color 15, 0

        For I1 = 1 To P6%(I)

            For I2 = 0 To 21

                If pitchRating(I, I2, 15) = I1 Then
                    I4 = 1 - I
                    Locate , 1
                    Print pitchers$(I, I2); " ";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then
                        Print "(W,"; Right$(Str$(P5%(I, I2, 6) + 1), (Len(Str$(P5%(I, I2, 6) + 1)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7)), (Len(Str$(P5%(I, I2, 7))) - 1)); ")";
                        gameW$ = pitchers$(I, I2)
                    End If

                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print "(S,"; Right$(Str$(P5%(I, I2, 12) + 1), (Len(Str$(P5%(I, I2, 12) + 1)) - 1)); ")";

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then
                        Print "(L,"; Right$(Str$(P5%(I, I2, 6) + 0), (Len(Str$(P5%(I, I2, 6) + 0)) - 1)); "-"; Right$(Str$(P5%(I, I2, 7) + 1), (Len(Str$(P5%(I, I2, 7) + 1)) - 1)); ")";
                        gameL$ = pitchers$(I, I2)
                    End If

                    If P8%(I, I2, 19) > 0 Then Print "(BS,";: Print Using "##"; P5%(I, I2, 19);: Print ")";
                    If P8%(I, I2, 22) > 0 Then Print "(H,";: Print Using "##"; P5%(I, I2, 22);: Print ")";

                    Locate , 30: Print Using "##"; Int(P8%(I, I2, 0) / 3);

                    If P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3) > 0 Then Print ".";: Print Using "#"; P8%(I, I2, 0) - (Int(P8%(I, I2, 0) / 3) * 3);

                    Locate , 34

                    Print Using "## ##  ## ## ##"; P8%(I, I2, 1); P8%(I, I2, 2); P8%(I, I2, 3); P8%(I, I2, 4); P8%(I, I2, 5);

                    Print "   ";

                    If P8%(I, I2, 3) > 0 And P8%(I, I2, 0) = 0 Then
                        Print "99.99 ";
                    Else
                        If P8%(I, I2, 0) = 0 Then
                            Print "--.-- ";
                        Else
                            If P8%(I, I2, 3) = 0 Then
                                Print " 0.00 ";
                            Else
                                Print Using "##.## "; P8%(I, I2, 3) / (P8%(I, I2, 0) / 3) * 9;
                            End If
                        End If
                    End If

                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 6) + 1; Else Print Using "## "; P5%(I, I2, 6);
                    If P2%(I) = I2 And S6%(I, 0) - S6%(I4, 0) < 0 Then Print Using "## "; P5%(I, I2, 7) + 1; Else Print Using "## "; P5%(I, I2, 7);
                    If S1%(I) = I2 And S6%(I, 0) - S6%(I4, 0) > 0 Then Print Using "## "; P5%(I, I2, 12) + 1; Else Print Using "## "; P5%(I, I2, 12);

                    Print Using "###"; P5%(I, I2, 5)

                    For I4 = 0 To 5
                        T3%(I4) = T3%(I4) + P8%(I, I2, I4)
                    Next I4
                End If

            Next I2

        Next I1

        Color 14, 0
        Print "TOTALS";
        Color 15, 0

        Locate , 30

        If (T3%(0) / 3) >= 10 Then
            Locate (P6%(I) + 2), 29
        End If

        Print Int(T3%(0) / 3);

        If T3%(0) - (Int(T3%(0) / 3) * 3) > 0 Then
            Print ".";: Print Using "#"; T3%(0) - (Int(T3%(0) / 3) * 3);
        End If

        Locate , 34

        Print Using "## ##  ## ## ##"; T3%(1); T3%(2); T3%(3); T3%(4); T3%(5)
        Print

        If autoPlay <> 1 Or playerMode <> 2 Then
            Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
            I$ = GetKeyPress$
        End If

        Cls

        LYN% = CInt(T3%(0) / 3)

        For I1 = 0 To 5: T3%(I1) = 0: Next I1

    Next I

    Cls

    '--- THIS NEEDS TO LOOK NICER

    Color 14, 0: Print " E-";
    Color 15, 0

    If E = 0 Then
        Print "none"
    Else
        For X = 0 To (E - 1): Print eventDesc$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "2B-";
    Color 15, 0

    If DB = 0 Then
        Print "none"
    Else
        For X = 0 To (DB - 1): Print DB$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "3B-";
    Color 15, 0

    If TR = 0 Then
        Print "none"
    Else
        For X = 0 To (TR - 1): Print TR$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "HR-";
    Color 15, 0

    If HR = 0 Then
        Print "none"
    Else
        For X = 0 To (HR - 1): Print homeRuns$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "SB-";
    Color 15, 0
    If SB = 0 Then
        Print "none"
    Else
        For X = 0 To (SB - 1): Print SB$(X); ",";: Next
        Print
    End If

    Color 14, 0: Print "LOB-";
    Color 15, 0: Print gameTeams$(0); Str$(L0%(0)); ","; gameTeams$(1); Str$(L0%(1))
    Print "DP-";
    Color 15, 0: Print gameTeams$(0); Str$(DP%(0)); ","; gameTeams$(1); Str$(DP%(1))
    Color 14, 0: Print "INJURIES-";
    Color 15, 0

    If IJL% = 0 Then
        Print "none"
    Else
        For X = 1 To IJL%: Print IJ$(X); ",";: Next
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
        Locate 24, 1: Print "HIT ANY KEY TO CONTINUE";
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

    If INNING% > eventSettings(1) Then Print #2, Using "MORE THAN ## INNINGS"; eventSettings(1)

    If S6%(1, 0) < eventSettings(2) Then Print #2, gameTeams$(1);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; eventSettings(2)
    If S6%(0, 0) < eventSettings(2) Then Print #2, gameTeams$(0);: Print #2, Using " SCORED LESS THAN ## RUN(S)"; eventSettings(2)
    If S6%(1, 0) > eventSettings(3) Then Print #2, gameTeams$(1);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(3)
    If S6%(0, 0) > eventSettings(3) Then Print #2, gameTeams$(0);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(3)
    If S6%(1, 1) > eventSettings(5) Then Print #2, gameTeams$(1);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(5)
    If S6%(0, 1) > eventSettings(5) Then Print #2, gameTeams$(0);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(5)
    If S6%(1, 1) < eventSettings(4) Then Print #2, gameTeams$(1);: Print #2, Using " HAD LESS THAN ## HIT%(S)"; eventSettings(4)
    If S6%(0, 1) < eventSettings(4) Then Print #2, gameTeams$(0);: Print #2, Using " HAD LESS THAN ## HIT%(S)"; eventSettings(4)

    For I = 0 To 1

        For I1 = 0 To 22

            If B8%(I, I1, 1) > eventSettings(6) Then Print #2, batters$(I, I1);: Print #2, Using " HAD MORE THAN ## HIT%(S)"; eventSettings(6)
            If B8%(I, I1, 2) > eventSettings(7) Then Print #2, batters$(I, I1);: Print #2, Using " SCORED MORE THAN ## RUN(S)"; eventSettings(7)
            If B8%(I, I1, 3) > eventSettings(8) Then Print #2, batters$(I, I1);: Print #2, Using " HAD MORE THAN ## RBI(S)"; eventSettings(8)
            If B8%(I, I1, 4) > eventSettings(9) Then Print #2, batters$(I, I1);: Print #2, Using " HIT MORE THAN ## DOUBLES(S)"; eventSettings(9)
            If B8%(I, I1, 5) > eventSettings(10) Then Print #2, batters$(I, I1);: Print #2, Using " HIT MORE THAN ## TRIPLE(S)"; eventSettings(10)
            If B8%(I, I1, 6) > eventSettings(11) Then Print #2, batters$(I, I1);: Print #2, Using " HOMERED MORE THAN ## TIME(S)"; eventSettings(11)
            If B8%(I, I1, 9) > eventSettings(12) Then Print #2, batters$(I, I1);: Print #2, Using " STOLE MORE THAN ## BASE(S)"; eventSettings(12)

        Next

    Next

    For I = 0 To 1
        For I1 = 0 To 21
            If P8%(I, I1, 5) > eventSettings(13) Then Print #2, pitchers$(I, I1);: Print #2, Using " STRUCK OUT MORE THAN ## BATTER(S)"; eventSettings(13)
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

    Close #7

    Cls

    If useVGA = 1 Then
        Screen 0
        _FreeImage imageScreen&
    End If

    If normalFinish = 1 Then

        Call POSTGAME

        ' Prompt to play another game
        Cls

        If autoPlay <> 1 Then

            'Using Home Team's color for the box
            Call TEAMBOX(1, 1)

            Do
                U$ = "PLAY ANOTHER GAME? (Y/N)"
                Locate 7, (80 - Len(U$)) / 2
                Print U$

                Color 15, 0
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

        If UCase$(I$) = "Y" Or (autoPlay = 1 And endAllGames <> 1) Then
            Cls
            Print "PLEASE WAIT ..."
            Call LOADER
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
'            pbplog Function
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

                Locate 8, 27: Print I$
                Locate 8, 28: Line Input NY$
                NY$ = I$ + NY$
                Color 15, 0

                If Not (NY$ = "" And yearNumber$ <> "") Then

                    yearNumber$ = RTrim$(NY$)
                    Erase teamNames$
                    numberTeams = 0

                    Cls

                    If Not _FileExists(diskPaths$(0) + TEAM_FILE_NAME$ + "." + yearNumber$) Then

                        MsgText$ = "NO TEAM FILES WERE FOUND MATCHING THIS ID. PRESS ANY KEY TO RETURN TO MAIN MENU"
                        BoxWidth = 20
                        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
                        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
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

                        previousYear$ = yearNumber$

                    End If 'Done checking if file exists

                End If 'Done checking for blank year number

            Else

                yearNumber$ = previousYear$

            End If 'Done checking for ENTER key

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Do

                Call SelectGameTeam(numberTeams, yearNumber$, chosenIdx, teamName$, ExitCode%, gameIdx)

                If ExitCode% <> 27 And numberTeams <> 0 Then

                    Do
                        Locate 2, 33
                        Color tmRat_GAME(gameIdx, 11), tmRat_GAME(gameIdx, 12): Print teamName$

                        Color 15, 4
                        Locate 3, 32: Print "ANY CHANGE? (Y/N)";

                        J$ = GetKeyPress$

                    Loop Until J$ = Chr$(27) Or UCase$(J$) = "Y" Or UCase$(J$) = "N"

                End If

            Loop Until ExitCode% = 27 Or J$ = Chr$(27) Or UCase$(J$) = "N"

        End If 'Done checking for ESC

    Loop Until UCase$(J$) = "N" Or I$ = Chr$(27) 'Or J$ = Chr$(27)

    If I$ = Chr$(27) Or J$ = Chr$(27) Then chosenIdx = 0

End Sub


'----------------------------------------
'       SelectGameTeam Subroutine
'----------------------------------------
'This subroutine provides an interface for
'selecting a team from the list loaded by
'the disk ID.
Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)

    If numberTeams > 0 Then

        SELECTION% = 1
        Count% = numberTeams
        Start% = 1
        FileFGColor = 15: FileBGColor = 4
        HiLiteFGColor = 0: HiLiteBGColor = 7
        Rows% = 20
        Columns% = 4
        Spacing% = 6
        Row% = 4
        Column% = 1

        Color , 4
        Cls
        Color 14
        Locate , 33: Print "TEAM SELECTION"
        Locate 25, 1
        Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";
        Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

        _MouseShow "DEFAULT"
        MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, Rows%, Columns%, Spacing%, Row%, Column%
        _MouseHide

        If ExitCode% <> 27 Then

            selectedIdx = teamIndex%(SELECTION%)

            'Locate 2, 33
            'Color 31: Print "LOADING..."

            Call ReadGameTeam(yearNumber$, teamName$, selectedIdx, gameIdx)

        End If

    End If

End Sub


'----------------------------------------
'        ReadGameTeam Subroutine
'----------------------------------------
'This subroutine reads in the data for the
'selected tema into each of the data arrays
'needed by the game.
Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx)

    Shared MF%()

    Dim DYS%(1, 21)

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

    For I1 = 1 To 13
        tmRat_GAME(gameIdx, I1) = CVI(Q2$(I1))
    Next

    If (tmRat_GAME(gameIdx, 11) = 7 Or tmRat_GAME(gameIdx, 11) = 15) And tmRat_GAME(gameIdx, 12) = 0 Then
        tmRat_GAME(gameIdx, 11) = 0
        tmRat_GAME(gameIdx, 12) = 7
    End If

    parkHRVals!(gameIdx) = CVS(Q2$(14))

    For I1 = 0 To 22:

        batters$(gameIdx, I1) = RTrim$(Q2$(I1 * 43 + 15))

        For I2 = 0 To 41
            batRating(gameIdx, I1, I2) = CVI(Q2$(I1 * 43 + I2 + 16))
        Next

        If batRating(gameIdx, I1, 31) <> 0 Then batRating(gameIdx, I1, 21) = 99
    Next

    For I1 = 0 To 21:
        pitchers$(gameIdx, I1) = Q2$(I1 * 38 + 1004)
        pitchers$(gameIdx, I1) = RTrim$(pitchers$(gameIdx, I1))

        For I2 = 0 To 36
            pitchRating(gameIdx, I1, I2) = CVI(Q2$(I1 * 38 + I2 + 1005))
        Next

        If pitchRating(gameIdx, I1, 34) <> 0 Then pitchRating(gameIdx, I1, 15) = 99
        If pitchRating(gameIdx, I1, 0) = 0 Then pitchRating(gameIdx, I1, 0) = 1
    Next

    gameStadiums$(gameIdx) = RTrim$(Q2$(1840))
    gameManagers$(gameIdx) = RTrim$(Q2$(1841))
    teamAbbreviatons$(gameIdx) = RTrim$(Q2$(1842))
    CK = CVI(Q2$(1843))

    Close 1

    For I1 = 0 To 22
        'check for 1934 season
        If Val(Left$(gameTeams$(gameIdx), 2)) < 54 And Mid$(gameTeams$(gameIdx), 3, 1) = " " Then batRating(gameIdx, I1, 30) = batRating(gameIdx, I1, 30) * .75

        For I2 = 22 To 25
            If batRating(gameIdx, I1, I2) <> -1 And batRating(gameIdx, I1, I2) <> 0 And batRating(gameIdx, I1, I2 + 4) = 0 Then batRating(gameIdx, I1, I2 + 4) = 900
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

    If useLRSplit = 0 Then

        TS%(gameIdx, 0) = -1

    Else

        'Load LR split from file
        If Not _FileExists(diskPaths$(0) + DATA_FILE_NAME$ + "." + teamYear$) Then

            TS%(gameIdx, 0) = -1

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
                TS%(gameIdx, X) = CVI(Q2$(X + 1))
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
                gameSA%(gameIdx, X) = CVI(Q2$(X + 950))
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

            If TS%(gameIdx, 0) <> 999 Then TS%(gameIdx, 0) = -1

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
    If Not _FileExists(diskPaths$(3) + MGR_FILE_NAME$ + "." + teamYear$) Then

        MF%(gameIdx) = -1

    Else

        Open diskPaths$(3) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #3 Len = MGR_SIZE_BYTES

        For X = 0 To 999
            Field #3, X * 2 As Q2$, 2 As Q2$(X), MGR_SIZE_BYTES - X * 2 - 2 As Q2$
        Next

        Get #3, teamIdx

        For X = 0 To 999: MG%(gameIdx, X) = CVI(Q2$(X)): Next

        Close 3

    End If

    If MG%(gameIdx, 0) <> 999 Or MF%(gameIdx) = -1 Then

        MF%(gameIdx) = -1

        For X = 0 To 999:
            MG%(gameIdx, X) = -1
        Next

    End If

    statFile$ = nameStatFile$(gameTeams$(idx))

    'Read in supplemental data
    If _FileExists(diskPaths$(1) + statFile$ + "D") Then

        Open diskPaths$(1) + statFile$ + "D" For Input As #4

        For I1 = 0 To 22
            Input #4, INJ%(gameIdx, I1)
        Next

        For I1 = 0 To 21
            Input #4, DYS%(gameIdx, I1)
        Next

        Close #4

        If useInj = 1 Then
            For I1 = 0 To 22
                If INJ%(gameIdx, I1) > 0 Then batRating(gameIdx, I1, 21) = 99
            Next
        End If

    End If

    If useRest = 1 Then

        For I1 = 0 To 21
            If DYS%(gameIdx, I1) <> 0 And DYS%(gameIdx, I1) < 90 Then
                pitchRating(gameIdx, I1, 35) = DYS%(gameIdx, I1)
            End If
        Next

    End If

End Sub



'$INCLUDE: 'GameRoutines_Gfx.bm'

