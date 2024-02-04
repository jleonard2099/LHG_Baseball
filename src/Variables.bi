'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim opSysType$

Dim Shared diskPaths$(0 To 3), Q$(0 To 2056), Q2$(0 To 2056), Q3$(100)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared teamNames$(MAX_TEAMS)


' *** Reading Stat Data ***
' -------------------------                                                                                                                   3
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
Dim expBatCategory$(0 To 31), expPitCategory$(0 To 29), batIndCategory$(0 To 39)
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

Dim BA$(40), ER$(40), statFiles$(40)
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
' Used in RECORDS routines
'----------------------------------------
Dim batRecVal!(32), batRecYr!(32), pitRecVal!(29), pitRecYr!(29)
Dim teamRecAVal!(24), teamRecBVal!(25), teamRecAYr!(24), teamRecBYr!(25)

Dim batRecPlyr$(32), batRecTeam$(32), pitRecPlyr$(29), pitRecTeam$(29)
Dim batIndCategory$(1 To 32), pitIndCategory$(1 To 28)
Dim batTeamCategory$(0 To 20), pitTeamCategory$(0 To 18)
Dim teamRecATeam$(24), teamRecBTeam$(25)


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
'DYS% = days rest; INJ% = injury status
Dim inputDYS%(21), inputINJ%(22), inputTP%(33)
Dim PS%(32), TB%(15)

Dim homeWins(50), awayWins(50), homeLosses(50), awayLosses(50)
Dim THW%(4), TAW%(4), THL(4), TAL(4)
Dim statR1%(200), statR2%(200), statR3%(200), statR4%(200)
Dim TR1%(4), TR2%(4), TR3%(4), TR4%(4)

Dim DV$(4)

Dim ERX!, statI2!

Dim CK
Dim parkHR

'Extra Inning Games
Dim EW, EL
Dim homeWins, homeLosses
'Last 10 Games
Dim LTL, LTW
'One Run Games
Dim ORW, ORL
Dim roadWins, roadLosses
Dim totalWins, totalLosses

Dim D0%, D1%, L2%, W2%

Dim parkType$(3), BS$(22), PS$(21)

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


'Used by POSTGAME
Dim DB, HR, TR, SB
Dim eventNbr
Dim BP(0 To 9), F(0 To 9)

Dim B6%(22, 21), BB%(0 To 9)
Dim gameD1%(1), daysRest(0 To 1, 0 To 21)

Dim gameT1%(0 To 1), triplePlayAdj(0 To 11, 0 To 2)
Dim mgrFile(1)
Dim startPitcher(1), SX%(0 To 1)

Dim E!

Dim grounderDir$(10), PT$(1)


' Shared / Global
Dim Shared badString%, EJ%, gameW%, H6%
Dim Shared IJL%, INNING%, Inotloop%, LYN%
Dim Shared PB%, PC%, SC%, WS%

Dim Shared autoPlay, BV, compTeam, desigHit, D
Dim Shared endGame, endAllGames, FontColAdj
Dim Shared gameLoc, gameTemp
Dim Shared nbrStrikes, noLUFound, normalFinish
Dim Shared P, pbpLine, PPH, pitchEraOpt, playerMode, playerOpt
Dim Shared RE, RD, useInj, useRest, useVGA
Dim Shared strikeoutRating

Dim Shared pbpDelay!, WF!

Dim Shared boxName$, fileString$, gameTime$, gameL$, gameW$
Dim Shared PARK$, pbpString$, wind$, YY$

Dim Shared batRating(0 To 1, 0 To 22, 80), CZ%(0 To 1), inningScore(1, 26)
Dim Shared MON(5, 14), pitchEff(0 To 1, 0 To 1), pitchRating(0 To 1, 0 To 21, 0 To 89)
Dim Shared schedGame(2), tmRat_GAME(1, 13), VV(1), VV1(1)
Dim Shared nbrOuts(1)

Dim SU(1, 1)

Dim Shared A5%(0 To 3), assigned(1 To 9)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(0 To 1, 0 To 9), B4%(1, 7, 9), B5%(0 To 1, 0 To 22, 0 To 21)
Dim Shared B7%(0 To 1, 0 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(0 To 1, 1 To 9, 1 To 9)
Dim Shared CF%(0 To 9, 0 To 2), CSS%(1, 22, 6), D3%(1), DP%(1)
Dim Shared gameB0%(0 To 3, 0 To 2), gameD0%(1), gameM%(0 To 9), gameSA%(1, 24), gameScore(1, 2)
Dim Shared GK%(1), H0%(1), injuryStatus(0 To 1, 0 To 22), L0%(1), LB%(9, 1)
Dim Shared MG%(0 To 1, 999), NG%(18)
Dim Shared P1%(0 To 1), P2%(1), P3%(1, 21), P4%(0 To 22)
Dim Shared P5%(1, 21, 41), P6%(1), P8%(0 To 1, 0 To 21, 0 To 41)
Dim Shared S1%(1), S8%(1, 1), SO%(0 To 5, 0 To 2)
Dim Shared T3%(22), gameTS%(1, 11), X0%(1, 2, 23), YR%(1)

Dim Shared B1!(9), parkHRVals!(1)

Dim Shared alpha$(12), baseName$(0 To 4), batters$(0 To 1, 0 To 22), handed$(4)
Dim Shared diskIDs$(1), doublesPlayer$(0 To 22), ejections$(20), eventDesc$(0 To 22)
Dim Shared fieldPos$(0 To 11)
Dim Shared gameManagers$(1), gameStadiums$(1), gameTeams$(1), gameUmpire$(4)
Dim Shared heldStatus$(1), homeRuns$(0 To 22), injPlayers$(20)
Dim Shared PARK$(99), pitchers$(1, 21), precip$(4), player$(23), pbpStrings$(10)
Dim Shared skyCond$(4), stolenBases$(0 To 22)
Dim Shared teamAbbreviatons$(0 To 1), triplesPlayer$(0 To 22)
Dim Shared windDirection$(10)

