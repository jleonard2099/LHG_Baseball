
Dim opSysType$

Dim Shared diskPaths$(0 To 4), Q$(0 To 2056), Q2$(0 To 2056), Q3$(100)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------                                                                                                                   3
Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)
Dim nbrInnings(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)
Dim losePitcher$(MAX_SCHED_STATS), winPitcher$(MAX_SCHED_STATS)

Dim div1Name$, div2Name$, div3Name$, div4Name$

Dim statB%(0 To 22), statB0%(0 To 22, 0 To 41)
Dim statP%(0 To 21), statP0%(0 To 21, 0 To 41)
Dim statSS%(0 To 22, 0 To 6)
Dim statT0%(0 To 21), statT1%(0 To 22)

Dim statT0!(22), statT1!(22)

'Record / Standings
Dim homeWins, homeLosses
Dim roadWins, roadLosses
Dim totalWins, totalLosses

'-- Extra Inning Games
Dim extraInnWins, extraInnLosses

'-- Last 10 Games
Dim lastTenLosses, lastTenWins

'-- One Run Games
Dim oneRunWins, oneRunLosses


' Conferences / Orgs
Dim memberIdx(TEAMS_PER_LEAGUE)
Dim memberName$(TEAMS_PER_LEAGUE)
Dim memberYear$(TEAMS_PER_LEAGUE)
Dim statFiles$(TEAMS_PER_LEAGUE)

'-- For Road Data
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)
Dim nbrInnings_Road(MAX_SCHED_STATS)

Dim oppName_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS)
Dim losePitcher_Road$(MAX_SCHED_STATS), winPitcher_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 18)
ReDim homeScores(MAX_SCHEDULE_GAMES), visitorScores(MAX_SCHEDULE_GAMES)

ReDim scheduleH$(MAX_SCHEDULE_GAMES), scheduleV$(MAX_SCHEDULE_GAMES)
'ReDim homeTeam$(MAX_SCHEDULE_GAMES), visitingTeam$(MAX_SCHEDULE_GAMES)
ReDim scheduleYN$(MAX_SCHEDULE_GAMES, 1)
'ReDim yearNumber$(MAX_SCHEDULE_GAMES, 1)


' *** Game Options ***
' -------------------------
Dim Shared DT$, TM$

Dim eventSettings(13)

Dim dayNight$(0 To 1), desigHitOpt$(1), gameType$(1)
Dim leagueType$(2), modeAbbrev$(3), month$(1 To 12)
Dim pitcherEra$(8), playMode$(0 To 3)
Dim teamIndicator$(1), umpireName$(50)
Dim yesNo$(1), yesNoText$(1)

Dim umpireNumber(50)

' *** Miscellaneous Use ***
' -------------------------
Dim Shared backToMenu


'----------------------------------------
'  Used in ALIGN / MERGE routines
'----------------------------------------
Dim LR2%, WR2%

Dim alignBatterName$(23), batterNames_Road$(23)
Dim alignPitcherName$(22), pitcherNames_Road$(22)

Dim BN%(23), B0N%(23, 22)
Dim PN%(22), P0N%(22, 42)
Dim S0%(22, 6), S0N%(22, 6)

Dim BR%(23), B0R%(23, 22)
Dim PR%(22), P0R%(22, 42)
Dim SR%(22, 6), S0R%(22, 6)

Dim draftTR(22), draftTR1(23)


'----------------------------------------
'  Used in CAREER / LEADER routines
'----------------------------------------
Dim NB, NP

Dim carLdrBatName$(18, 20), carLdrPitName$(16, 20)
Dim careerBL$(15), careerPL$(15)
Dim careerBatters$(0 To 99), careerPitchers$(0 To 79)

Dim BLYR%(MAX_CAREER_YEARS), BRS%(MAX_CAREER_YEARS, 100, 17)
Dim PLYR%(MAX_CAREER_YEARS), PRS%(MAX_CAREER_YEARS, 80, 18)

Dim careerB0%(100, 23), careerB1%(100)
Dim careerP0%(80, 42), careerP1%(80)

Dim BLL!(15), PLL!(15)
Dim carLdrBatVal!(18, 20), carLdrPitVal!(16, 21)

Dim leaderBL$(1 To 920), leaderPL$(1 To 880)

Dim expBatCategory$(0 To 31), expPitCategory$(0 To 29), expTeamCategory$(0 To 39)
Dim TMB$(1 To 920), TMM$(1 To 920), TMP$(1 To 880), TPP$(1 To 920)

Dim plyLeaderTeams$(TEAMS_PER_LEAGUE, 22), tmLeaderTeams$(TEAMS_PER_LEAGUE, 23)

Dim BK%(1 To 920, 22), PM%(1 To 880, 23)

Dim TYP!(1 To 920), TYP1!(1 To 920)

Dim GM1!(920), GM2(880)

'Expanded Standings
Dim CLT!(TEAMS_PER_LEAGUE), CWS!(TEAMS_PER_LEAGUE), ELL!(TEAMS_PER_LEAGUE), EWW!(TEAMS_PER_LEAGUE)
Dim HLL!(TEAMS_PER_LEAGUE), HWW!(TEAMS_PER_LEAGUE), LLS!(TEAMS_PER_LEAGUE), LWS!(TEAMS_PER_LEAGUE)
Dim expLL!(TEAMS_PER_LEAGUE), lastTenWins!(TEAMS_PER_LEAGUE), lastTenLosses!(TEAMS_PER_LEAGUE)
Dim RLL!(TEAMS_PER_LEAGUE), RWW!(TEAMS_PER_LEAGUE)
Dim TLS!(TEAMS_PER_LEAGUE), TWS!(TEAMS_PER_LEAGUE), TT!(TEAMS_PER_LEAGUE, 22), TT1!(TEAMS_PER_LEAGUE, 23)
Dim WW!(TEAMS_PER_LEAGUE)

Dim TML$(TEAMS_PER_LEAGUE)


'----------------------------------------
'   Used in COMPARE routines
'----------------------------------------
Dim TP&(33)


'----------------------------------------
'   Used in COMPILER routines
'----------------------------------------
Dim BAT%(24), PIT%(23)
Dim O1(1 To 40), O2(1 To 40)
Dim Wins(40), Losses(40)

Dim compileZ1!(298, 2)
Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)
Dim seeZ!(298), seeZ1!(298)

Dim BA$(40), ER$(40)
Dim compileZ2$(298), compileZ3$(298)
Dim LF$(70), NM$(46), seeP$(298)
Dim RT$(70), seeT$(40), compileTeams$(1 To 40)


'----------------------------------------
'  Used for DRAFT routine
'----------------------------------------
Dim draftSA%(24), draftTS%(11)

Dim batRat_DRAFT%(22, 79)
Dim teamRat_DRAFT(13)
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
Dim teamIndexes(0 To 1), teamRat_TRADE(1, 14)

Dim batNam_TRADE$(2, 23), pitNam_TRADE$(2, 22)
Dim mgrs_TRADE$(2), stads_TRADE$(2), tmAbbrev_TRADE$(2)


' ** Stat File **
Dim tradeL2%(1), TEAM%(1, 22)

Dim tmInfo_TRADE$(1), A1$(1)
Dim tradeB1$(2, 23), tradeP1$(2, 22)

Dim tradeB0%(2, 22, 22), tradeB3%(2, 23)
Dim tradeD0%(1), tradeD1%(1)

Dim tradeP0%(2, 22, 42), tradeP3%(2, 22)

'-- There are "Dim Shared" equivalents for the game
Dim tradeSA%(1, 24), tradeSS%(1, 22, 6)

Dim tradeT1%(2, 23), tradeTS%(1, 11)
Dim tradeW2%(1)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim homeWins(50), awayWins(50)
Dim homeLosses(50), awayLosses(50)
Dim totHomeWin(4), totAwayWin(4)
Dim totHomeLosses(4), totAwayLosses(4)
Dim homeScoreTeam(200), homeScoreOpp(200)
Dim awayScoreTeam(200), awayScoreOpp(200)
Dim totHomeScoreTeam(4), totHomeScoreOpp(4)
Dim totAwayScoreTeam(4), totAwayScoreOpp(4)


'----------------------------------------
' Used in RECORD routines
'----------------------------------------
Dim batRecVal!(32), batRecYr!(32), pitRecVal!(29), pitRecYr!(29)
Dim batRecTmVal!(24), pitRecTmVal!(25), batRecTmYr!(24), pitRecTmYr!(25)

Dim batRecPlyr$(32), batRecTeam$(32), pitRecPlyr$(29), pitRecTeam$(29)
Dim batIndCategory$(1 To 32), pitIndCategory$(1 To 28)
Dim batTeamCategory$(0 To 20), pitTeamCategory$(0 To 18)
Dim batRecTmDesc$(24), pitRecTmDesc$(25)


'----------------------------------------
' Used in STAT / INPUT other routines
'----------------------------------------
Dim inputBS%(22)

'DYS% = days rest; INJ% = injury status
Dim inputDYS%(21), inputINJ%(22), inputTP%(33)

Dim PS%(32), TB%(15)

Dim DV$(4)

Dim ERX!, statI2!

Dim parkHR

Dim D0%, D1%, L2%, W2%

Dim parkType$(3), BS$(22), PS$(21)


'----------------------------------------
' Used in SEExxx routines
'----------------------------------------


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
Dim TM(TEAMS_PER_LEAGUE)

Dim A$(TEAMS_PER_LEAGUE)
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
Dim Shared schedGame(2), teamRat_GAME(1, 13), VV(1), VV1(1)
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

