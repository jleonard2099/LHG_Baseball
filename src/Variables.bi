'----------------------------------------
' Used in more than one source file
'----------------------------------------
Dim Shared menu&
Dim opSysType$
Dim Shared dirSeparator$, cfgPath$

Dim Shared diskPaths$(0 To 4), Q$(0 To 2056), Q2$(0 To 2056), Q3$(100)
Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)

' *** Reading Stat Data ***
' -------------------------                                                                                                                   3
Dim leagueName$
Dim div1Name$, div2Name$, div3Name$, div4Name$
Dim teamNameStats$

Dim gameAttendance&(MAX_SCHED_STATS)

Dim oppScore(MAX_SCHED_STATS), teamScore(MAX_SCHED_STATS)
Dim nbrInnings(MAX_SCHED_STATS)

Dim gameSite$(MAX_SCHED_STATS), locIndicator$(MAX_SCHED_STATS), oppName$(MAX_SCHED_STATS)
Dim losePitcher$(MAX_SCHED_STATS), winPitcher$(MAX_SCHED_STATS)

Dim batCategoryBatterAbbr$(0 To 30)
Dim batCategoryPitcherAbbr$(0 To 24)
Dim pitCategoryAbbr$(0 To 26)

Dim calcProperties(0 To 12)

Dim statAtBats(0 To 22), statBatter(0 To 22, 0 To 41)
Dim statInnPitch(0 To 21), statPitcher(0 To 21, 0 To 41)
Dim statSplit(0 To 22, 0 To 6)

'Team Stats
Dim statTeamBat!(0 To 21), statTeamPit!(0 To 22)

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

'-- For Road Data
Dim gameAtt_Road&(MAX_SCHED_STATS)

Dim oppScore_Road(MAX_SCHED_STATS), teamScore_Road(MAX_SCHED_STATS)
Dim nbrInnings_Road(MAX_SCHED_STATS)

Dim oppName_Road$(MAX_SCHED_STATS), locIndicator_Road$(MAX_SCHED_STATS)
Dim losePitcher_Road$(MAX_SCHED_STATS), winPitcher_Road$(MAX_SCHED_STATS)


' *** Schedule Data ***
' -------------------------
Dim BS%, NS%

ReDim scheduleAP%(MAX_SCHEDULE_GAMES), scheduleNG%(MAX_SCHEDULE_GAMES, 20)
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
Dim league_brief$(0 To 1), leagueType$(2)
Dim modeAbbrev$(3), month$(1 To 12)
Dim pitcherEra$(8), playMode$(0 To 3)
Dim teamIndicator$(1)

Dim yesNo$(1), yesNoText$(1)


' *** Miscellaneous Use ***
' -------------------------
Dim Shared backToMenu


'----------------------------------------
'  Used in ALIGN / MERGE routines
'----------------------------------------
Dim teamLosses_Road, teamWins_Road

Dim batNames_Align$(23), batNames_Road$(23)
Dim pitName_Align$(22), pitNames_Road$(22)

'Align
'--> why not using stat.. arrays?
Dim alignB(23), alignB0(23, 22)
Dim alignP(22), alignP0(22, 42)
Dim alignS(22, 6), alignS0(22, 6)

'Road
Dim alignB_Road(23), alignB0_Road(23, 22)
Dim alignP_Road(22), alignP0_Road(22, 42)
Dim alignS_Road(22, 6), alignS0_Road(22, 6)
Dim alignTR(22), alignTR1(23)


'----------------------------------------
'  Used in CAREER / LEADER routines
'----------------------------------------
Dim NB, NP

Dim careerBatters$(0 To 99), careerPitchers$(0 To 79)

Dim carLdrBatName$(18, 20), carLdrPitName$(16, 20)
Dim carLdrBatVal!(18, 20), carLdrPitVal!(16, 21)

Dim careerB0(100, 23), careerB1(100)
Dim careerP0(80, 42), careerP1(80)

Dim carRecBatName$(15), carRecPitName$(15)

Dim carRecBatYear(MAX_CAREER_YEARS), carRecBatStats(MAX_CAREER_YEARS, 100, 17)
Dim carRecBatLdrVal!(15)

Dim carRecPitYear(MAX_CAREER_YEARS), carRecPitStats(MAX_CAREER_YEARS, 80, 18)
Dim carRecPitLdrVal!(15)

Dim plyLeaderTeams$(TEAMS_PER_LEAGUE, 22), tmLeaderTeams$(TEAMS_PER_LEAGUE, 23)


'Expanded Leades
Dim batTotGames!(920), pitTotGames!(880)

Dim expBatCategory$(0 To 31), expPitCategory$(0 To 29), expTeamCategory$(0 To 39)
Dim expBatLdrName$(1 To 920), expPitLdrName$(1 To 880)
Dim expBatLdrTeam$(1 To 920), expPitLdrTeam$(1 To 880)
Dim expLdrTeam$(1 To 920), expLdrPlyrName$(1 To 920)

Dim expLdrB0(1 To 920, 22), expLdrP0(1 To 880, 23)
Dim expLdrVal!(1 To 920), expLdrTYP1!(1 To 920)

'Expanded Standings
Dim expStdLeagName$(TEAMS_PER_LEAGUE)

Dim teamTotLosses(TEAMS_PER_LEAGUE), teamTotWins(TEAMS_PER_LEAGUE)
Dim lastTenWins(TEAMS_PER_LEAGUE), lastTenLosses(TEAMS_PER_LEAGUE)

Dim currLoseStreak(TEAMS_PER_LEAGUE), currWinStreak(TEAMS_PER_LEAGUE), totExtInnLosses(TEAMS_PER_LEAGUE), totExtInnWins(TEAMS_PER_LEAGUE)
Dim totHomeLosses!(TEAMS_PER_LEAGUE), totHomeWins!(TEAMS_PER_LEAGUE), lastLoseStreak(TEAMS_PER_LEAGUE), lastWinStreak(TEAMS_PER_LEAGUE)
Dim totRoadLosses(TEAMS_PER_LEAGUE), totRoadWins(TEAMS_PER_LEAGUE)
Dim teamLoseStreak(TEAMS_PER_LEAGUE), teamWinStreak(TEAMS_PER_LEAGUE)

Dim plyLdrVal!(TEAMS_PER_LEAGUE, 22), teamLdrVal!(TEAMS_PER_LEAGUE, 23)


'----------------------------------------
' Used in COMPILE / SEE routines
'----------------------------------------
Dim divWins(1 To 40), divLosses(1 To 40)

Dim leaderNames$(46)
Dim leaderTeams$(46)
'Dim leaderPlayer$(298)
Dim leaderStatVal2!(46, 42)
Dim leaderVals!(298, 2)

Dim leagLdrPlayer$(298), leagLdrTeam$(298)
Dim leagLdrVal1!(298), leagLdrVal2!(298)

Dim ldrBatStat!(24), ldrPitStat!(23)

Dim BA$(40), ER$(40)

Dim DFT!(40, 20), OFT!(40, 20)


'----------------------------------------
'  Used for DRAFT routine
'----------------------------------------
Dim draftSA(24), draftTS(11)

Dim batRat_DRAFT(22, 79)
Dim pitRat_DRAFT(21, 88)
Dim teamRat_DRAFT(13)

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
Dim tradeT0(1, 22)
Dim tradeLosses(1), tradeWins(1)

Dim tmInfo_TRADE$(1)

Dim tradeTeamName$(1), tradeBatName$(2, 23), tradePitName$(2, 22)
Dim tradeB(2, 23), tradeB0(2, 22, 22)
Dim tradeD0(1), tradeD1(1)
Dim tradeP(2, 22), tradeP0(2, 22, 42)

'-- There are "Dim Shared" equivalents for the game
Dim tradeSA(1, 24), tradeSS(1, 22, 6)
Dim tradeT1(2, 23), tradeTS(1, 11)


'----------------------------------------
'   Used in HD2HD routines
'----------------------------------------
Dim awayScoreTeam!(200), awayScoreOpp!(200)
Dim homeWins(50), awayWins(50)
Dim homeLosses(50), awayLosses(50)
Dim homeScoreTeam!(200), homeScoreOpp!(200)
Dim divTotHomeWins(4), totAwayWins(4)
Dim divTotHomeLosses(4), totAwayLosses(4)
Dim totAwayScoreTeam!(4), totAwayScoreOpp!(4)
Dim totHomeScoreTeam!(4), totHomeScoreOpp!(4)


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
Dim batAvg!, earnedRuns!

Dim parkHR

'Team / Opp Double-Plays
Dim statTeamDP, statTeamDP_Road
Dim statOppDP, statOppDP_Road

Dim teamLosses, teamWins

Dim daysRest(21), daysInjure(22)

Dim totBatRat(15)
Dim totPitchRat(33)
Dim totPitRat&(33)

Dim divisionNames$(4)
Dim parkType$(3)


'----------------------------------------
' Used in PROGMAN routines
'----------------------------------------
Dim AB%(9)
Dim progB3%(0 To 9), progB7%(0 To 9), progB9%(0 To 9)
Dim progM%(0 To 9)

Dim mgrProfileVal(1000)

Dim progR$(999)
Dim progB1$(4)


'----------------------------------------
' Used in POSSRT routines
'----------------------------------------
Dim currPosition
Dim posTeamIdx(TEAMS_PER_LEAGUE)
Dim pitchHand$(22), posTeamYr$(TEAMS_PER_LEAGUE)

'----------------------------------------
' Used in UMPMGR routine
'----------------------------------------
Dim umpireLeague$
Dim umpireYear
Dim leagWalksPerG!, leagRunsPerG!, leagSOsPerG!

Dim umpireName$(MAX_UMPIRES)

Dim umpBBadj(1 To 4), umpSZadj(1 To 4), umpRunAdj(1 To 4)
Dim umpireRating(MAX_UMPIRES, 0 To 2)


'----------------------------------------
' Used in Game Routines
'----------------------------------------
Dim scheduleFile$
Dim actualAttendance&, avgAttendance&

Dim parkAttKey$(1 To 3000)
Dim parkAvgAtt&(1 To 3000)
Dim totalParks

'Used by POSTGAME
Dim DB, HR, TR, SB
Dim errorNbr

Dim bestBatterIdx(0 To 9)
Dim bestBatterFPos(0 To 9), bLineupIdx(0 To 9)
Dim buntTriple(0 To 11, 0 To 2)
Dim gameMgrIdx(0 To 1)
Dim gameDaysRest(0 To 1, 0 To 21)
Dim mgrFile(1)
Dim startPitcher(1), SX%(0 To 1)

Dim grounderDir$(10), pitHand$(1)


' Shared / Global
Dim Shared INNING%

Dim Shared P9, W5, W6

Dim Shared autoPlay, batterMinimumOpt, BV, compTeam, currFielder
Dim Shared desigHit, D, dayOrNight
Dim Shared earlyExit, endAllGames, ejectCnt, FontColAdj
Dim Shared gameLoc, gameOver, gameTemp, ghostRunOpt
Dim Shared inningsPitched
Dim Shared nbrLines, nbrStrikes, noLUFound, normalFinish
Dim Shared P, pbpLine, pinchHitterFlag, pitchEraOpt
Dim Shared playerMode, playerOpt, precipType
Dim Shared skyType, sndOpt, strikeoutRating
Dim Shared useInj, userDone, useRest, useVGA
Dim Shared umpBBadj, umpSZadj, windDir, windSpeed

Dim Shared pbpDelay!, windImpact!

Dim Shared boxName$, chosenPark$, fileString$, gameTime$, gameL$, gameW$
Dim Shared pbpString$, wind$, YY$

Dim setupUsed(1), closerUsed(1)

'Park ratings
Dim monthlyRatings(5, 14)
Dim fenceDistCF
Dim fenceDistLF, fenceDistLCF
Dim fenceDistRF, fenceDistRCF
Dim DOME, turfType, windRating


'baseMove represents how many bases a runner will advance
' 10 = retreat 1 base, but safe
' 11 = retreat 1 base, tagged out
Dim Shared assigned(1 To 9), baseMove(0 To 3)
Dim Shared bRunner(3), b1Pitcher(3), b2Pitcher(3)
Dim Shared batRating(0 To 1, 0 To 22, 80), battersFaced(0 To 1, 0 To 21)
Dim Shared buntBaseHit(0 To 9, 0 To 2), buntLeadRun(9, 1)
Dim Shared buntQuality(0 To 1, 1 To 9, 1 To 9), buntStrike(0 To 5, 0 To 2)
Dim Shared closerZone(0 To 1), CSS(1, 22, 6)
Dim Shared currLineupSlot(9), currPitcher(0 To 1)
Dim Shared fldPos(0 To 1, 0 To 9), fielder(1, 10)
Dim Shared game_batRating(1, 7, 9), gamePitcher(1)
Dim Shared gameScore(1, 2), injuryStatus(0 To 1, 0 To 22)
Dim Shared inningScore(0 To 1, 30)
Dim Shared leftOnBase(1), lineupAssigned(0 To 9), lineupPlayer(0 To 1, 0 To 9)
Dim Shared nbrOuts(1), newRelieverNeeded(1)
Dim Shared oppBatStats(0 To 1, 0 To 22, 0 To 21), oppPitStats(0 To 1, 0 To 21, 0 To 41)
Dim Shared pitcherStam(0 To 1, 0 To 1), pitchRating(0 To 1, 0 To 21, 0 To 89)
Dim Shared pitcherOfRecord(1), playersUsed(1)
'stealPhase is both a % threshold and a coded state flag.
Dim Shared relieverOnRecordForSave(1), runnerOnBase(3), stealPhase(3)
Dim Shared schedGame(2), schedOptions(18), teamRat_GAME(1, 13)
Dim Shared teamBatStats(0 To 1, 0 To 22, 0 To 21), teamPitStats(0 To 1, 0 To 21, 0 To 41)
Dim Shared teamDPs(1), teamMgrProfileVal(0 To 1, 999), teamSplit(1, 11), teamYears(1)
Dim Shared totalDPs(1), totalStats(22)
Dim Shared usedUmpires(MAX_UMPIRES), VV(1), VV1(1)
Dim Shared gameRoster(1, 2, 23)

Dim Shared gameAtBats(1, 22), gameInnPit(1, 21), gameSA(1, 24)

Dim Shared parkHRVals!(1)

Dim Shared alpha$(12), baseName$(0 To 4), batters$(0 To 1, 0 To 22), handed$(4)
Dim Shared diskIDs$(1), doublesPlayer$(0 To 22), ejections$(20), errorsPlayer$(0 To 22)
Dim Shared fieldPos$(0 To 11)
Dim Shared gameManagers$(1), gameStadiums$(1), gameTeams$(1), gameUmpire$(1 To 4)
Dim Shared genericNames$(4), heldStatus$(1), homeRuns$(0 To 22), injPlayers$(20)
Dim Shared parkNames$(1 To 99), pitchers$(1, 21), precip$(4), player$(23), pbpText$(1 To MAX_PBP_LINES)
Dim Shared skyCond$(4), stolenBases$(0 To 22)
Dim Shared teamAbbreviatons$(0 To 1), triplesPlayer$(0 To 22)
Dim Shared windDirection$(10)

