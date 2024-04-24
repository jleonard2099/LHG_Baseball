Declare Sub ACTIVATE ()
Declare Sub ALIGN ()
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub AnalyzePlayerUsage (targetTeam$)
Declare Sub BASERUN (teamYear$, mgrNbr)
Declare Sub BatterTotals ()
Declare Sub BLANK ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CalcStatTotals (WT$, totalGames)
Declare Sub CalculateBuntRating ()
Declare Sub CalculateRunRating ()
Declare Sub Calculations_Menu ()
Declare Sub CAREDIT ()
Declare Sub CAREER ()
Declare Sub CareerLeaderOptions ()
Declare Sub CareerRecords_Report (printDest, destFile$)
Declare Sub CareerToPrinter (printDest, destFile$, careerFile$)
Declare Sub CareerToScreen (careerFile$)
Declare Sub CARLDR ()
Declare Sub CARVIEW ()
Declare Sub ChangeBatters ()
Declare Sub ChangeGlobalSettings ()
Declare Sub ChangeLineup_PreGame (P9, reselect)
Declare Sub ChangeLineup_PROGMAN (yearNumber$, I$)
Declare Sub ChangePitchers ()
Declare Sub ChangeSchedule (teamName$)
Declare Sub CheckStatsLeague (targetTeam$, WT$, totalGames)
Declare Sub ChooseDestTeam (yearNumber$, numberTeams, chosenIdx, teamName$)
Declare Sub ChooseSourceTeam (yearNumber$, numberTeams, chosenIdx, teamName$, draftIdx)
Declare Sub ChooseTradeTeam (yearNumber$, numberTeams, chosenIdx, teamName$, tradeIdx)
Declare Sub CNGTM (userChoice)
Declare Sub COMPARE ()
Declare Sub Comparisons_Report (printDest, destFile$, teamName$)
Declare Sub Comparisons_Screen (teamName$, backToMenu)
Declare Sub CompileLeague ()
Declare Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)
Declare Sub ContinueTrade (yearNumbers$())
Declare Sub CreateCareerFile ()
Declare Sub CreateSeasonSchedule ()
Declare Sub CREATLEAG ()
Declare Sub DefenseLateInning (yearNumber$, T$, mgrNbr)
Declare Sub DefenseSubPinchRun (yearNumber$, targetTeam$, mgrNbr)
Declare Sub DefensiveStrategy (yearNumber$, mgrNbr)
Declare Sub DefineLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub DeleteBoxscores (fileChoice%, fileSpec$)
Declare Sub DisplayBattingLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayPitchingLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (XX, screenType, printOrView, TVN, maxRecords)
Declare Sub DoSort (B, C, P9)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DRAFT ()
Declare Sub DraftChoices (draftNew, doneDrafting, teamName$, teamYears$(), teamIdx)
Declare Sub DraftPlayer (playerType, draftPos)
Declare Sub EditBatter ()
Declare Sub EditBatters (careerFile$)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame (DT$, TM$)
Declare Sub EditLineups (handRL%, targetTeam$, yearNumber$)
Declare Sub EditOneLineup (targetTeam$, yearNumber$)
Declare Sub EditParameters ()
Declare Sub EditPitcher ()
Declare Sub EditPitchers (careerFile$)
Declare Sub EditStats ()
Declare Sub EditStatsSchedules (action%)
Declare Sub EditTeamInfo (YN$, T$, teamIdx)
Declare Sub EditUmpireFile ()
Declare Sub EnterUmpireFile ()
Declare Sub ExpandedLeaders_Menu (leaderFile$, JB, JP, tStats, ttStats)
Declare Sub ExpandedStandings ()
Declare Sub ExportManagerProfile (mgrNbr)
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub FieldingStats (teamName$)
Declare Sub GetLineup (handRL%)
Declare Sub GetLineupPosition (I1, I$)
Declare Sub GetListLength (printOrView, listLength, maxItems)
Declare Sub GetManagerProfile (targetTeam$, yearNumber$, mgrNbr)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub GROUND ()
Declare Sub HD2HD ()
Declare Sub Hd2HdPro_League ()
Declare Sub Hd2HdPro_Report (teamName$, printDest)
Declare Sub Hd2HdPro_Screen (teamName$)
Declare Sub HitAndRun (yearNumber$, mgrNbr)
Declare Sub HittingStrategy (yearNumber$, mgrNbr)
Declare Sub ImportManagerProfile (yearNumber$, mgrNbr)
Declare Sub InputDeleteLineup (D%, yearNumber$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, targetTeam$, teamIdx)
Declare Sub InputPitchStrat (yearNumber$, mgrNbr)
Declare Sub Leaders_Menu ()
Declare Sub LeagueStats_Report (league$, div1Name$, div2Name$, printDest, destFile$)
Declare Sub LeagueStats_Screen (league$, div1Name$, div2Name$)
Declare Sub LeagueTotals_A_Screen (XX, TVN, maxRecords)
Declare Sub LeagueTotals_B_Screen (XX, TVN, maxRecords)
Declare Sub LineupFromProfile (lineupFound%, P9, skipLineChange%)
Declare Sub LineupPositions (teamIdx, posIdx)
Declare Sub LineupStrategy (yearNumber$, targetTeam$)
Declare Sub LineupSummary (P9, reselect, LU%)
Declare Sub LOADER
Declare Sub LoadLeaderFiles (league$, div1Name$, div2Name$, targetFile$)
Declare Sub LoadLRForTrade (teamYear$, tradeIdx)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (yearNumbers$(), sourceIdx)
Declare Sub LoadTeam (targetTeam$, SS, statFile$)
Declare Sub LoadTeamStats (statFile$, nbrGames)
Declare Sub LOOKY ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub ManagePitchingRotations (action%, targetTeam$, mgrNbr)
Declare Sub MERGE ()
Declare Sub MergeAddYear (selectedFile$, statFile$)
Declare Sub MergeCareerFiles ()
Declare Sub MergeFirstYear ()
Declare Sub MergeStats ()
Declare Sub ModifyTeamStats (Z$)
Declare Sub NewGame (visitor$, home$, YN$, DT$, TM$, NTMS, gameIndex%)
Declare Sub NEWLDR ()
Declare Sub NewPrintStats (printDest, destFile$, statFile$, teamName$, nbrGames, printChoice, splitStatChoice)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandings_Report (printDest, destFile$, league$)
Declare Sub NewStandings_Screen (league$)
Declare Sub NewTeamParameters (draftYear$, teamName$)
Declare Sub PHBATTER (yearNumber$, mgrNbr)
Declare Sub PHPITCHER (yearNumber$, mgrNbr)
Declare Sub PinchRunnerOptions (yearNumber$, targetTeam$, mgrNbr)
Declare Sub PinchRunStrategy (yearNumber$, targetTeam$, mgrNbr)
Declare Sub PITCHAROUND (yearNumber$, mgrNbr)
Declare Sub Pitchers10_Games (PC, pitchPosnDesc$, positionFile$)
Declare Sub Pitchers10_Print (printDest, destFile$, PC, pitchPosnDesc$, positionFile$)
Declare Sub PitchersHanded (THR, pitchPosnDesc$, positionFile$)
Declare Sub PitchersHanded_Print (printDest, destFile$, pitchPosnDesc$)
Declare Sub PitcherTotals ()
Declare Sub PitchingRotations (computerRotations%, P9)
Declare Sub PitchingStarter (idx%)
Declare Sub PITCHINIT (i, FG, BG)
Declare Sub POSCR ()
Declare Sub POSSRT ()
Declare Sub PostLeaders (league$, leaderFile$, APP)
Declare Sub PrintBatterStats (printDest, destFile$, careerFile$, playerIdx)
Declare Sub PrintCareerLeaders (printDest, destFile$)
Declare Sub PrintLeaders (printDest, destFile$, TVN, maxRecords, categoryID)
Declare Sub PrintManagerProfile (printDest, destFile$, targetTeam$)
Declare Sub PrintPitcherInfo (idx%)
Declare Sub PrintPitcherStats (printDest, destFile$, careerFile$, playerIdx)
Declare Sub PrintSeasonRecords (printDest, destFile$, leagueName$)
Declare Sub PrintTeamStats (printDest, destFile$, targetTeam$)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub PROGMAN ()
Declare Sub ReadDestTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub READLEADER (leaderFile$, league$, JB, JP, tStats, ttStats)
Declare Sub ReadSourceTeam (yearNumber$, targetTeam$, teamIdx)
Declare Sub ReadStatsLeague (statFile$, teamName$, totalGames)
Declare Sub ReadTradeTeam (teamYear$, targetTeam$, teamIdx, tradeIdx)
Declare Sub Replay_Menu ()
Declare Sub ReportsAllTeams ()
Declare Sub ResetStatPrompt (statFile$, teamRatings%())
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewCareerRecords ()
Declare Sub ReviewEditLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub RotationParams (targetTeam$, mgrNbr)
Declare Sub RotationReliefMenu (yearNumber$, targetTeam$, mgrNbr)
Declare Sub SAVELEADER (leaderFile$, league$, JB, JP, tStats, ttStats)
Declare Sub SaveLeague (league$, div1$, div2$, div3$, div4$)
Declare Sub SaveLineup (idx%)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (draftNew, yearNumber$, teamName$, teamIdx)
Declare Sub SaveTeamFile (newID, yearNumber$, targetTeam$, targetFile$, teamIdx)
Declare Sub SaveTrades (yearNumbers$())
Declare Sub SCHEDULE ()
Declare Sub SearchForLineup (P9)
Declare Sub SearchPlayers ()
Declare Sub SearchResults_Report (printDest, destFile$, posnType, posnDesc$, positionFile$)
Declare Sub SeasonRecords ()
Declare Sub SEELEAG ()
Declare Sub SelectBatters (batterFlag%, P9)
Declare Sub SelectDraftTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, draftIdx)
Declare Sub SelectPinchHitters (yearNumber$, targetTeam$, mgrNbr)
Declare Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)
Declare Sub SelectStadium (useVGA)
Declare Sub SelectTradeTeam(numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, tradeIdx)
Declare Sub SetupClosers (targetTeam$, yearNumber$, mgrNbr)
Declare Sub SetupMiddleRelievers (targetTeam$, yearNumber$, mgrNbr)
Declare Sub SetupRelievers (targetTeam$, yearNumber$, mgrNbr)
Declare Sub ShowPitcherInfo (targetTeam$)
Declare Sub SortHitters (P9)
Declare Sub SortOptions (printOrView, sortType, reselect)
Declare Sub SortPitchers (positionFile$)
Declare Sub SortPlayer (PPP, stpl, sortType)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortStandingsLeague
Declare Sub SortTeam (sortMax, stpl, sortType)
Declare Sub SpotStarters (targetTeam$, yearNumber$, mgrNbr)
Declare Sub StartingLineup (P9)
Declare Sub StatOptions (league$, div1Name$, div2Name$)
Declare Sub StatsOneTeam ()
Declare Sub SwapSpots (yearNumber$, targetTeam$, teamIdx)
Declare Sub SwitchBatters ()
Declare Sub SwitchPitchers ()
Declare Sub TeamComparison_Report (printDest, destFile$, teamName$)
Declare Sub TeamComparison_Screen (teamName$)
Declare Sub TeamMgt_Menu ()
Declare Sub THROWHOME (yearNumber$, mgrNbr)
Declare Sub TRADE ()
Declare Sub TradeBatters ()
Declare Sub TradePitchers ()
Declare Sub TradeSummary_Batters (p1idx, p2idx)
Declare Sub TradeSummary_Pitchers (p1idx, p2idx)
Declare Sub UMPMGR ()
Declare Sub UpdateCareerLeaders (recordFile$)
Declare Sub UpdateExpandedLeaders (JB, JP, tStats, ttStats)
Declare Sub UpdateExpandedStandings (league$)
Declare Sub UpdateLeaders ()
Declare Sub UpdateManagerFile (teamYear$, mgrNbr)
Declare Sub UpdateRecords (leagueFile$)
Declare Sub ViewAndPrintOptions (statFile$, teamName$, nbrGames, menuExit)
Declare Sub ViewBattingLeaders (leaderFile$, JB, JP, tStats, ttStats)
Declare Sub ViewCareerLeaders ()
Declare Sub ViewExpandedStats (JB, JP, tStats, ttStats)
Declare Sub ViewHitters (teamName$)
Declare Sub ViewHittingFielding ()
Declare Sub ViewLeaders_Batters (careerFile$)
Declare Sub ViewLeaders_Pitchers (careerFile$)
Declare Sub ViewLeaders_Sequence (careerFile$)
Declare Sub ViewLineup (targetTeam$)
Declare Sub ViewPitchers (teamName$)
Declare Sub ViewPitchingLeaders (leaderFile$, JB, JP, tStats, ttStats)
Declare Sub ViewRoster (targetTeam$)
Declare Sub ViewSearchResults (posnType, posnDesc$, positionFile$)
Declare Sub ViewSeasonRecords (leagueFile$)
Declare Sub ViewStatSummary (statFile$, teamName$, splitStatChoice)
Declare Sub ViewTeamLeaders (leaderFile$, JB, JP, tStats, ttStats)

Declare Function FindStatsPro% (statFile$, teamName$, fullWins, fullLosses)
