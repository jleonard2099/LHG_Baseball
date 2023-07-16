Declare Sub ACTIVATE ()
Declare Sub ALIGN ()
Declare Sub AlignStats (pathRoadStats$, statFile$)
Declare Sub AnalyzePlayerUsage (targetTeam$)
Declare Sub BASERUN (yearNumber$)
Declare Sub BatterTotals ()
Declare Sub BLANK ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CalculateBuntRating ()
Declare Sub CalculateRunRating ()
Declare Sub CAREDIT ()
Declare Sub CAREER ()
Declare Sub CareerToPrinter (careerFile$)
Declare Sub CareerToScreen (careerFile$)
Declare Sub CARLDR ()
Declare Sub CARVIEW ()
Declare Sub ChangeBatters (Z$)
Declare Sub ChangeGlobalSettings ()
Declare Sub ChangeLineup_PROGMAN (yearNumber$, I$)
Declare Sub ChangePitchers (D0%, D1%, L2%, W2%, Z$)
Declare Sub ChangePlayers (yearNumber$, teamName$, teamIdx)
Declare Sub ChangeSchedule (Z$)
Declare Sub CheckStatsLeague (targetTeam$, WT$)
Declare Sub CheckTeam_Schedule (teamIdx, yearNumber$)
Declare Sub ChooseLeague (league$)
Declare Sub ChooseTeam (yearNumber$, numberTeams, teamIdx, teamName$)
Declare Sub COMPARE ()
Declare Sub Comparisons_Printer (teamName$)
Declare Sub Comparisons_Screen (teamName$, backToMenu)
Declare Sub COMPLEAG ()
Declare Sub ConfigureDrives ()
Declare Sub ContinueTrade (yearNumbers$(), destIdx, noStats)
Declare Sub CREATE ()
Declare Sub CreateCareerFile ()
Declare Sub CreateSeasonSchedule ()
Declare Sub DefenseLateInning (yearNumber$, T$)
Declare Sub DefenseSubPinchRun (targetTeam$)
Declare Sub DefensiveStrategy (yearNumber$)
Declare Sub DefineLeague(LG$, D1$, D2$, D3$, D4$)
Declare Sub DeleteSchedules (fileChoice%, fileSpec$)
Declare Sub DisplayBattingLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayPitchingLeaders (TVN, maxRecords, categoryId)
Declare Sub DisplayTeamView (XX, screenType, printOrView%, TVN, maxRecords, categoryId)
Declare Sub DoTheMerge (pathCurrentStats$, statFile$, B$)
Declare Sub DRAFT ()
Declare Sub DraftChoices (DNT, doneDrafting, teamName$, teamYears$(), teamIdx)
Declare Sub DraftExistingTeam (teamYear$, teamName$, teamIdx)
Declare Sub DraftNewTeam (draftYear$, teamName$)
Declare Sub DraftPlayer (playerType, yearNumber$, draftPos)
Declare Sub EditBatter ()
Declare Sub EditBatters (careerFile$)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditLineups (handRL%, targetTeam$, yearNumber$)
Declare Sub EditOneLineup (targetTeam$, yearNumber$)
Declare Sub EditParameters ()
Declare Sub EditPitcher ()
Declare Sub EditPitchers (careerFile$)
Declare Sub EditStatsSchedules (action%)
Declare Sub EditTeamInfo (YN$, T$, NN$, S$, M$, teamIdx)
Declare Sub EditUmpireFile ()
Declare Sub EnterUmpireFile ()
Declare Sub ExpandedLeaders_Menu (leagueFile$, JB, JP, TS, TTS)
Declare Sub ExpandedStandings ()
Declare Sub ExportManagerProfile ()
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub FieldingStats ()
Declare Sub GetDiskID_ALIGN (A$)
Declare Sub GetDiskID_MERGE (A$)
Declare Sub GetDiskYear (targetTeam$, yearNumber$, teamidx)
Declare Sub GetLineup (handRL%, targetTeam$)
Declare Sub GetLineupPosition (I1, I$)
Declare Sub GetListLength (printOrView%, listLength, maxItems)
Declare Sub GetMergePaths (pathCurrentStats$, pathRoadStats$, promptResponse$)
Declare Sub GetTeam (yearNumber$, numberTeams, teamIndex%)
Declare Sub GetTeams_Activate (yearNumber$, numberTeams)
Declare Sub GetTeam_BINPUT (T$, yearNumber$, numberTeams, teamIdx)
Declare Sub GetTeam_CAREER (teamIdx, A$)
Declare Sub GetTeam_CREATE (teamName$, yearNumber$, teamIdx)
Declare Sub GetTeam_GROUND (yearNumber$, numberTeams, teamName$, teamIdx)
Declare Sub GetTeam_HD2HD (yearNumber$, numberTeams, teamName$)
Declare Sub GetTeam_PROGMAN (targetTeam$, yearNumber$)
Declare Sub GetTeam_Schedule (numberTeams, yearNumber$, teamIdx)
Declare Sub GetTeam_SINPUT (T$, yearNumber$, numberTeams)
Declare Sub GetTeam_TRADE (yearNumber$, tradeIdx)
Declare Sub GROUND ()
Declare Sub HD2HD ()
Declare Sub Hd2Hd_League (LG$, sameLeague)
Declare Sub Hd2Hd_Printer (teamName$)
Declare Sub Hd2Hd_Screen (teamName$)
Declare Sub HitAndRun (yearNumber$)
Declare Sub HittingStrategy (yearNumber$)
Declare Sub ImportManagerProfile ()
Declare Sub InputDeleteLineup (D%, yearNumber$)
Declare Sub InputEditTeam (inputAction%, yearNumber$, targetTeam$, teamIdx)
Declare Sub Input_PITSTRAT (yearNumber$)
Declare Sub Leaders_Menu ()
Declare Sub LeagueStatsToScreen (LG$, D1$, D2$)
Declare Sub LeagueTotals_A_Screen (XX, TVN, maxRecords)
Declare Sub LeagueTotals_B_Screen (XX, TVN, maxRecords)
Declare Sub LineupStrategy (targetTeam$)
Declare Sub LoadLeaderFiles (LG$, D1$, D2$, targetFile$)
Declare Sub LoadSchedule (targetFile$)
Declare Sub LoadScheduleInfo (targetFile$)
Declare Sub LoadStatsForTrade (teamYear$, tradeIdx)
Declare Sub LoadTeam (targetTeam$, teamHasStats, SS, targetFile$)
Declare Sub LoadTeamStats (statFile$, totalGames%)
Declare Sub LOOKY ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub ManagePitchingRotations (action%, targetTeam$)
Declare Sub MERGE ()
Declare Sub MergeCareerFiles ()
Declare Sub MergeStats ()
Declare Sub Merge_AddYear ()
Declare Sub Merge_FirstYear ()
Declare Sub ModifyTeamStats (Z$)
Declare Sub NewGame (visitor$, home$, YN$, NTMS, gameIndex%)
Declare Sub NEWLDR ()
Declare Sub NewPrintStats (statFile$, teamName$, totalGames%, printChoice, splitScreenChoice%, menuExit)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub NewStandingOptions ()
Declare Sub PHBATTER (yearNumber$)
Declare Sub PHPITCHER (yearNumber$)
Declare Sub PinchRunners (targetTeam$)
Declare Sub PinchRunStrategy (targetTeam$)
Declare Sub PITCHAROUND (yearNumber$)
Declare Sub Pitchers10_Games (PC, PPS$, LGE$)
Declare Sub Pitchers10_Print (PC, PPS$, LGE$)
Declare Sub PitchersHanded (THR, PPS$, LGE$)
Declare Sub PitcherTotals ()
Declare Sub PITCHINIT (i, FG, BG)
Declare Sub POSCR ()
Declare Sub POSSRT ()
Declare Sub PostLeaders (LG$, WE$, APP)
Declare Sub PrintBatterStats (careerFile$, playerIdx)
Declare Sub PrintLeaders (TVN, maxRecords, B9)
Declare Sub PrintLeaders_Screen ()
Declare Sub PrintManagerProfile (targetTeam$)
Declare Sub PrintNewStandings (LGF$)
Declare Sub PrintPitcherStats (careerFile$, playerIdx)
Declare Sub PrintSearchResults (PS, PS$, LGE$)
Declare Sub PrintSeasonRecords (leagueFile$)
Declare Sub PrintTeamStats (targetTeam$, teamIdx)
Declare Sub PrintViewReselect (userOpt$)
Declare Sub PROGMAN ()
Declare Sub ReadStatsLeague (statFile$, TN$, fullWins, fullLosses)
Declare Sub ReadTeam (teamYear$, targetTeam$, teamIdx)
Declare Sub ReadTeamData_COMPARE (yearNumber$, numberTeams)
Declare Sub ReadTeam_A (yearNumber$, teamIdx)
Declare Sub ReadTeam_B (yearNumber$, teamIdx)
Declare Sub ReadTeam_POSCR (yearNumber$, teamIndex%)
Declare Sub ReadTeam_PosSrt (teamIndex%, yearNumber$)
Declare Sub Replay_Menu ()
Declare Sub ReportFilePrompt_League (reportFile$)
Declare Sub ReportsAllTeams ()
Declare Sub ResetStatPrompt (statFile$, teamRatings%())
Declare Sub ReviewAndSaveAlignment (pathRoadStats$, statFile$)
Declare Sub ReviewEditLeague (league$, D1$, D2$, D3$, D4$)
Declare Sub RotationParams (targetTeam$)
Declare Sub RotationReliefMenu (targetTeam$)
Declare Sub SAVELEADER (league$, leagueFile$, JB, JP, TS, TTS)
Declare Sub SaveLeague (LG$, D1$, D2$, D3$, D4$)
Declare Sub SavePrintLeagueStats (LG$, D1$, D2$, destination)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (DNT, yearNumber$, teamName$, teamIdx)
Declare Sub SaveTeamFile (newID%, yearNumber$, targetTeam$, targetFile$, teamIdx)
Declare Sub SaveTrades (yearNumbers$())
Declare Sub SCHEDULE ()
Declare Sub SearchPlayers ()
Declare Sub SeasonRecords ()
Declare Sub SEELEAG ()
Declare Sub SelectLeague (targetFile$)
Declare Sub SelectLeague_REC (leagueFile$, ExitCode%)
Declare Sub SelectPinchHitters (yearNumber$, targetTeam$)
Declare Sub SelectScheduleFile (targetFile$, scheduleFiles$(), FileCount%, ExitCode%)
Declare Sub SelectTeam (numberTeams, yearNumber$, selectedIdx, teamName$)
Declare Sub SelectTeam_ACTIVATE (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_ALIGN (numberTeams, yearNumber$)
Declare Sub SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_CAREER (yearNumber$, numberTeams, teamIdx%)
Declare Sub SelectTeam_COMPARE (yearNumber$, numberTeams, T$)
Declare Sub SelectTeam_CREATE (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_DRAFT (draftYear$, numberTeams%, teamNumber, teamIdx)
Declare Sub SelectTeam_GROUND (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_HD2HD (yearNumber$, numberTeams)
Declare Sub SelectTeam_LOOKY (numberTeams, yearNumber$)
Declare Sub SelectTeam_MERGE (yearNumber$, numberTeams)
Declare Sub SelectTeam_POSCR (yearNumber$, numberTeams, teamIndex%)
Declare Sub SelectTeam_PROGMAN (numberTeams, yearNumber$, T)
Declare Sub SelectTeam_Schedule (numberTeams, yearNumber$)
Declare Sub SelectTeam_SINPUT (T$, yearNumber$, numberTeams)
Declare Sub SelectTeam_TRADE (yearNumber$, numberTeams, tradeIdx)
Declare Sub SetupClosers (targetTeam$, yearNumber$)
Declare Sub SetupMiddleRelievers (targetTeam$, yearNumber$)
Declare Sub SetupRelievers (targetTeam$, yearNumber$)
Declare Sub ShowPitcherInfo (targetTeam$)
Declare Sub SINPUT ()
Declare Sub SortLeaders
Declare Sub SortOptions (printOrView%, sortType%)
Declare Sub SortPitchers (LGE$)
Declare Sub SortPlayer (PPP, stpl, sortType%)
Declare Sub SortSchedule ()
Declare Sub SortStandings (AA, ZZ)
Declare Sub SortTeam (sortMax, stpl, sortType%)
Declare Sub SpotStarters (targetTeam$, yearNumber$)
Declare Sub StartTrade (yearNumbers$(), sourceIdx, noStats)
Declare Sub StatOptions (LG$, D1$, D2$)
Declare Sub StatsByCategory (WT$, fullWins, fullLosses)
Declare Sub StatsOneTeam ()
Declare Sub SwapSpots (yearNumber$, targetTeam$, teamIdx)
Declare Sub SwitchBatters ()
Declare Sub SwitchPitchers ()
Declare Sub TeamComparison_Printer (teamName$)
Declare Sub TeamComparison_Screen (teamName$)
Declare Sub THROWHOME (yearNumber$)
Declare Sub TINPUT ()
Declare Sub TRADE ()
Declare Sub TradeBatters ()
Declare Sub TradePitchers ()
Declare Sub TradeSummary_Batters (p1idx, p2idx)
Declare Sub TradeSummary_Pitchers (p1idx, p2idx)
Declare Sub UMPMGR ()
Declare Sub UpdateCareerLeaders (leaderFile$)
Declare Sub UpdateExpandedLeaders (JB, JP, TS, TTS)
Declare Sub UpdateLeaders ()
Declare Sub UpdateLeagueStandings ()
Declare Sub UpdateManagerFile (yearNumber$)
Declare Sub UpdateRecords (leagueFile$)
Declare Sub ViewAndPrintOptions (statFile$, teamName$, totalGames%, menuExit)
Declare Sub ViewBattingLeaders (leagueFile$, JB, JP, TS, TTS)
Declare Sub ViewExpandedLeaders ()
Declare Sub ViewHitters ()
Declare Sub ViewHittingFielding ()
Declare Sub ViewLeaders_Batters (careerFile$)
Declare Sub ViewLeaders_Pitchers (careerFile$)
Declare Sub ViewLeaders_Screen ()
Declare Sub ViewLeaders_Sequence (careerFile$)
Declare Sub ViewLineup (targetTeam$)
Declare Sub ViewNewStandings (LGF$)
Declare Sub ViewPitchers ()
Declare Sub ViewPitchingLeaders (leagueFile$, JB, JP, TS, TTS)
Declare Sub ViewPrintCareerRecords ()
Declare Sub ViewPrintLeaders ()
Declare Sub ViewRoster (targetTeam$)
Declare Sub ViewSearchResults (PS, PS$, LGE$)
Declare Sub ViewSeasonRecords (leagueFile$)
Declare Sub ViewStatSummary (statFile$, teamName$, totalGames%, X, splitScreenChoice%, menuExit)
Declare Sub ViewTeamLeaders (leagueFile$, JB, JP, TS, TTS)

Declare Function FindStatsPro% (statFile$, TN$, fullWins, fullLosses)

