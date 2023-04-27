Declare Sub ACTIVATE ()
Declare Sub ALIGN ()
Declare Sub AnalyzePlayerUsage (targetTeam$)
Declare Sub AnalyzeTeamRecord (sameLeague)
Declare Sub BASERUN (yearNumber$)
Declare Sub BATTEREDIT ()
Declare Sub BINPUT ()
Declare Sub BLANK ()
Declare Sub BatterTotals ()
Declare Sub BoxscoreMaintenance ()
Declare Sub CAREDIT ()
Declare Sub CAREER ()
Declare Sub CARLDR ()
Declare Sub CARVIEW ()
Declare Sub COMPARE ()
Declare Sub CREATE ()
Declare Sub CalculateBuntRating ()
Declare Sub CalculateRunRating ()
Declare Sub CareerToPrinter (careerFile$)
Declare Sub CareerToScreen (careerFile$)
Declare Sub ChangeGlobalSettings ()
Declare Sub ChangeLineup_PROGMAN (yearNumber$, I$)
Declare Sub ChangeLineup_PreGame (P9, reselect)
Declare Sub ChangePitchingStats (L2%, W2%)
Declare Sub ChangePlayers (yearNumber$, teamName$, teamIdx)
Declare Sub CheckLeagueStats (targetTeam$)
Declare Sub CheckMoreStats ()
Declare Sub CheckTeam_Schedule (teamIdx, yearNumber$)
Declare Sub ChooseLeague (targetFile$)
Declare Sub Comparisons_Printer (teamName$)
Declare Sub Comparisons_Screen (teamName$)
Declare Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)
Declare Sub ConfigureDrives ()
Declare Sub ContinueTrade (yearNumbers$(), tradeIdx)
Declare Sub CreateSeasonSchedule ()
Declare Sub DHLineups (P9, reselect, LU%)
Declare Sub DRAFT ()
Declare Sub DRAWBOX ()
Declare Sub DefenseLateInning (yearNumber$, T$)
Declare Sub DefenseSubPinchRun (targetTeam$)
Declare Sub DefensiveStrategy (yearNumber$)
Declare Sub DeleteSchedules (fileChoice%, fileSpec$)
Declare Sub DoTheDraft (DNT%, doneDrafting%, teamName$, teamYears$(), saveSpot)
Declare Sub DraftNewTeam (draftYear$, teamName$)
Declare Sub DraftPlayer (playerType, yearNumber$)
Declare Sub EDITTEAM (YN$, T$, NN$, S$, M$, teamIdx)
Declare Sub EditBatters (careerFile$)
Declare Sub EditBoxscore (boxName$)
Declare Sub EditGame ()
Declare Sub EditLeague (leagueFile$)
Declare Sub EditLineups (handRL%, targetTeam$, yearNumber$)
Declare Sub EditOneLineup (targetTeam$, yearNumber$)
Declare Sub EditParameters ()
Declare Sub EditPitchers (careerFile$)
Declare Sub EditStatsSchedules (action%)
Declare Sub EditUmpireFile ()
Declare Sub EnterUmpireFile ()
Declare Sub ExpandedLeaders_Menu (leagueFile$)
Declare Sub ExportManagerProfile ()
Declare Sub ExtractSchedule (scheduleAction%, scheduleFile$)
Declare Sub FieldingStats ()
Declare Sub FinalMerge (leaderFile$)
Declare Sub GROUND ()
Declare Sub GetDiskYear (targetTeam$, yearNumber$, teamidx)
Declare Sub GetLineup (handRL%, targetTeam$)
Declare Sub GetLineupPosition (I1, I$)
Declare Sub GetListLength (QW, LG)
Declare Sub GetSortOptions (JX, PV)
Declare Sub GetTeam (yearNumber$, numberTeams, teamIndex%)
Declare Sub GetTeamA_TRADE (yearNumber$, numberTeams, tradeIdx)
Declare Sub GetTeamB_TRADE (yearNumber$, numberTeams, tradeIdx)
Declare Sub GetTeam_BINPUT (yearNumber$, numberTeams, teamIdx)
Declare Sub GetTeam_CREATE (yearNumber$, teamName$, teamIdx)
Declare Sub GetTeam_GROUND (yearNumber$, numberTeams, teamName$, teamIdx)
Declare Sub GetTeam_PROGMAN (targetTeam$, yearNumber$)
Declare Sub GetTeam_REC (yearNumber$, numberTeams)
Declare Sub GetTeam_SINPUT (yearNumber$, numberTeams)
Declare Sub GetTeam_Schedule (numberTeams, yearNumber$, teamIdx)
Declare Sub GetTeams_Activate (yearNumber$, numberTeams)
Declare Sub HitAndRun (yearNumber$)
Declare Sub HittingStrategy (yearNumber$)
Declare Sub ImportManagerProfile ()
Declare Sub InputDeleteLineup (D%, yearNumber$)
Declare Sub InputTeam (inputAction%, yearNumber$, targetTeam$, teamIdx)
Declare Sub InputTeamStats (Z$, A$, D0%, D1%, L2%, W2%)
Declare Sub Input_PITSTRAT (yearNumber$)
Declare Sub LEAGCMPL ()
Declare Sub LOOKY ()
Declare Sub Leaders_Menu ()
Declare Sub LeagueTotals_A_Screen (TVN, QW)
Declare Sub LeagueTotals_B_Screen (TVN, QW)
Declare Sub LineupFromProfile (lineupFound%, P9, skipLineChange%)
Declare Sub LineupPositions (teamIdx, posIdx, skipLU%)
Declare Sub LineupStrategy (targetTeam$)
Declare Sub LoadSchedule (targetFile$)
Declare Sub LoadTeamStats (targetTeam$, teamHasStats, SS, targetFile$)
Declare Sub LoadTeam_LOADER (teamIdx%, YN$, idx%)
Declare Sub MERGE ()
Declare Sub Main_Menu (menuSelection%)
Declare Sub MergeIt ()
Declare Sub Merge_AddYear ()
Declare Sub Merge_FirstYear ()
Declare Sub NEWLDR ()
Declare Sub NEWLDRRC ()
Declare Sub NEWLDRST ()
Declare Sub NewGame (visitor$, home$, targetGame%)
Declare Sub NewPrintStats (statFile$, printChoice, splitScreenChoice%, teamName$)
Declare Sub NewSchedule (scheduleFile$)
Declare Sub PHBATTER (yearNumber$)
Declare Sub PHPITCHER (yearNumber$)
Declare Sub PITCHAROUND (yearNumber$)
Declare Sub PITCHEREDIT ()
Declare Sub PITCHINIT (i, FG, BG)
Declare Sub POSCR ()
Declare Sub POSSRT ()
Declare Sub PROGMAN ()
Declare Sub PRTLEAG ()
Declare Sub PinchRunStrategy (targetTeam$)
Declare Sub PinchRunners (targetTeam$)
Declare Sub PitcherTotals ()
Declare Sub Pitchers10_Games (PC, PPS$, LGE$)
Declare Sub Pitchers10_Print (PC, PPS$, LGE$)
Declare Sub PitchersHanded (THR, PPS$, LGE$)
Declare Sub PitchingLeaders_Screen (TVN, B9, QW)
Declare Sub ManagePitchingRotations (action%, targetTeam$)
Declare Sub PitchingRotations (computerRotations%, P9)
Declare Sub PlayerStats_Screen (teamName$)
Declare Sub PlayerTotals (TVN, B9, QW)
Declare Sub PrintBatterStats (careerFile$, playerIdx)
Declare Sub PrintLeaders_Screen ()
Declare Sub PrintManagerProfile (targetTeam$)
Declare Sub PrintNewLeaders ()
Declare Sub PrintPitcherInfo (idx%)
Declare Sub PrintPitcherStats (careerFile$, playerIdx)
Declare Sub PrintRecords (leagueFile$)
Declare Sub PrintSearchResults (PS, PS$, LGE$)
Declare Sub PrintTeamStats (targetTeam$, teamIdx)
Declare Sub PromptScheduleChanges (Z$)
Declare Sub REC ()
Declare Sub ReadStats (statFile$)
Declare Sub ReadStats_TRADE (yearNumbers$(), tradeIdx)
Declare Sub ReadTeam_A (yearNumber$, teamIdx)
Declare Sub ReadTeam_B (yearNumber$, teamIdx)
Declare Sub ReadTeam_POSCR (yearNumber$, teamIndex%)
Declare Sub ReadTeam_PosSrt (teamIndex%, yearNumber$)
Declare Sub Records_Printer ()
Declare Sub Records_Screen ()
Declare Sub Replay_Menu ()
Declare Sub ReportsAllTeams ()
Declare Sub RotationParams (targetTeam$)
Declare Sub RotationReliefMenu (targetTeam$)
Declare Sub SCHEDULE ()
Declare Sub SEELEAG ()
Declare Sub SINPUT ()
Declare Sub SaveLineup (idx%)
Declare Sub SaveScheduleFile (saveFile$)
Declare Sub SaveTeamDraft (DNT%, yearNumber$, teamName$, teamIdx)
Declare Sub SaveTeamFile (yearNumber$, targetTeam$, targetFile$, teamIdx)
Declare Sub SaveTrades (yearNumbers$())
Declare Sub SearchForLineup (P9)
Declare Sub SearchPlayers ()
Declare Sub SelectBatters (batterFlag%, P9)
Declare Sub SelectBatters_SINPUT
Declare Sub SelectLeague (leagueFile$)
Declare Sub SelectPinchHitters (yearNumber$, targetTeam$)
Declare Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)
Declare Sub SelectPitchers_SINPUT (D0%, D1%, L2%, W2%, Z$)
Declare Sub SelectPrintOrView (printOption)
Declare Sub SelectStadium ()
Declare Sub SelectTeam_ACTIVATE (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_BINPUT (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_COMPARE (yearNumber$, numberTeams)
Declare Sub SelectTeam_CREATE (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_DRAFT (draftYear$, numberTeams%, teamNumber, teamIdx)
Declare Sub SelectTeam_GROUND (yearNumber$, numberTeams, teamIdx)
Declare Sub SelectTeam_LOADER (NTMS, YN$, idx%)
Declare Sub SelectTeam_LOOKY (numberTeams, yearNumber$)
Declare Sub SelectTeam_POSCR (yearNumber$, numberTeams, teamIndex%)
Declare Sub SelectTeam_PROGMAN (numberTeams, yearNumber$, T)
Declare Sub SelectTeam_REC (yearNumber$, numberTeams)
Declare Sub SelectTeam_SINPUT (yearNumber$, numberTeams)
Declare Sub SelectTeam_Schedule (yearNumber$, numberTeams)
Declare Sub SelectTeam_TRADE (yearNumber$, numberTeams, tradeIdx)
Declare Sub SetupClosers (targetTeam$, yearNumber$)
Declare Sub SetupMiddleRelievers (targetTeam$, yearNumber$)
Declare Sub SetupRelievers (targetTeam$, yearNumber$)
Declare Sub ShowPitcherInfo (targetTeam$)
Declare Sub SomeCalculations (B, C, P9)
Declare Sub SortHitters (P9)
Declare Sub SortPitchers (LGE$)
Declare Sub SortPlayer1 (JX, PPP, stpl)
Declare Sub SortPlayer2 (JX, PPP, stpl)
Declare Sub SortSchedule ()
Declare Sub SortTeam1 (JX, TS, stpl)
Declare Sub SortTeam2 (JX, TTS, stpl)
Declare Sub SpotStarters (targetTeam$, yearNumber$)
Declare Sub StartingLineup (P9)
Declare Sub StatsOneTeam ()
Declare Sub SwapSpots (yearNumber$, targetTeam$, teamIdx)
Declare Sub SwitchBatters ()
Declare Sub SwitchPitchers ()
Declare Sub THROWHOME (yearNumber$)
Declare Sub TRADE ()
Declare Sub TeamComparison_Printer (teamName$)
Declare Sub TradeBatters ()
Declare Sub TradePitchers ()
Declare Sub TradeSummary_Batters ()
Declare Sub TradeSummary_Pitchers
Declare Sub UMPMGR ()
Declare Sub UpdateExpandedLeaders ()
Declare Sub UpdateLeaders ()
Declare Sub UpdateLeagueStandings ()
Declare Sub UpdateManagerFile (yearNumber$)
Declare Sub UpdateRecords (leagueFile$)
Declare Sub ViewAndPrintOptions (statFile$, splitScreenChoice%, teamName$)
Declare Sub ViewBattingLeaders (leagueFile$)
Declare Sub ViewComparisons ()
Declare Sub ViewExpandedLeaders ()
Declare Sub ViewHitters ()
Declare Sub ViewHittingFielding
Declare Sub ViewLeaders_Batters (careerFile$)
Declare Sub ViewLeaders_Pitchers (careerFile$)
Declare Sub ViewLeaders_Screen ()
Declare Sub ViewLeaders_Sequence (careerFile$)
Declare Sub ViewLineup (targetTeam$)
Declare Sub ViewNewLeaders ()
Declare Sub ViewPitchers ()
Declare Sub ViewPitchingLeaders (leagueFile$)
Declare Sub ViewPrintLeaders ()
Declare Sub ViewPrintLeagueStandings
Declare Sub ViewPrintRecords (leagueFile$)
Declare Sub ViewRostersRatings (targetTeam$)
Declare Sub ViewSearchResults (PS, PS$, LGE$)
Declare Sub ViewTeamLeaders ()
Declare Sub ViewTeamTotals (leagueFile$)
