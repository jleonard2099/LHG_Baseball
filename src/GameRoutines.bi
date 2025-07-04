Declare Sub LOADER
Declare Sub StartingLineup (P9)
Declare Sub LineupPositions (teamIdx, posIdx)
Declare Sub ChangeLineup_PreGame (P9, reselect)
Declare Sub SaveLineup (idx%)
Declare Sub SearchForLineup (P9, Q2%)
Declare Sub LineupFromProfile (lineupFound%, P9, skipLineChange%, useMgrProf, I$, Q2%)
Declare Sub LineupSummary (P9, reselect, LU%)
Declare Sub SelectBatters (batterFlag%, P9)
Declare Sub SelectStadium (useVGA)
Declare Sub PitchingRotations (computerRotations%, P9)
Declare Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)
Declare Sub SortHitters (P9)
Declare Sub DoSort (B, C, P9)
Declare Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)
Declare Sub PrintPitcherInfo (idx%)
Declare Sub PitchingStarter (tmIdx)
Declare Sub SOURCE ()
Declare Sub INFO ()
Declare Sub InitVar ()
Declare Sub AUTOSETUP (validTeam)
Declare Sub LetterToNumber (promptval$, J)
Declare Sub TEAMBOX (boxHeight, teamIdx)
Declare Sub FRAMEOPTION (boxHeight, teamIdx)
Declare Sub Rolld100 (randNumVar)
Declare Sub DELAY ()
Declare Sub GetPBPString (range%, minIdx%, fileString$, D)
Declare Sub GETRPBP (range%, minIdx%, fileString$, D)
Declare Sub STRIPRPBP (range%, minIdx%, fileString$, D)
Declare Sub PBP (pbpString$)
Declare Sub ADDPIT (D, I5, I6)
Declare Sub ADDSTTS (P, D)
Declare Sub LocateInning (I2)
Declare Sub ADJRUNNERS (S2%, P)
Declare Sub ADV1BASE (p_base)
Declare Sub ADV2BASES (p_base)
Declare Sub ADV3BASES (I3, P, D)
Declare Sub ADVANCEMENT (P, D, S2%, errorOnPlay%, I3)
Declare Sub DisplayBatter_Lineup (PG, P9)
Declare Sub BATOFF (P, I5, I6)
Declare Sub BATTER1 (i)
Declare Sub BATTER2 (i)
Declare Sub BATTER3 (I3, P, D)
Declare Sub CAUGHTFLY (i, S2%, D)
Declare Sub CHANGELINEUP (P9, P)
Declare Sub CHARGERUN (D, i)
Declare Sub CHECKSAVE (D, P, S4%)
Declare Sub CLEARBASE (i)
Declare Sub CREDITASSIST (currFielder, D)
Declare Sub CREDITPUTOUT (I3, FB2P%, AST%, D)
Declare Sub CREDITREGPO (currFielder, D)
Declare Sub DELIVERY (D)
Declare Sub DisplayFielding_Lineup (PG, P9, TG)
Declare Sub DisplayRun (I2, P, X)
Declare Sub DPPBP ()
Declare Sub DPPIVOT (currFielder, D)
Declare Sub DPRELAY ()
Declare Sub DPSAFE ()
Declare Sub EJECTINC (ejectCnt, X$)
Declare Sub EJECTIONINJURY (playerMode, P, compTeam, D, po%, P9, W5, W6)
Declare Sub FLDAVG (D, currFielder, I3)
Declare Sub DisplayBallField (nbrBalls, P, nbrStrikes, D)
Declare Sub FLDERR (errorOnPlay%, currFielder, D, I3, SB%, S2%, P)
Declare Sub GETFIELDER (currFielder, P, I1, D)
Declare Sub GETFIELDNAME (currFielder, D)
Declare Sub GETINFIELDER (currFielder, P, I1, D, S2%, P2)
Declare Sub GETOUTFIELDER (currFielder, P, I1, D)
Declare Sub GROUNDERPBP (currFielder, D)
Declare Sub GROUNDERS (currFielder, D)
Declare Sub HITS2GAP (currFielder, P, I1, D)
Declare Sub HOLDSTATUS (runnerHeld)
Declare Sub INCBATDF (D, I5, I6)
Declare Sub INCBATOFF (P, I5, I6)
Declare Sub INCPITOFF (P, I5, I6)
Declare Sub INCREMENTHIT (P, D, MO)
Declare Sub INJPIT (P, D, IP%, pinchHitterFlag)
Declare Sub INJURYLENGTH (injCount, D, po%, playerMode, P, compTeam, P9, W5, W6)
Declare Sub INNDISP (currInning, teamLine)
Declare Sub LDRIVE (X$)
Declare Sub LONGFLY (D, currFielder)
Declare Sub MAINSCREEN (D, P, nbrBalls, nbrStrikes, MO)
Declare Sub UpdateSchedule
Declare Sub WINDEX ()
Declare Sub NEWPITCHER (pIdx, pinchHitterFlag, newPitcherFlag)
Declare Sub NOJUMP (D)
Declare Sub OFFPIT (P9, I5, I6)
Declare Sub OUTATFIRST (currFielder, D, fbun%, FB2P%)
Declare Sub OUTFIELDERR ()
Declare Sub OUTFLDNAME (currFielder, D)
Declare Sub PICKEDOFF (D, i)
Declare Sub PINCHRUNNER (MS%, A1, S4%, I2, P, PR%, nbrBalls, nbrStrikes, D, MO)
Declare Sub PITCHERNAME (currFielder, D)
Declare Sub PITCHOUT (baseRunners, TGTBASE%, stlChance)
Declare Sub PITDISP (P9)
Declare Sub PITOFF (P, I5, I6)
Declare Sub PITPLACEMENT (P, X$)
Declare Sub PITSTAM (P9)
Declare Sub PITSTRAT (pinchHitterFlag, newPitcherFlag)
Declare Sub PITSWAP (i)
Declare Sub PLAYERNAME (P)
Declare Sub PLYRHRT (P, P9, W5, W6, nbrBalls, nbrStrikes, D, MO)
Declare Sub PinchHitter (P, P9, W5, W6, idx)
Declare Sub PopUp_Lineup ()
Declare Sub PopUp_Batter (WK, M)
Declare Sub PopUp_Pitcher (WK, M)
Declare Sub POPUPSTATS ()
Declare Sub PUTOUT (i, P)
Declare Sub RecordOut (i, baseNbr)
Declare Sub RecordRun (sah%, i, D, errorOnPlay%, PQ, S2%, currFielder, INFPOS%)
Declare Sub DISPBAVG (D)
Declare Sub HITTINGLINES
Declare Sub LRADJ (A!, A1!)
Declare Sub NORMALIZE (I7!, I2!, I6!, K8!)
Declare Sub BASEONBALLS (S2%, D2, D, P)
Declare Sub HITBYPITCH (S2%, D)
Declare Sub WILDPITCH (S2%, D)
Declare Sub RELIEVERS (pinchHitterFlag, newPitcherFlag)
Declare Sub REPLACEMENTS (I7%, I8%, i9%)
Declare Sub SCOREBOARD ()
Declare Sub ColorChange ()
Declare Sub SECOND2FIRST (D)
Declare Sub SINGLEPBP (S2%, D, currFielder)
Declare Sub SINGLEROUTINE (currFielder, P, I1, D, S2%, P2)
Declare Sub SOMESTATS (V, P, errorOnPlay%)
Declare Sub SQUEEZECHOICE (SH)
Declare Sub STEAL3RD (TGTBASE%, AA, stlChance, runnerHeld, D)
Declare Sub STEALCHANCE (AA, I2, TGTBASE%, CHANCE%)
Declare Sub STRIKEOUT (nbrStrikes, D)
Declare Sub STRIKEOUTCS (SO)
Declare Sub TAGADVANCE (S2%, I8!, D)
Declare Sub THROW2FIRST (D)
Declare Sub TIRING (P9)
Declare Sub TRIPLEPLAY (D)
Declare Sub TRYFOREXTRABASE (S2%, P2, runnerOn2nd, D, currFielder, sah%, stealing, baseNbr!, noThrow)
Declare Sub VIEWTEAM ()
Declare Sub WALLDOUBLE (HW%, currFielder)
Declare Sub WHICHRUNNER (i, S4%, D5)
Declare Sub WHOSON1ST (S4%)
Declare Sub FOULBALL ()
Declare Sub RAREPLAYS (H6%, H7%, IP%)
Declare Sub WEATHER ()
Declare Sub COLCHK (P9, I)
Declare Sub GETWEATHER (windDir, windSpeed, gameMonth, WF!, skyType, gameTemp, rainDelay)
Declare Sub LINSWAP (P9)
Declare Sub NEWLINES (P9)
Declare Sub PositionCursor (I)
Declare Sub POSSWAP (P9)
Declare Sub SHOWBATTERS (P9)
Declare Sub SHOWFIELD (P9)
Declare Sub ShowSplit (P9)
Declare Sub OPTIONS ()
Declare Sub LOADSTATS ()
Declare Sub EVENTS (specialCount, specialStat)
Declare Sub FoundSpecial (specialCount, specialStat)
Declare Sub CheckRecords ()
Declare Sub InningPrintBox (I2)
Declare Sub InningPrintFile (I2)
Declare Sub LDRNRCTDN ()
Declare Sub POSTGAME ()
Declare Sub SAVESTATS ()
Declare Sub COMPILESTATFILES (teamIdx)
Declare Sub BOXOPTIONS (outputChoice)
Declare Sub BOX2FILE (printChoice, boxName$)
Declare Sub BOX2SCREEN
Declare Sub ScoreSummary
Declare Sub STATS2SCREEN
Declare Sub BOX2SPC (boxName$)
Declare Sub EndOfGame ()
Declare Sub pbpLog (lineNumber)
Declare Sub ChooseGameTeam (yearNumber$, numberTeams, chosenIdx, teamName$, gameIdx)
Declare Sub SelectGameTeam (numberTeams, yearNumber$, selectedIdx, teamName$, ExitCode%, gameIdx)
Declare Sub ReadGameTeam (teamYear$, targetTeam$, teamIdx, gameIdx, dataOK)
Declare Sub ReadGameExtraData (teamYear$, teamIdx, gameIdx)

Declare Function d100% ()
Declare Function CheckString% (targetString$)
Declare Function GetTeamYear% (yearFromName$)
Declare Function OUTFLDLOCATION$ (currFielder)
