'----------------------------------------
'            LOADER Subroutine
'----------------------------------------
'This subroutine is used to setup all of
'the parameters for a baseball game.
'This involves loading parameters, teams,
'rotations, linesup and a park.
Sub LOADER

    Shared actualAttendance&, avgAttendance&

    _Title "Full Count Baseball v" + GAME_VERSION$ + " - PLAY BALL!"

    Shared dayNight$(), desigHitOpt$(), gameType$()
    Shared leagueType$(), month$()
    Shared PT$(), pitcherEra$()
    Shared playMode$(), umpireName$()
    Shared teamIndicator$(), yesNoText$()
    Shared gameT1%(), umpireNumber()

    Shared scheduleFile$

    Color 7, 0
    Cls

    Randomize Timer

    Call InitVar

    '----------------------------------------
    '         DETERMINE GAME OPTIONS
    '----------------------------------------

    If Not _FileExists("DEV.BAS") Then

        Open "DEV.BAS" For Output As #1

        For I = 0 To 4
            Print #1, _CWD$ + "\"
        Next

        Close #1

    End If

    Open "DEV.BAS" For Input As #2

    For I = 0 To 4
        Input #2, diskPaths$(I)
    Next

    Close #2

    If Not _FileExists("DEFERA") Then

        Open "DEFERA" For Output As #3

        For I = 1 To 4:
            Print #3, 0
        Next

        Print #3, 4

        For I = 6 To 14:
            Print #3, 0
        Next

        Close #3

    End If

    Open "DEFERA" For Input As #4
    Input #4, MAIN_playerOpt
    Input #4, leagueOpt
    Input #4, MAIN_desigHit
    Input #4, gameTypeOpt
    Input #4, MAIN_gameMonth
    Input #4, MAIN_dayOrNight
    Input #4, MAIN_useWeather
    Input #4, MAIN_useInj
    Input #4, MAIN_useLRSplit
    Input #4, MAIN_useRest
    Input #4, MAIN_useMgrProf
    Input #4, MAIN_pbpDelay!
    Input #4, MAIN_useVGA
    Input #4, MAIN_pitchEraOpt
    Close #4

    'Load game in-progress, if there is one
    If _FileExists("GAME82") Then

        Open "GAME82" For Input As #5

        Input #5, scheduleFile$

        For I = 0 To 2:
            Input #5, schedGame(I)
        Next

        Close #5

        'currGame = schedGame(0) + schedGame(2)

        Open "pbplog" + _Trim$( Str$(schedGame(0)) ) + "-" + _Trim$( Str$(schedGame(1)) ) For Append As #11

        Open diskPaths$(4) + scheduleFile$ For Random As #6 Len = SCHEDULE_SIZE_BYTES

        For X = 0 To 18
            Field #6, X * 2 As X$, 2 As Q2$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
        Next

        Field #6, 38 As X$, 2 As Q2$(22), 2 As Q2$(23), 3 As Q2$(20), 3 As Q2$(21)

        Get #6, schedGame(0) + schedGame(2)

        For X = 0 To 18
            NG%(X) = CVI(Q2$(X + 1))
        Next

        For X = 0 To 1
            diskIDs$(X) = _Trim$(Q2$(20 + X))
        Next

        VS% = CVI(Q2$(22))
        HS% = CVI(Q2$(23))

        Close #6

        autoPlay = _TRUE

    Else

        Open "pbplog" For Output As #11 'todo [bm] we need to close this when we leave so it's not still open if we re-enter

    End If


    If autoPlay Then

        Call AUTOSETUP

        For Idx = 0 To 1
            gameT1%(Idx) = NG%(Idx)
            YN$ = diskIDs$(Idx)
            Call ReadGameTeam(YN$, gameTeams$(Idx), gameT1%(Idx), Idx)
            diskIDs$(Idx) = YN$
        Next Idx

    Else

        170 '
        Call INFO

        If gameLoc = 0 Then gameLoc = 1

        Do

            Color 15, 0
            Cls

            Color 2
            Print Tab(29); "SELECT OPENING OPTIONS"

            Locate 2, 2: Color 14
            Print "(1) ";
            Color 15: Print playMode$(MAIN_playerOpt)
            Locate 3, 2: Color 14
            Print "(2) ";
            Color 15: Print "VISITING TEAM: ";
            Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0)
            Locate 4, 2: Color 14, 0
            Print "(3) ";
            Color 15: Print "HOME TEAM: ";
            Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1)
            Locate 5, 2: Color 14, 0
            Print "(4) ";
            Color 15: Print "LEAGUE TYPE: "; leagueType$(leagueOpt)
            Locate 6, 2: Color 14
            Print "(5) ";
            Color 15: Print "DESIGNATED HITTER: "; desigHitOpt$(MAIN_desigHit)
            Locate 7, 2: Color 14
            Print "(6) ";
            Color 15: Print "GAME TYPE: "; gameType$(gameTypeOpt)
            Locate 8, 2: Color 14
            Print "(7) ";
            Color 15: Print "MONTH: "; month$(MAIN_gameMonth)
            Locate 9, 2: Color 14
            Print "(8) ";
            Color 15: Print "TIME OF DAY: "; dayNight$(MAIN_dayOrNight)
            Locate 10, 2: Color 14
            Print "(9) ";
            Color 15: Print "START GAME"
            Locate 11, 2: Color 14
            Print "(0) ";
            Color 15: Print "SAVE SETTINGS AS DEFAULT"
            Locate 12, 2: Color 14
            Print "(A) ";
            Color 15: Print Using "PLAY-BY-PLAY DELAY (SECS): ##.# "; MAIN_pbpDelay!
            Locate 13, 2: Color 14
            Print "(B) ";
            Color 15: Print "USE INJURIES: "; yesNoText$(MAIN_useInj)
            Locate 14, 2: Color 14
            Print "(C) ";
            Color 15: Print "USE LEFT/RIGHT SPLIT STATS: "; yesNoText$(MAIN_useLRSplit)
            Locate 15, 2: Color 14
            Print "(D) ";
            Color 15: Print "USE DAYS REST DATA FOR PITCHERS: "; yesNoText$(MAIN_useRest)
            Locate 16, 2: Color 14
            Print "(E) ";
            Color 15: Print "USE MANAGER PROFILE, IF AVAILABLE: "; yesNoText$(MAIN_useMgrProf)
            Locate 17, 2: Color 14
            Print "(F) ";
            Color 15: Print "USE WEATHER EFFECTS: "; yesNoText$(MAIN_useWeather)
            Locate 18, 2: Color 14
            Print "(G) ";
            Color 15: Print "USE VGA SCREENS, IF ABLE: "; yesNoText$(MAIN_useVGA)
            Locate 19, 2: Color 14
            Print "(H) ";
            Color 15: Print "PITCHER ERA: "; pitcherEra$(MAIN_pitchEraOpt)

            Do
                Color 7, 0

                I$ = GetKeyPress$

                If I$ <> Chr$(27) Then Call LetterToNumber(I$, I)

            Loop Until I >= 0 And I <= 17 Or I$ = Chr$(27) Or UCase$(I$) = "Q"

            If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then

                Select Case I

                    Case 0: 'Save settings as default'
                        Open "DEFERA" For Output As #7
                        Print #7, MAIN_playerOpt
                        Print #7, leagueOpt
                        Print #7, MAIN_desigHit
                        Print #7, gameTypeOpt
                        Print #7, MAIN_gameMonth
                        Print #7, MAIN_dayOrNight
                        Print #7, MAIN_useWeather
                        Print #7, MAIN_useInj
                        Print #7, MAIN_useLRSplit
                        Print #7, MAIN_useRest
                        Print #7, MAIN_useMgrProf
                        Print #7, MAIN_pbpDelay!
                        Print #7, MAIN_useVGA
                        Print #7, MAIN_pitchEraOpt
                        Close #7

                    Case 1: 'Play Mode'
                        MAIN_playerOpt = MAIN_playerOpt + 1
                        If MAIN_playerOpt > 3 Then MAIN_playerOpt = 0

                    Case 2: 'Visiting Team'
                        Call ChooseGameTeam(diskIDs$(0), NTMS, gameT1%(0), gameTeams$(0), 0)
                        Color 15, 0

                    Case 3: 'Home Team'
                        Call ChooseGameTeam(diskIDs$(1), NTMS, gameT1%(1), gameTeams$(1), 1)
                        Color 15, 0

                    Case 4: 'League Type'
                        leagueOpt = leagueOpt + 1
                        If leagueOpt > 2 Then leagueOpt = 0

                    Case 5: 'Designated Hitter'
                        MAIN_desigHit = 1 - MAIN_desigHit

                    Case 6: 'Game Type'
                        gameTypeOpt = 1 - gameTypeOpt

                    Case 7: 'Month'
                        MAIN_gameMonth = MAIN_gameMonth + 1
                        If MAIN_gameMonth > 10 Then MAIN_gameMonth = 4
                        Locate 7, 4: Print BK$

                    Case 8: 'Time of Day'
                        MAIN_dayOrNight = 1 - MAIN_dayOrNight
                        Locate 8, 4
                        Print BK$

                    Case 9: 'Start Game'
                        '

                    Case 10:    'Play-By-Play Delay'
                        Locate 12, 32
                        Input MAIN_pbpDelay!

                    Case 11:    'Use Injuries'
                        MAIN_useInj = 1 - MAIN_useInj

                    Case 12:    'Use Left/Right Split Stats'
                        MAIN_useLRSplit = 1 - MAIN_useLRSplit

                    Case 13:    'Use Days Rest Data For Pitchers'
                        MAIN_useRest = 1 - MAIN_useRest

                    Case 14:    'Use Manager Profile, If Available'
                        MAIN_useMgrProf = 1 - MAIN_useMgrProf

                    Case 15:    'Use Weather Effects'
                        MAIN_useWeather = 1 - MAIN_useWeather

                    Case 16:    'Use VGA Screens, If Able'
                        MAIN_useVGA = 1 - MAIN_useVGA

                    Case 17:    'Pitcher Era'
                        MAIN_pitchEraOpt = MAIN_pitchEraOpt + 1
                        If MAIN_pitchEraOpt > 8 Then MAIN_pitchEraOpt = 0

                End Select

            Else

                'Close #7
                'End
                'System
                Exit Sub

            End If

        Loop Until (I = 9 And gameTeams$(0) <> "" And gameTeams$(1) <> "") Or I$ = Chr$(27) Or UCase$(I$) = "Q"

        If I$ <> Chr$(27) And UCase$(I$) <> "Q" Then
            'TICKER PROMPT
        Else

            'End
            'System
            'Exit Sub

        End If

    End If

    'Determine attendance

    ' ** the original game generated random attendance from 5,000 to 59,000
    ' ** Attendannce hasn't been as low as 5,000 since early 1900,
    ' ** and almost never 59,000 except for significant events
    'https://www.baseball-reference.com/leagues/majors/misc.shtml
    If gameLoc = 2 Or avgAttendance& = 0 Then
        'Get assumed year of home team
        YR% = GetTeamYear%(gameTeams$(1))

        Select Case YR%

            Case Is <= 1900
                'Random # from 500 to 3,000
                actualAttendance& = Int(Rnd(1) * 2500) + 500

            Case 1901 - 1945
                'Random # from 4,000 to 10,000
                actualAttendance& = Int(Rnd(1) * 6000) + 4000

            Case Is >= 1946
                'Random # from 15,000 to 30,000
                actualAttendance& = Int(Rnd(1) * 15000) + 15000

        End Select

    Else
        'Random % +/- 10,000 of team's average
        ' ** this was added by me (Jason) to reflect
        ' ** how other games calculate attendance.
        ' ** Previously the source was not using the
        ' ** averages read in from PARKS.DBF
        actualAttendance& = avgAttendance& + Int(Rnd(1) * 20000) - 10000
    End If

    '----------------------------------------
    '    AUTO-DETERMINE OTHER OPTIONS
    '----------------------------------------

    ' Determine computer team
    Select Case MAIN_playerOpt

        Case 1:
            playerMode = PLAY_MODE_COMPUTER
            compTeam = 0

        Case 2:
            playerMode = PLAY_MODE_COMPUTER
            compTeam = 1

        Case 3:
            playerMode = PLAY_MODE_PCvsPC

    End Select

    'playerMode represents the following values:
    '   0 = Human
    '   1 = Computer Team
    '   2 = PC vs. PC

    If teamRat_GAME(0, 11) = teamRat_GAME(1, 11) And teamRat_GAME(0, 12) = teamRat_GAME(1, 12) Then Call ColorChange

    YV = 1
    YM = 0

    '-------------------------
    ' Determine umpires
    '-------------------------
    If Not _FileExists(diskPaths$(0) + "UMPIRES." + umpireName$(leagueOpt)) Then

        'Assign generic umpire names if we don't have any
        gameUmpire$(1) = "RHODES"
        gameUmpire$(2) = "WINBERRY"
        gameUmpire$(3) = "TIPPETT"
        gameUmpire$(4) = "MILLER"

    Else
        YU = 0

        Open diskPaths$(0) + "UMPIRES." + umpireName$(leagueOpt) For Input As #8

        For I1 = 1 To 50
            Input #8, umpireName$(I1)
            Input #8, umpireNumber(I1)
        Next

        Close #8

        For I1 = 1 To 50
            If umpireName$(I1) <> "" Then YU = YU + 1
        Next

        If YU < 4 Then
            'If there aren't 4 umpires, assign generic names
            gameUmpire$(1) = "RHODES"
            gameUmpire$(2) = "WINBERRY"
            gameUmpire$(3) = "TIPPETT"
            gameUmpire$(4) = "MILLER"
        Else
            'Randomly assign umpires
            Do

                While (umpireName$(YU) = "" Or YU = YM)
                    YU = Int(Rnd(1) * 50) + 1
                Wend

                gameUmpire$(YV) = umpireName$(YU)
                YM = YU
                YV = YV + 1

            Loop Until YV >= 5

        End If

    End If

    '-------------------------
    ' Team Mgmt + Lineups
    '-------------------------

    'Determine which team PC will play
    'If playerMode = PLAY_MODE_COMPUTER And Not autoPlay Then

    'If not doing autoplay...
    '    Call INFO

    '    Color 14: Print "WHICH TEAM DOES COMPUTER MANAGE ?"
    '    Print
    '    Color 15: Print "(0) ";: Color teamRat_GAME(0, 11), teamRat_GAME(0, 12): Print gameTeams$(0): Color 7, 0
    '    Print
    '    Color 15: Print "(1) ";: Color teamRat_GAME(1, 11), teamRat_GAME(1, 12): Print gameTeams$(1): Color 7, 0

    '    Do
    '        I$ = GetKeyPress$
    '        compTeam = Val(I$)
    '    Loop Until compTeam = 0 Or compTeam = 1

    'End If

    'Determine pitchers
    For P9 = 0 To 1

        cancelPitchers% = 0

        Call SelectPitchers(P9, cancelPitchers%, computerRotations%)
        Call PitchingStarter(P9)

        If Inotloop% <= 5 And gameScore(P9, 0) - gameScore(1 - P9, 0) > 0 Then P2%(P9) = currPitcher%(P9)

        pitchEff(P9, 0) = Int(((pitchRating(P9, currPitcher%(P9), SS_P_BB) + pitchRating(P9, currPitcher%(P9), SS_P_H)) / pitchRating(P9, currPitcher%(P9), SS_P_G)) + .5)
        pitchEff(P9, 1) = 3

        If gamePitcher%(P9) > 1 Then pitchEff(P9, 1) = 0

        'I'm not sure why we go back only
        'if ESC was pressed during pitcher select
        If cancelPitchers% = 1 Then GoTo 170

    Next P9

    'Determine lineup for each team
    For P9 = 0 To 1

        Do

            Cls

            Call INFO

            Locate 3, 1
            Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12): Print gameTeams$(P9): Color 7, 0
            Print
            Color 14: Print "(0) ";: Color 15: Print "INPUT A LINEUP MANUALLY"
            Print
            Color 14: Print "(1) ";: Color 15: Print "SELECT A LINEUP FROM MANAGER PROFILE"
            Print
            Color 14: Print "(2) ";: Color 15: Print "COMPUTER DETERMINE LINEUP RANDOMLY"
            Print
            Color 14: Print "(3) ";: Color 15: Print "COMPUTER DETERMINE LINEUP FROM MANAGER PROFILE"

            reselect = 0

            If autoPlay Then

                'noLUFound = no lineup found?
                If noLUFound = 1 Then

                    noLUFound = 0
                    I$ = "2"

                Else

                    If (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = P9) Then
                        If MAIN_useMgrProf = 1 Then
                            I$ = "1"
                        Else
                            I$ = "2"
                        End If
                    End If

                End If

            Else

                Do
                    I$ = GetKeyPress$
                    I1 = Val(I$)
                Loop Until I1 >= 0 And I1 <= 3 And I$ <> Chr$(27)

            End If

            Select Case I$

                Case "0":
                    'Lineup - no profile

                    Call SelectBatters(batterFlag%, P9)

                    If batterFlag% = 1 Then
                        For I = 10 To 22
                            Locate I, 59: Print Space$(21);
                        Next

                        Call StartingLineup(P9)
                        Call ChangeLineup_PreGame(P9, reselect)

                        If noLineups = 1 Then
                            noLineups = 0
                        End If
                    End If

                Case "1", "3":
                    'Lineup using profile
                    Call SearchForLineup(P9, Q2%)

                    Call LineupFromProfile(lineupFound%, P9, skipLineChange%, MAIN_useMgrProf, I$, Q2%)

                    If lineupFound% = 1 Or skipLineChange% = 0 Then
                        Call ChangeLineup_PreGame(P9, reselect)
                        If noLineups = 1 Then
                            noLineups = 0
                        End If
                    Else
                        noLineups = 1
                    End If

                Case "2"
                    Call ComputerLineups(batterFlag%, P9, noLineups, reselect)

                    If noLineups = 1 Then
                        noLineups = 0
                        skipLineChange% = 1
                    End If

            End Select

            If I$ <> Chr$(27) Then
                'We didn't ESC so we have to move forward

                If skipLineChange% <> 1 Then
                    Color 15, 0
                    Locate 5, 59: Print "OPPOSING PITCHER"
                    Locate , 59: Print pitchers$(1 - P9, currPitcher%(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher%(1 - P9), 0) + 2)
                    If RP = 1 Then RP = 0
                End If

            Else
                'ESC means we go back to earlier menus
                noLineups = 1
                GoTo 170

            End If

        Loop Until noLineups = 0 And reselect = 0

    Next P9

    '-------------------------
    ' Ready to Play
    '-------------------------

    For I = 0 To 1
        For I1 = 1 To 9
            X0%(I, 0, I1) = playerNum%(I, I1)
            X0%(I, 1, I1) = fldPos%(I, I1)
            X0%(I, 2, I1) = I1 * 10
        Next I1

        gameD0%(I) = 9

        Print #11, I; "Ready to PLay: "; gameTeams$(I)  ' [bm]'
        Print #11, "playerNum%: "; playerNum%(I, 1); playerNum%(I, 2); playerNum%(I, 3); playerNum%(I, 4); playerNum%(I, 5); playerNum%(I, 6); playerNum%(I, 7); playerNum%(I, 8); playerNum%(I, 9)
        Print #11, "fldPos%:    "; fldPos%(I, 1); fldPos%(I, 2); fldPos%(I, 3); fldPos%(I, 4); fldPos%(I, 5); fldPos%(I, 6); fldPos%(I, 7); fldPos%(I, 8); fldPos%(I, 9)

        Print  #11,
        'Print #11, "Roster Batter 0", batterNames$(0)
        'For I1 = 0 To 80
       '     Print #11, I1; batRating(I, 0, I1)
        'Next I1

        'Print  #11,
        'Print #11, "Roster Pitcher 0", pitcherNames(0)
        'For I1 = 0 To 89
            'Print #11, I1; pitchRating(I, 0, I1)
        'Next I1

    Next I

    W5 = 0
    W6 = 1
    VV(0) = 1
    VV1(0) = 9
    VV(1) = 1
    VV1(1) = 9

    Cls

    rainDelay = 0

    For I = 0 To 1
        For I1 = 1 To 9
            If fldPos%(I, I1) >= 2 And fldPos%(I, I1) <= 9 Then
               If fldPos%(I, I1) = batRating(I, playerNum%(I, I1), 22) Then
                    batRating(I, playerNum%(I, I1), 15) = batRating(I, playerNum%(I, I1), 33)
                    batRating(I, playerNum%(I, I1), 17) = batRating(I, playerNum%(I, I1), 34)
                    batRating(I, playerNum%(I, I1), 19) = batRating(I, playerNum%(I, I1), 26)
                ElseIf fldPos%(I, I1) = batRating(I, playerNum%(I, I1), 23) Then
                    batRating(I, playerNum%(I, I1), 15) = batRating(I, playerNum%(I, I1), 35)
                    batRating(I, playerNum%(I, I1), 17) = batRating(I, playerNum%(I, I1), 36)
                    batRating(I, playerNum%(I, I1), 19) = batRating(I, playerNum%(I, I1), 27)
                ElseIf fldPos%(I, I1) = batRating(I, playerNum%(I, I1), 24) Then
                    batRating(I, playerNum%(I, I1), 15) = batRating(I, playerNum%(I, I1), 37)
                    batRating(I, playerNum%(I, I1), 17) = batRating(I, playerNum%(I, I1), 38)
                    batRating(I, playerNum%(I, I1), 19) = batRating(I, playerNum%(I, I1), 28)
                ElseIf fldPos%(I, I1) = batRating(I, playerNum%(I, I1), 25) Then
                    batRating(I, playerNum%(I, I1), 15) = batRating(I, playerNum%(I, I1), 39)
                    batRating(I, playerNum%(I, I1), 17) = batRating(I, playerNum%(I, I1), 40)
                    batRating(I, playerNum%(I, I1), 19) = batRating(I, playerNum%(I, I1), 29)
                Else
                    If fldPos%(I, I1) = 2 Then
                        batRating(I, playerNum%(I, I1), 15) = 1
                        batRating(I, playerNum%(I, I1), 17) = 0
                        batRating(I, playerNum%(I, I1), 19) = 910 + Int(Rnd(1) * 10) + 1
                    End If
                    If fldPos%(I, I1) = 3 Then
                        batRating(I, playerNum%(I, I1), 15) = 0
                        batRating(I, playerNum%(I, I1), 17) = 3
                        batRating(I, playerNum%(I, I1), 19) = 960 + Int(Rnd(1) * 10) + 1
                    End If
                    If fldPos%(I, I1) = 4 Then
                        batRating(I, playerNum%(I, I1), 15) = 0
                        batRating(I, playerNum%(I, I1), 17) = 3
                        batRating(I, playerNum%(I, I1), 19) = 912 + Int(Rnd(1) * 10) + 1
                    End If
                    If fldPos%(I, I1) = 5 Or fldPos%(I, I1) = 6 Then
                        batRating(I, playerNum%(I, I1), 15) = 0
                        batRating(I, playerNum%(I, I1), 17) = 3
                        batRating(I, playerNum%(I, I1), 19) = 902 + Int(Rnd(1) * 10) + 1
                    End If
                    If fldPos%(I, I1) >= 7 Then
                        batRating(I, playerNum%(I, I1), 15) = 2
                        batRating(I, playerNum%(I, I1), 17) = 2
                        batRating(I, playerNum%(I, I1), 19) = 972 + Int(Rnd(1) * 10) + 1
                    End If
                End If
            End If
        Next I1
    Next I

    Call SelectStadium(MAIN_useVGA)

    Call GETWEATHER(windDir, windSpeed, MAIN_gameMonth, WF!, skyType, gameTemp, rainDelay)

    Cls

    Call TEAMBOX(5, 1)

    Locate , 11: Print "CONDITIONS AT "; gameStadiums$(1)
    If teamRat_GAME(1, 9) = 1 Then
        Locate , 11: Print "TEMPERATURE: 70"
        Locate , 11: Print "WEATHER: DOME"
        Locate , 11: Print "WIND: NONE"
    Else
        Locate , 11: Print "TEMPERATURE: "; gameTemp
        Locate , 11: Print "SKY CONDITIONS: "; skyCond$(skyType)
        Locate , 11: Print "WIND: "; windDirection$(windDir);
        Print Using " ## MPH"; windSpeed

        If MAIN_useVGA = 0 Then
            windDirection$(2) = "L->R"
            windDirection$(3) = "R->L"
            windDirection$(4) = "<-LF"
            windDirection$(5) = "<-CF"
            windDirection$(6) = "<-RF"
            windDirection$(7) = "->LF"
            windDirection$(8) = "->CF"
            windDirection$(9) = "->RF"
        End If

        wind$ = windDirection$(windDir) + Str$(windSpeed) + " MPH"
        Locate , 11: Print "PRECIPITATION: "; precip$(precipType)

    End If

    I1 = Val(Left$(gameTeams$(0), 2))
    I2 = Val(Left$(gameTeams$(1), 2))

    If Val(Left$(gameTeams$(0), 3)) = 211 Then I1 = 201
    If Val(Left$(gameTeams$(0), 3)) = 200 Then I1 = 200

    If Val(Left$(gameTeams$(1), 3)) = 200 Then I2 = 200
    If Val(Left$(gameTeams$(1), 3)) = 211 Then I2 = 201

    '1955 - Present
    'pitchEff(0, 1) = calculated from Walks + Hits / Game
    'pitchEff(1, 1) = 3

    Select Case MAIN_pitchEraOpt

        Case 3
            '1946-1954
            pitchEff(0, 1) = 4
            pitchEff(1, 1) = 4

        Case 4 To 6
            '1909 to 1945
            pitchEff(0, 1) = 5
            pitchEff(1, 1) = 5

        Case 7
            '1894 to 1908
            pitchEff(0, 1) = 7
            pitchEff(1, 1) = 7

        Case Is >= 8
            '1876-1893
            pitchEff(0, 1) = 99
            pitchEff(1, 1) = 99

    End Select

    Call GETSTATS
    Call DISPBAVG(D)

    W5 = 0
    W6 = 1

    '--- we need to check for Revised.pbp
    If _FileExists("revised.pbp") Then

        Call SOURCE

    Else

        MsgText$ = "ERROR!!! PLAY-BY-PLAY SOURCE FILE NOT FOUND. CHECK THAT THE FILE 'REVISED.PBP' EXISTS AND IS READ-ONLY."
        BoxWidth = 22
        BoxHeight = (Len(MsgText$) / BoxWidth) + 3 'top and box lines + 1 extra
        Locate ((25 - BoxHeight) / 2), (80 - ((BoxWidth + 2) / 2))
        Call CSMsgBox(MsgText$, BoxWidth, Pass)
        A$ = GetKeyPress$
        Call CSMsgBox("", 0, Pass)

    End If

    'Close #7
    'End
    'System

End Sub


'----------------------------------------
'       StartingLineup Subroutine
'----------------------------------------
'This subroutine displays the existing
'lineup options and calls a routine TO
'let the user select the lineup
Sub StartingLineup (P9)

    skipLU% = 0

    Do
        'track assigned positions
        '   0 = unassigned
        Erase gameM%

        Locate 25, 1

        For I1 = 0 To 9

            If MAIN_desigHit <> 0 Or I1 <> 0 Then

                If MAIN_desigHit <> 1 Or I1 <> 1 Then

                    Locate 24, (I1 * 3) + 1
                    If gameM%(I1) = 0 Then Print fieldPos$(I1); "  ";
                    If gameM%(I1) = 1 Then Print "   ";

                    Locate 25, (I1 * 3) + 1
                    If gameM%(I1) = 0 Then Print I1; "  ";
                    If gameM%(I1) = 1 Then Print "   ";

                End If

            End If

        Next I1

        For I = 1 To 9

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate , 59: Print pitchers$(1 - P9, currPitcher%(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher%(1 - P9), 0) + 2)
            Call LineupPositions(P9, I)
            If C1 = 1 Then Exit For

        Next I

    Loop Until skipLU% = 0

End Sub


'----------------------------------------
'       LineupPositions Subroutine
'----------------------------------------
'This subroutine works with the user to
'help them select fielding positions for
'each place in the lineup
Sub LineupPositions (teamIdx, posIdx)

    'Do

    560 '
    Call PositionCursor(posIdx)
    Print Space$(18);
    Call PositionCursor(posIdx)

    'Get keypress for batter #
    I$ = GetKeyPress$

    If I$ = Chr$(27) Then

        Erase gameM%

        For J = 1 To 9
            fldPos%(teamIdx, J) = -1
            playerNum%(teamIdx, J) = -1
        Next

        C1 = 0

        For J = 0 To 22
            batRating(teamIdx, J, SS_B_Avail) = 0
        Next

        RP = 1

        For J = 0 To 22
            If batRating(teamIdx, J, SS_B_Active) <> 0 Then batRating(teamIdx, J, SS_B_Avail) = 99
        Next

    Else

        Call LetterToNumber(I$, I1)

        If UCase$(I$) <> "X" Then

            For J = 1 To 9
                If Not (J = posIdx Or (playerNum%(teamIdx, J) = I1 And fldPos%(teamIdx, J) = 1)) Then
                    If playerNum%(teamIdx, J) = I1 And UCase$(I$) <> "P" Then GoTo 560 'skip1% = 1
                End If
            Next J

            If Not (MAIN_desigHit = 1 And UCase$(I$) = "P") Or Not (batRating(teamIdx, I1, SS_B_Avail) <> 0 And UCase$(I$) <> "P") Then

                If UCase$(I$) = "P" Then

                    For J = 1 To 9
                        If J <> posIdx Then
                            If playerNum%(teamIdx, J) = currPitcher%(teamIdx) And fldPos%(teamIdx, J) = 1 Then skip2% = 1
                        End If
                    Next J

                    If skip2% <> 1 Then

                        playerNum%(teamIdx, posIdx) = currPitcher%(teamIdx)
                        Print pitchers$(teamIdx, currPitcher%(teamIdx));
                        gameM%(I1) = 1

                        Locate , 75
                        Print "P ";
                        fldPos%(teamIdx, posIdx) = 1
                        fielder%(teamIdx, 1) = currPitcher%(teamIdx)
                        gameM%(1) = 1

                        Locate 25, 1

                        For I0 = 0 To 9

                            If MAIN_desigHit <> 0 Or I0 <> 0 Then

                                If MAIN_desigHit <> 1 Or I0 <> 1 Then

                                    Locate 24, (I0 * 3) + 1
                                    If gameM%(I0) = 0 Then Print fieldPos$(I0); "  ";
                                    If gameM%(I0) = 1 Then Print "   ";

                                    Locate 25, (I0 * 3) + 1
                                    If gameM%(I0) = 0 Then Print I0; "  ";
                                    If gameM%(I0) = 1 Then Print "   ";

                                End If
                            End If

                        Next I0

                        '-- this is taken care of outside the routine
                        'If C1 = 1 Then Call ChangeLineup_Pregame

                    End If

                Else

                    playerNum%(teamIdx, posIdx) = I1
                    Print Using "# "; posIdx;
                    Print batters$(teamIdx, playerNum%(teamIdx, posIdx));

                    Locate , 74

                    680 '
                    'Input fielding position
                    'If MAIN_desigHit = 1 Then firstSlot = 1 Else firstSlot = 0
                    firstSlot = 1

                    Do
                        J$ = GetKeyPress$
                        J1 = Val(J$)
                    Loop Until J1 <> firstSlot

                    For J = 1 To 9
                        If J <> posIdx Then
                            If J1 = fldPos%(teamIdx, J) Then 680
                        End If
                    Next J

                    If Not (MAIN_desigHit = 0 And J1 = 0 Or UCase$(J$) = "X") Then

                        'Print position selected
                        Print fieldPos$(J1);
                        fldPos%(teamIdx, posIdx) = J1

                        fielder%(teamIdx, fldPos%(teamIdx, posIdx)) = playerNum%(teamIdx, posIdx)
                        gameM%(J1) = 1

                        'Highlight the player we selected
                        Locate I1 + 2, 2: Print "*";

                        Color teamRat_GAME(teamIdx, 11), teamRat_GAME(teamIdx, 12)
                        Print batters$(teamIdx, I1)
                        Color 15, 0

                        Locate 25, 1

                        For I1 = 0 To 9

                            If MAIN_desigHit <> 0 Or I1 <> 0 Then

                                If MAIN_desigHit <> 1 Or I1 <> 1 Then

                                    Locate 24, (I1 * 3) + 1
                                    If gameM%(I1) = 0 Then Print fieldPos$(I1); "  ";
                                    If gameM%(I1) = 1 Then Print "   ";

                                    Locate 25, (I1 * 3) + 1
                                    If gameM%(I1) = 0 Then Print I1; "  ";
                                    If gameM%(I1) = 1 Then Print "   ";

                                End If

                            End If

                        Next I1

                    End If

                End If 'Done checking for "P"

            Else

                GoTo 560 'skipLU% = 1

            End If 'Done checking skip

        End If 'Done checking for "X"

    End If 'Done checking for ESC

End Sub


'----------------------------------------
'    ChangeLineup_PreGame Subroutine
'----------------------------------------
'This routine gives the user a chance to
'make adjustments to the lineup before
'they start the game.
Sub ChangeLineup_PreGame (P9, reselect)

    Do

        C1 = 1
        Locate 25, 1
        Color 14: Print "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
        Color 15, 0

        If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = P9) Then
            I$ = "0"
        Else
            I$ = GetKeyPress$
        End If

        I = Val(I$)

        Select Case I

            Case 0:
                '0 can be us entering "0"
                'or it is also what gets assigned
                'if you use VAL against an alpha char
                Select Case UCase$(I$)
                    Case "0"
                        'Here we still could have selected R
                        If Inotloop% = 0 Then Call SaveLineup(P9)

                        For I = 1 To 9
                            If fldPos%(P9, I) <> 1 Then
                                batRating(P9, playerNum%(P9, I), SS_B_Avail) = 1
                            End If
                        Next I

                        Erase gameM%

                        C1 = 0

                    Case "R", Chr$(27):

                        Erase gameM%

                        For J = 1 To 9
                            fldPos%(P9, J) = -1
                            playerNum%(P9, J) = -1
                        Next

                        C1 = 0

                        For J = 0 To 22:
                            batRating(P9, J, SS_B_Avail) = 0
                        Next

                        RP = 1

                        '1001
                        For J = 0 To 22
                            If batRating(P9, J, SS_B_Active) <> 0 Then batRating(P9, J, SS_B_Avail) = 99
                        Next

                        reselect = 1

                    Case "L":
                        Call LINSWAP(P9)

                    Case "P":
                        Call POSSWAP(P9)

                End Select

            Case 1 To 9:
                Locate playerNum%(P9, I) + 2 - liveBatRating%, 2
                Print " "; batters$(P9, playerNum%(P9, I))

                Call LineupPositions(P9, I)

        End Select

    Loop Until UCase$(I$) = "0" Or UCase$(I$) = "R" Or I$ = Chr$(27)

End Sub


'----------------------------------------
'         SaveLineup Subroutine
'----------------------------------------
'This routine saves the lineup that has
'been generated to the manager profile for
'the team. It also gives the user a chance
'to assign a frequency to it.
Sub SaveLineup (idx%)

    Shared mgrFile(), gameT1%()

    If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = idx%) Then
        I$ = "N"
    Else
        Do
            Locate 25, 1: Print Space$(74);
            Locate 25, 1: Print "SAVE THIS LINE UP? (Y/N) ";
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

    End If

    If UCase$(I$) = "Y" Then

        Cls

        If pitchRating(1 - idx%, currPitcher%(1 - idx%), 0) = -1 Then Q2% = 1
        If pitchRating(1 - idx%, currPitcher%(1 - idx%), 0) = 0 Then Q2% = 0

        For I = 0 To 9
            Print I; " ";
            If MG%(idx%, 120 + (I + Q2% * 10) * 21) <> 999 Then Print "AVAILABLE"
            If MG%(idx%, 120 + (I + Q2% * 10) * 21) = 999 Then Print "LINEUP ALREADY SAVED "; MG%(idx%, 100 + (I + Q2% * 10) * 21); " % USAGE"
        Next

        Input "LINEUP # TO SAVE "; LN%
        Input "% OF TIME TO USE "; LU%

        If MG%(idx%, 120 + (LN% + Q2% * 10) * 21) <> 999 Then MG%(idx%, 120 + (LN% + Q2% * 10) * 21) = 999
        If Q2% = 1 Then LN% = LN% + 10

        MG%(idx%, 101 + LN% * 21) = MAIN_desigHit
        MG%(idx%, 100 + LN% * 21) = LU%

        For I = 1 To 9
            MG%(idx%, 110 + I + LN% * 21) = playerNum%(idx%, I)
            MG%(idx%, 101 + I + LN% * 21) = fldPos%(idx%, I)
        Next

        'idx% = P9
        MG%(idx%, 99) = 999
        MG%(idx%, 0) = 999
        mgrFile(idx%) = _TRUE

        teamYear$ = RTrim$(diskIDs$(idx%))

        Open diskPaths$(3) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #9 Len = MGR_SIZE_BYTES

        For I = 0 To 999
            Field #9, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
        Next

        For I = 0 To 999:
            LSet Q2$(I) = MKI$(MG%(idx%, I))
        Next

        Put #9, gameT1%(idx%)

        Close 9

    End If

End Sub

'----------------------------------------
'       SearchForLineup Subroutine
'----------------------------------------
'This routines searches for an existing
'lineup from the manager profiles.
Sub SearchForLineup (P9, Q2%)

    Shared PT$()

    '2500
    Cls
    If pitchRating(1 - P9, currPitcher%(1 - P9), 0) = -1 Then Q2% = 1
    If pitchRating(1 - P9, currPitcher%(1 - P9), 0) =  1 Then Q2% = 0
    'pitchRating(0-1,0-1,0); all indexes are 1's

    Print Tab(30); "LINEUPS FOUND VS. "; PT$(Q2%)
    Print " #0    "; MG%(P9, 100 + (0 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #1    "; MG%(P9, 100 + (1 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #2    "; MG%(P9, 100 + (2 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #3    "; MG%(P9, 100 + (3 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #4    "; MG%(P9, 100 + (4 + Q2% * 10) * 21); "%"

    For I = 0 To 4
        For I1 = 1 To 9

            If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 3, 16 * I + 1: Print "NONE FOUND"
            Else

                If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 2 + I1, 16 * I + 1: Print "         "
                Else
                    Locate 2 + I1, 16 * I + 1

                    If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 2 + I1, 16 * I + 12: Print fieldPos$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If
        Next I1
    Next I

    Print
    Print " #5    "; MG%(P9, 100 + (5 + Q2% * 10) * 21); "%";
    Locate , 17: Print " #6    "; MG%(P9, 100 + (6 + Q2% * 10) * 21); "%";
    Locate , 33: Print " #7    "; MG%(P9, 100 + (7 + Q2% * 10) * 21); "%";
    Locate , 49: Print " #8    "; MG%(P9, 100 + (8 + Q2% * 10) * 21); "%";
    Locate , 65: Print " #9    "; MG%(P9, 100 + (9 + Q2% * 10) * 21); "%"

    For I = 5 To 9

        For I1 = 1 To 9

            If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 = 1 Then
                Locate 14, 16 * (I - 5) + 1: Print "NONE FOUND"
            Else
                If MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 And I1 <> 1 Then
                    Locate 13 + I1, 16 * (I - 5) + 1: Print "         "
                Else
                    Locate 13 + I1, 16 * (I - 5) + 1

                    If MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 Then
                        Print "PITCHER";
                    Else
                        Print Left$(batters$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);
                    End If

                    Locate 13 + I1, 16 * (I - 5) + 12: Print fieldPos$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))
                End If
            End If

        Next

    Next

End Sub


'----------------------------------------
'      LineupFromProfile Subroutine
'----------------------------------------
'This subroutine attempts to load a selected
'lineup from the manager profile and presents
'a summary of it, if found.
Sub LineupFromProfile (lineupFound%, P9, skipLineChange%, MAIN_useMgrProf, I$, Q2%)

    lineupFound% = 0
    PCT% = 0

    For I = 0 To 9
        If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
            lineupFound% = lineupFound% + 1
            PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
        End If
    Next

    If lineupFound% = 0 Then

        Cls
        Print "ERROR:: NO LINEUPS FOUND...BE SURE CORRECT LINEUP ID WAS CHOSEN"

        If autoPlay Then
            noLUFound = 1
        Else
            'Locate 24, 1
            Locate 22, 1
            Print "PRESS ANY KEY TO CONTINUE"
            I$ = GetKeyPress$
        End If
        skipLineChange% = 1

    Else

        If ( autoPlay And MAIN_useMgrProf = 1 And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = P9) ) Or I$ = "3" Then

            'Create automated lineup
            RN = Int(Rnd(1) * PCT%) + 1
            PCT% = 0

            For I = 0 To 9
                If MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 Then
                    PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
                    If RN <= PCT% Then LN% = I: Exit For
                End If
            Next

        Else
            Do
                Locate 23, 1: Input "ENTER LINEUP NUMBER (0-9) "; LN%
            Loop Until LN% >= 0 And LN% <= 9

        End If

        If MG%(P9, 120 + (LN% + Q2% * 10) * 21) = 999 Then

            If Q2% = 1 Then LN% = LN% + 10

            If MAIN_desigHit = MG%(P9, 101 + LN% * 21) Then

                For I = 1 To 9
                    playerNum%(P9, I) = MG%(P9, 110 + I + LN% * 21)
                    fldPos%(P9, I) = MG%(P9, 101 + I + LN% * 21)
                Next

                LU% = 1
                Call LineupSummary(P9, skipLineChange%, LU%)

            Else
                'Technically we found a lineup
                'But we will skip prompting for
                'any lineup changes
                skipLineChange% = 1
            End If 'Done checking MAIN_desigHit
        Else
            'skip lineup changes
            skipLineChange% = 1
        End If 'Done checking for 999

    End If 'Done searching for lineup

End Sub

'----------------------------------------
'   LineupSummary Subroutine
'----------------------------------------
'This subroutine displays a summary of the
'starting lineup before allowing the user
'to make any adjustemts.
Sub LineupSummary (P9, reselect, LU%)

    For I = 1 To 9:

        testIdx = fldPos%(P9, I)

        If testIdx >= 0 Then

            fielder%(P9, fldPos%(P9, I)) = playerNum%(P9, I)

            If fldPos%(P9, I) = 1 Then
                playerNum%(P9, I) = currPitcher%(P9)
                fielder%(P9, 1) = currPitcher%(P9)
            Else
                batRating(P9, playerNum%(P9, I), SS_B_Avail) = 0
            End If

        End If

    Next I

    If LU% = 1 Then
        LU% = 0
        Call SelectBatters(batterFlag%, P9)
    End If

    For I = 10 To 22
        Locate I, 59: Print Space$(21);
    Next

    Call NEWLINES(P9)

    For I2 = 0 To 22:

        Locate I2 + 2, 2

        For I3 = 1 To 9
            If playerNum%(P9, I3) = I2 And fldPos%(P9, I3) <> 1 Then
                Print "*";
                Color teamRat_GAME(P9, 11), teamRat_GAME(P9, 12)
                Print batters$(P9, I2)
                Color 15, 0
            End If
        Next I3

    Next I2

    For I3 = 0 To 9:
        gameM%(I3) = 1
    Next I3

    Call ChangeLineup_PreGame(P9, reselect)

End Sub


'----------------------------------------
'        SelectBatters Subroutine
'----------------------------------------
'This subroutine lets you choose the batters
'for the game. It interfaces with various
'other routines to show you different types
'of batter stats to help you with your choices.
Sub SelectBatters (batterFlag%, P9)

    Inotloop% = 99

    Call SHOWBATTERS(P9)

    Color 15, 0
    Locate 5, 59: Print "OPPOSING PITCHER"
    Locate 6, 59: Print pitchers$(1 - P9, currPitcher%(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher%(1 - P9), 0) + 2)

    'TG = 0

    Do
        'Select batters first
        Locate 10, 59: Print "DISABLE BATTER (0-M)"
        Locate , 59: Print "Z-CONTINUE"
        Locate , 59: Print "X-REACTIVATE ALL"
        Locate , 59: Print "<TAB>-BAT/FIELD/SPLIT"

        If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = P9) Then
            'Autoplay = auto-select
            I$ = "Z"
            batterFlag% = 1
        Else
            I$ = GetKeyPress$

            'Handle ESC
            If I$ = Chr$(27) Then

                Erase gameM%

                For J = 1 To 9
                    fldPos%(P9, J) = -1
                    playerNum%(P9, J) = -1
                Next

                C1 = 0

                For J = 0 To 22
                    batRating(P9, J, SS_B_Avail) = 0
                Next

                RP = 1

                For J = 0 To 22
                    If batRating(P9, J, SS_B_Active) <> 0 Then batRating(P9, J, SS_B_Avail) = 99
                Next

                batterFlag% = 0

                Exit Sub

            End If

        End If

        ' Check if user pressed Tab
        If Asc(I$) = 9 Then TG = TG + 1

        If TG > 2 Or TG = 2 And gameTS%(P9, 0) <> 999 Then TG = 0

        If Asc(I$) = 9 Then
            '   Update display after TAB
            Select Case TG
                Case 0:
                    Call SHOWBATTERS(P9)
                Case 1:
                    Call SHOWFIELD(P9)
                Case 2:
                    Call ShowSplit(P9)
            End Select

        Else
            'Not TAB / ESC
            'Deteremine index of batter chosen
            Call LetterToNumber(I$, I1)

            'Assign field position
            Select Case UCase$(I$)

                Case "Z":
                    TG = 0
                    Call SHOWBATTERS(P9)

                Case "X":
                    For I3 = 0 To 22
                        If batRating(P9, I3, SS_B_Avail) = 99 And batRating(P9, I3, SS_B_Active) = 0 Then
                            batRating(P9, I3, SS_B_Avail) = 0
                            Locate I3 + 2, 3: Print batters$(P9, I3)
                        End If
                    Next I3

                Case Else:
                    If I1 >= 0 And I1 <= 22 Then
                        batRating(P9, I1, SS_B_Avail) = 99
                        Color 0, 7
                        Locate I1 + 2, 3: Print batters$(P9, I1);
                        Color 15, 0
                    End If

            End Select

        End If 'Done checking for TAB

        'If it wasn't an ESC, let's print the batter
        If I$ <> Chr$(27) Then

            Color 15, 0
            Locate 5, 59: Print "OPPOSING PITCHER"
            Locate 6, 59: Print pitchers$(1 - P9, currPitcher%(1 - P9)); " "; handed$(pitchRating(1 - P9, currPitcher%(1 - P9), 0) + 2)
            Inotloop% = 0
            batterFlag% = 1

        End If 'Done checking for ESC

    Loop Until UCase$(I$) = "Z"

End Sub

'----------------------------------------
'        SelectStadium Subroutine
'----------------------------------------
'This subroutine assists the user with the
'choice of any of the pre-created graphics
'for the stadiums.
Sub SelectStadium (MAIN_useVGA)
    Shared fileSeparator$

    Do
        'SELECT A DIFFERENT STADIUM
        PARK$(1) = "ANAHEIM STADIUM             "
        PARK$(2) = "THE ASTRODOME               "
        PARK$(3) = "OAKLAND-ALAMEADA COLISEUM   "
        PARK$(4) = "SKYDOME                     "
        PARK$(5) = "ATLANTA-FULTON COUNTY STADIU"
        PARK$(6) = "MILWAUKEE COUNTY STADIUM    "
        PARK$(7) = "BUSCH MEMORIAL STADIUM      "
        PARK$(8) = "WRIGLEY FIELD               "
        PARK$(9) = "DODGER STADIUM              "
        PARK$(10) = "OLYMPIC STADIUM             "
        PARK$(11) = "CANDLESTICK PARK            "
        PARK$(13) = "SEATTLE KINGDOME            "
        PARK$(12) = "CLEVELAND MUNICIPAL STADIUM "
        PARK$(14) = "SHEA STADIUM                "
        PARK$(15) = "BALTIMORE MEMORIAL STADIUM  "
        PARK$(16) = "JACK MURPHY STADIUM         "
        PARK$(17) = "VETERAN'S STADIUM           "
        PARK$(18) = "THREE RIVERS STADIUM        "
        PARK$(19) = "ARLINGTON STADIUM           "
        PARK$(20) = "RIVERFRONT STADIUM          "
        PARK$(21) = "FENWAY PARK                 "
        PARK$(22) = "ROYALS/KAUFMANN STADIUM     "
        PARK$(23) = "TIGER/BRIGGS/NAVIN STADIUM  "
        PARK$(24) = "H.H.H. METRODOME            "
        PARK$(25) = "OLD COMISKEY PARK           "
        PARK$(26) = "OLD YANKEE STADIUM          "
        PARK$(27) = "LOS ANGELES COLISEUM        "
        PARK$(28) = "COLT STADIUM                "
        PARK$(29) = "CONNIE MACK/SHIBE STADIUM   "
        PARK$(30) = "EXHIBITION FIELD (TOR)      "
        PARK$(31) = "PILOT FIELD                 "
        PARK$(32) = "COUNTY STADIUM (MIL)        "
        PARK$(33) = "SPORTSMAN'S PARK            "
        PARK$(34) = "EBBETTS FIELD               "
        PARK$(35) = "JARRY PARK                  "
        PARK$(36) = "POLO GROUNDS                "
        PARK$(37) = "CROSLEY FIELD               "
        PARK$(38) = "RFK STADIUM                 "
        PARK$(39) = "METROPOLITAN STADIUM        "
        PARK$(40) = "FORBES FIELD                "
        PARK$(41) = "GRIFFITH STADIUM            "
        PARK$(42) = "SAN DIEGO STADIUM           "
        PARK$(43) = "KAN CITY MUNICIPAL STADIUM  "
        PARK$(44) = "SICKS STADIUM               "
        PARK$(45) = "LOS ANGELES WRIGLEY FIELD   "
        PARK$(46) = "SEALS STADIUM               "
        PARK$(47) = "*AMERICAN LEAGUE            "
        PARK$(48) = "*AMERICAN LEAGUE II         "
        PARK$(49) = "HILLTOP PARK                "
        PARK$(50) = "POLO GROUNDS (1913-1922)    "
        PARK$(51) = "BRAVES FIELD (1915-1952)    "
        PARK$(52) = "BRAVES FIELD (1915-1916)    "
        PARK$(53) = "SOUTHSIDE PARK              "
        PARK$(54) = "BAKER BOWL                  "
        PARK$(55) = "CAMDEN YARDS                "
        PARK$(56) = "COLUMBIA PARK               "
        PARK$(57) = "EXPOSITION PARK             "
        PARK$(58) = "LEAGUE PARK/PALACE OF FANS  "
        PARK$(59) = "WEST SIDE PARK              "
        PARK$(60) = "SOUTH END GROUNDS           "
        PARK$(61) = "HUNTINGTON AVENUE GROUNDS   "
        PARK$(62) = "LEAGUE PARK                 "
        PARK$(63) = "BENNETT PARK                "
        PARK$(64) = "WASHINGTON PARK             "
        PARK$(65) = "MILE HIGH STADIUM           "
        PARK$(66) = "JOE ROBBIE STADIUM          "
        PARK$(67) = "JACOBS FIELD                "
        PARK$(68) = "BALLPARK AT ARLINGTON       "
        PARK$(69) = "COORS STADIUM               "
        PARK$(70) = "NEW YANKEE STADIUM          "
        PARK$(71) = "NEW COMISKEY PARK           "
        PARK$(72) = "BUSCH STADIUM - GRASS       "
        PARK$(73) = "TURNER FIELD                "
        PARK$(74) = "BANK ONE BALLPARK           "
        PARK$(75) = "TROPICANA FIELD             "
        PARK$(76) = "SAFECO FIELD (SEA)          "
        PARK$(77) = "ENRON PARK (HOU)            "
        PARK$(78) = "PACIFIC BELL PARK (SF)      "
        PARK$(79) = "COMERICA PARK (DET)         "
        PARK$(80) = "                            "
        PARK$(81) = "PNC PARK                    "
        PARK$(82) = "MILLER PARK-MILWAUKEE       "
        PARK$(83) = "UNKNOWN                     "
        PARK$(99) = "GENERIC                     "

        Call TEAMBOX(5, 1)

        'Determine park from the team data
        PARK$ = Right$(Str$(teamRat_GAME(1, 13)), Len(Str$(teamRat_GAME(1, 13))) - 1)

        If MAIN_dayOrNight = 0 Then
            '0 = daytime
            PARK$ = PARK$ + "D.png"
        Else
            '1 = nighttime
            PARK$ = PARK$ + "N.png"
        End If

        'Display detected park
        Locate 7, 11: Print "CURRENT PARK IS:"
        Locate , 11: Print PARK$(teamRat_GAME(1, 13))

        Locate , 11

        If _FileExists("." + fileSeparator$ + "parks" + fileSeparator$ + PARK$) Then
            Print "GRAPHIC EXISTS"
        Else
            '99 is the Generic Park
            Print "GRAPHIC NOT AVAILABLE"
            PARK$ = "99N.png"
        End If

        'We may have checked if the file associated with the park
        'exists, but we haven't checked if any graphics exist at all
        If Not autoPlay Then

            Do
                Locate 10, 11
                Print "SELECT A DIFFERENT STADIUM? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            Color 15, 0

            If UCase$(I$) = "Y" Then

                Cls
                Locate 23, 10: Print "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
                Start% = 1

                Do
                    Selection% = 1
                    Count% = 15
                    ExitCode% = 99
                    FileFGColor = 15: FileBGColor = 1
                    HiLiteFGColor = 0: HiLiteBGColor = 7
                    Spacing% = 1
                    Row% = 5

                    menuWidth% = Len(menuChoices$(1))
                    Column% = (80 - menuWidth% - 2) / 2

                    Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor%, FileBGColor%)

                    _MouseShow "Default"
                    MAMenu PARK$(), Selection%, Start%, Count%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor, HiLiteBGColor, 15, 1, 1, Row%, Column%
                    _MouseHide

                    Color , 0

                    If ExitCode% <> 27 Then

                        PARK$ = Right$(Str$(Selection%), Len(Str$(Selection%)) - 1)
                        If MAIN_dayOrNight = 0 Then PARK$ = PARK$ + "D.png" Else PARK$ = PARK$ + "N.png"
                        Color 15, 0

                        If _FileExists("." + fileSeparator$ + "parks" + fileSeparator$ + PARK$) Then
                            foundPark% = 1
                            teamRat_GAME(1, 13) = Selection%
                            gameStadiums$(1) = PARK$(Selection%)
                        Else
                            Do
                                Locate 10, 8
                                Print "STADIUM GRAPHIC NOT AVAILABLE"
                                Locate , 8: Print "USE SELECTED PARK RATINGS WITH"
                                Locate , 8: Print "GENERIC DISPLAY? (Y/N)"
                                J$ = GetKeyPress$
                            Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                            If I$ = "Y" Then
                                teamRat_GAME(1, 13) = Selection%: gameStadiums$(1) = PARK$(Selection%)
                                If MAIN_dayOrNight = 0 Then PARK$ = "99D.png" Else PARK$ = "99N.png"
                            End If

                        End If

                    Else
                        BO% = 1
                    End If

                Loop Until UCase$(J$) = "Y" Or ExitCode% = 27 Or foundPark% = 1

            End If

        End If 'Done checking auto play

        'We are set for autoplay, and checked if the stadium
        'graphic exists for the park, but not if the generic
        'stadium graphic exists. We would only be in this
        'position if no graphics exist at all!

        'So instead, let's switch to regular mode

        If Not _FileExists("." + fileSeparator$ + "parks" + fileSeparator$ + PARK$) Then MAIN_useVGA = 0

    Loop Until BO% = 0 Or UCase$(I$) = "N" Or ExitCode% = 27

End Sub


'----------------------------------------
'      PitchingRotations Subroutine
'----------------------------------------
'This subroutine guides the user through
'the various ways that pitching rotations
'can be configured for the game.
Sub PitchingRotations (computerRotations%, P9)

    Dim gamesStarted%(21, 1)

    Cls
    Call INFO

    Locate 2, 1

    For I = 1 To 5
        Color 14: Print "(";: Print Using "#"; I;: Print ") ";: Color 15: Print "MAN ROTATION"
    Next

    Print "YOU MAY CONSIDER THE FOLLOWING GUIDELINES:"
    Print "MAX GAMES STARTED BY ANY PITCHER           ROTATION SIZE"
    Print "          32                                    5"
    Print "          41                                    4"
    Print "          54                                    3"
    Print "          75                                    2"
    Print "         ALL                                    1"
    Print

    For I = 0 To 6

        Locate I + 15, 1
        If pitchers$(P9, I) <> "XXX" Then
            Print pitchers$(P9, I); Tab(18); pitchRating(P9, I, SS_P_GS);
        End If

        Locate , 25
        If pitchers$(P9, I + 7) <> "XXX" Then
            Print pitchers$(P9, I + 7); Tab(43); pitchRating(P9, I + 7, SS_P_GS);
        End If

        Locate , 50
        If pitchers$(P9, I + 14) <> "XXX" Then
            Print pitchers$(P9, I + 14); Tab(68); pitchRating(P9, I + 14, SS_P_GS)
        End If

    Next

    If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = P9) Then
        '   4685
        gamesStarted = 0

        For I = 0 To 21
            If pitchRating(P9, I, SS_P_GS) > gamesStarted Then gamesStarted = pitchRating(P9, I, SS_P_GS)
        Next

        'Auto-determine # of men in rotation
        'based on # of games started?
        I = 1
        Select Case gamesStarted

            Case 1 To 32: I = 5
            Case 33 To 41: I = 4
            Case 42 To 54: I = 3
            Case 55 To 75: I = 2

        End Select

    Else

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 And I <= 5

    End If

    '4615
    MG%(P9, 1) = I
    MG%(P9, 2) = 0

    For I = 3 To 36
        MG%(P9, I) = -1
    Next

    For I = 41 To 56
        MG%(P9, I) = 0
    Next

    MG%(P9, 0) = 999

    Inotloop% = 99

    Call PrintPitcherInfo(P9)

    If computerRotations% = 0 Then

        For I = 0 To MG%(P9, 1) - 1

            Do

                'We need to skip this in certain circumstances
                'Right now, it is being loaded for auto-play
                Locate 15, 59: Print "STARTER "; I + 1

                Do
                    I$ = GetKeyPress$
                    Call LetterToNumber(I$, I1)
                Loop Until I1 <= 21 And MG%(P9, 3) <> I1 And MG%(P9, 4) <> I1 And MG%(P9, 5) <> I1 And MG%(P9, 6) <> I1 And MG%(P9, 7) <> I1

                Locate 16, 59: Print pitchers$(P9, I1)

                Do
                    Locate 17, 59: Print "ANY CHANGE? (Y/N)";
                    J$ = GetKeyPress$
                Loop Until UCase$(J$) = "Y" Or UCase$(J$) = "N"

                If UCase$(J$) = "Y" Then
                    For I2 = 15 To 17: Locate I2, 59: Print Space$(17): Next
                End If

            Loop Until UCase$(J$) = "N"

            MG%(P9, I + 3) = I1

            For I2 = 15 To 17: Locate I2, 59: Print Space$(18): Next

        Next

    Else

        ' COMPUTER SET ROTATION
        'Make sure our next team isn't auto-set
        'without a choice
        computerRotations% = 0

        For I = 0 To 21
            If pitchers$(P9, I) = "XXX" Then
                gamesStarted%(I, 0) = 0
                gamesStarted%(I, 1) = -1
            Else
                gamesStarted%(I, 0) = pitchRating(P9, I, SS_P_GS)
                gamesStarted%(I, 1) = I
            End If
        Next

        For Z = 0 To 20:
            For Z1 = Z + 1 To 21:
                If gamesStarted%(Z, 0) < gamesStarted%(Z1, 0) Then
                    Swap gamesStarted%(Z, 0), gamesStarted%(Z1, 0)
                    Swap gamesStarted%(Z, 1), gamesStarted%(Z1, 1)
                End If
            Next Z1
        Next Z

        For I = 0 To MG%(P9, 1) - 1
            MG%(P9, I + 3) = gamesStarted%(I, 1)
        Next

        I1 = 0

        For I = MG%(P9, 1) To 21
            I1 = I1 + 1

            If gamesStarted%(I, 0) <> 0 Then
                MG%(P9, I1 + 20) = gamesStarted%(I, 1)
            Else
                Exit For
            End If
        Next

    End If

    I1 = I1 - 1

    If MG%(P9, 1) = 1 Then
        MG%(P9, 13) = 100
    Else
        If MG%(P9, 1) = 2 Then gamesStarted% = 75
        If MG%(P9, 1) = 3 Then gamesStarted% = 54
        If MG%(P9, 1) = 4 Then gamesStarted% = 41
        If MG%(P9, 1) = 5 Then gamesStarted% = 32

        For I = 1 To MG%(P9, 1)
            MG%(P9, I + 12) = Int(pitchRating(P9, MG%(P9, I + 2), SS_P_GS) * 100 / gamesStarted% + .5)
        Next

        SS% = 0

        For I = 21 To 36
            If MG%(P9, I) >= 0 And MG%(P9, I) <= 21 Then
                SS% = SS% + pitchRating(P9, MG%(P9, I), SS_P_GS)
            End If
        Next

        For I = 21 To 36
            If MG%(P9, I) >= 0 And MG%(P9, I) <= 21 Then
                MG%(P9, I + 20) = Int(pitchRating(P9, MG%(P9, I), SS_P_GS) * 100 / SS% + .5)
            End If
        Next

    End If

End Sub


'----------------------------------------
'       ComputerLineups Subroutine
'----------------------------------------
'This subroutine is built with the logic
'to automatically select batters, before
'presenting the user a final summary of the
'results.
Sub ComputerLineups (batterFlag%, P9, noLineups, reselect)

    Shared fldNo(), gameF()
    Shared playNo%()

    Dim position(23, 1)

    For I = 0 To 22
        If batters$(P9, I) = "XXX" Then batRating(P9, I, SS_B_Avail) = 98
    Next

    Call SelectBatters(batterFlag%, P9)

    If batterFlag% <> 0 Then

        For I = 10 To 22
            Locate I, 59: Print Space$(21);
        Next

        Erase gameF, fldNo

        For I = 1 To 9:
            playNo%(I) = -1
        Next

        M = 0

        For I = 9 To 1 Step -1

            J = I

            For I2 = 0 To 22:
                position(I2, 0) = 0
                position(I2, 1) = -1
            Next

            I2 = 0

            If I = 1 Then
                '5120
                If MAIN_desigHit = 0 Then
                    fldNo(9) = 1
                    Exit For
                Else
                    J = J - 1
                    I2 = 0
                    For I1 = 0 To 22
                        ' Print #11, Using "DH_! I2: ##  I1: ##  status: ###  batter: &"; I2, I1, batRating(P9, I1, SS_B_Avail), batters$(P9, I1)

                        If batRating(P9, I1, SS_B_Avail) <= 0 And batters$(P9, I1) <> "XXX" Then

                            If     batRating(P9, I1, SS_B_Pos1) = 0 Then
                                If     batRating(P9, I1, SS_B_Pos2) = -1 Then
                                                                        position(I2, 0) = batRating(P9, I1, SS_B_AB)
                                ElseIf batRating(P9, I1, SS_B_Pos3) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .9 + .5)
                                ElseIf batRating(P9, I1, SS_B_Pos4) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .8 + .5)
                                Else
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .7 + .5)
                                End If

                            ElseIf batRating(P9, I1, SS_B_Pos2) = 0 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .5  + .5)
                            ElseIf batRating(P9, I1, SS_B_Pos3) = 0 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .3  + .5)
                            ElseIf batRating(P9, I1, SS_B_Pos4) = 0 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .2  + .5)
                            Else
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .05 + .5)    ' no DH experience
                            End If

                            position(I2, 1) = I1

                            ' Print #11, Using "Player ## could DH, i: ##  p0 = ####  p1 = ####  &"; I1, I, position(I2, 0), position(I2, 1), batters$(P9, I1)

                            I2 = I2 + 1

                        End If

                    Next I1

                End If
            Else
                'I <> 1
                ' For each fielding position I (1-9), for each active player who plays that position, and store his
                ' "normalized" at-bats in position(I2, 0) and the field position # in position(I2, 1)

                For I1 = 0 To 22:
                    If batRating(P9, I1, SS_B_Avail) <= 0 And batters$(P9, I1) <> "XXX" Then

                        If batRating(P9, I1, SS_B_Pos1) = I Then
                            If     batRating(P9, I1, SS_B_Pos2) = -1 Then
                                                                        position(I2, 0) = batRating(P9, I1, SS_B_AB)
                            ElseIf batRating(P9, I1, SS_B_Pos3) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .9 + .5)
                            ElseIf batRating(P9, I1, SS_B_Pos4) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .8 + .5)
                            Else
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .7 + .5)
                            End If

                        ElseIf batRating(P9, I1, SS_B_Pos2) = I Then
                            If     batRating(P9, I1, SS_B_Pos3) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                            ElseIf batRating(P9, I1, SS_B_Pos4) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                            Else
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(P9, I1, SS_B_Pos3) = I Then
                            If     batRating(P9, I1, SS_B_Pos4) = -1 Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                            Else
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                            End If

                        ElseIf batRating(P9, I1, SS_B_Pos4) = I Then
                                                                        position(I2, 0) = Int(batRating(P9, I1, SS_B_AB) * .1 + .5)
                        End If

                        position(I2, 1) = I1

                        ' Print #11, Using "Player ##: & could play position ##  p0 = ####  p1 = ####"; I1, batters$(P9, I1), I, position(I2, 0), position(I2, 1)

                        I2 = I2 + 1

                    End If

                Next I1

            End If 'Done checking value of I

            I2 = I2 - 1
            AB% = 0

            For I1 = 0 To I2
                AB% = AB% + position(I1, 0)

                ' Print #11, Using "I: ##  I2: ##  I1: ##  AB: ####  batter: &"; _
                '     I, I2, I1, AB%, _IIF( position(I1, 1) >= 0, batters$(P9, position(I1, 1)), "???" )
            Next

            ' Print #11,
            ' For I1 = 0 To 22
                ' Print #11, Using "I: ##  I2: ##  I1: ##  pos0: ####  pos1: ####  AB: ####  batter: &"; _
                '     I, I2, I1, position(I1, 0), position(I1, 1), AB%, _IIF( position(I1, 1) >= 0, batters$(P9, position(I1, 1)), "???" )
            ' Next
            ' Print #11,

            'I2 = 4
            'AB% = 630
            RN = Int(Rnd(1) * AB%) + 1

            ' Print #11, "---"

            ' For each fielding position I (1-9), find a position(I1, 0) [i.e., at-bats] value that is above the random threshold, and
            ' assign the player to that position

            lineupDone% = 0
            For I1 = 0 To I2
                If RN <= position(I1, 0) Then
                    gameF(J) = position(I1, 1)
                    batRating(P9, position(I1, 1), SS_B_Avail) = 1
                    lineupDone% = 1

                    ' Print #11, Using "Found xxx I: ##  J: ##  I2: ##  I1: ##  pos0: ####  pos1: ####  AB: ####  RN: ####  batter: &"; _
                    '     I, J, I2, I1, position(I1, 0), position(I1, 1), AB%, RN, batters$(P9, position(I1, 1))

                    Exit For
                Else
                    position(I1 + 1, 0) = position(I1 + 1, 0) + position(I1, 0)

                    ' Print #11, Using "NOT Found I: ##  J: ##  I2: ##  I1: ##  pos0: ####  pos1: ####  AB: ####  RN: ####  batter: &"; _
                    '     I, J, I2, I1, position(I1, 0), position(I1, 1), AB%, RN, batters$(P9, position(I1, 1))
                End If

                ' Print #11, Using "AB: ####  RN: ####  I1: ##  I2: ##  pos0: ####  pos1: ####  pos11: ####  J: ##  FJ: ####  batter: &  lineupDone: ##"; _
                '     AB%, RN, I1, I2, position(I1, 0), position(I1+1, 0), position(I1, 1), J, gameF(J), batters$(P9, I1), lineupDone%
            Next I1
            ' Print #11, "---"

            If lineupDone% <> 1 Then
                Locate 25, 1: Print "CANNOT MAKE A LINEUP!!...";
                Print #11, "CANNOT MAKE A LINEUP!!..."
                noLineups = 1
                'reselect = 1

                Erase gameM%

                For J = 1 To 9:
                    fldPos%(P9, J) = -1
                    playerNum%(P9, J) = -1
                Next

                C1 = 0

                For J = 0 To 22
                    If batRating(P9, J, SS_B_Avail) <> 99 Then batRating(P9, J, SS_B_Avail) = 0
                Next

                If Not autoPlay Then
                    I$ = GetKeyPress$
                    Exit For
                End If

            End If 'done checking lineup

        Next I

        Call SortHitters(P9)

        Call LineupSummary(P9, reselect, LU%)

    End If 'done checking batterFlag

End Sub


'----------------------------------------
'         SortHitters Subroutine
'----------------------------------------
'This subroutine is used to perform a sort
'on all the hitters before presenting the
'lineup summary to the user.
Sub SortHitters (P9)

    Shared fldNo(), playNo%(), currPitcher%()

    Shared USE_SLUG, USE_LH, USE_OPS, USE_OBA_SB                ' Shared with DoSort()
    USE_SLUG = 0 : USE_LH = 1 : USE_OPS = 2 : USE_OBA_SB = 3

    ' Call BenDisplay (0)

    ' Note: At least for now, I am not using the original "USE_LH" algorithm,
    ' until I understand it better  :)

    ' Put the 3 hitters with the highest SLG Pct in the 3-5 holes in the lineup card
    Call DoSort(3, USE_SLUG, P9)
    Call DoSort(4, USE_SLUG, P9)
    Call DoSort(5, USE_SLUG, P9)

    ' Of the remaining hitters, put the 2 with the highest OBA plus SB in the 1-2 holes
    Call DoSort(1, USE_OBA_SB, P9)
    Call DoSort(2, USE_OBA_SB, P9)

    ' For the rest, slot them in descending order by OPS (on-base plus slugging)
    Call DoSort(6, USE_OPS, P9)
    Call DoSort(7, USE_OPS, P9)
    Call DoSort(8, USE_OPS, P9)

    If MAIN_desigHit = 1 Then
        Call DoSort(9, USE_OPS, P9)
    Else
        playNo%(9) = currPitcher%(P9)
        fldNo = 1
    End If


    'I have not found a cause, but sometimes
    'spot 8 doesn't get populated. The loop
    'in DoSort exits too early.
    'My fix here is to find an available position
    'and assign that to the slot.
    'We start at slot 3 b/c the first few slots
    'are often main pitchers and rarely get
    'picked by the DoSort routine
    If playNo%(8) < 0 Then

        'Find an open spot
        'Don't start with first few
        openPos = 3
        currIdx = 1

        Do
            'If current spot is "used", move to next spot
            If (playNo%(currIdx) = openPos) Then openPos = openPos + 1
            currIdx = currIdx + 1
        Loop Until currIdx > 9

        playNo%(8) = openPos

        'We need to find whatever number is missing
        For X = 1 To 9
            assigned(X) = fldNo(X)
        Next X

        For a = 9 To 1 Step -1
            For B = 1 To 9
                If assigned(B) > assigned(a) Then Swap assigned(a), assigned(B)
            Next B
        Next a

        'Find an open spot
        '--Don't look for DH / pitcher
        openPos = 2
        currIdx = 1
        Do
            'If current spot is "used", move to next spot
            If (assigned(currIdx) = openPos) Then openPos = openPos + 1
            currIdx = currIdx + 1
        Loop Until currIdx > 9

        fldNo(8) = openPos

    End If

    For I = 1 To 9
        playerNum%(P9, I) = playNo%(I)
        fldPos%(P9, I) = fldNo(I)
    Next

End Sub

Sub BenDisplay (idx)
    _Dest _Console
    For lineupSlot = 1 To 9
        For rostNum% = 0 To 22:
            If playerNum%(P9, lineupSlot) = rostNum% Then
                Print Using "idx: ##  rosterNum: ##  lineupSlot: ##  batter: &"; _
                    idx, rostNum%, lineupSlot, batters$(P9, rostNum%)
            End If
        Next
    Next
    Print ""
    _Dest 0
    foo$ = GetKeyPress$
End Sub

'----------------------------------------
'      DoSort Subroutine
'----------------------------------------
'I literally have no clue what this sub-
'routine does. It has very little context
'that hints to it's purpose.
'It is only called by the routine I've
'named "SortHitters", and likely DOES
'some of the actual sorting as it's
'called after each lineup position.
Sub DoSort (B, calcMode, P9)

    Shared fldNo(), gameF()
    Shared playNo%()
    Shared USE_SLUG, USE_LH, USE_OPS, USE_OBA_SB

    ' Dim JJ!, R!, slug!, stealPct!

    theMax! = -100
    UFldNo = -1
    TPlayNo = -1

    For L = 0 To 9

        If MAIN_desigHit = 0 And L = 0 Then L = 2
        If MAIN_desigHit = 1 And L = 1 Then L = 2

        found = _FALSE

        For Z = 1 To 9
            If playNo%(Z) = gameF(L) Then
                'If we found the matching player, loop to next lineup position
                found = _TRUE
                Exit For
            End If
        Next Z

        If Not found Then

            'Assign J based on lineup position
            J = gameF(L)

            Select Case calcMode
                Case USE_SLUG
                    slug! = (batRating(P9, J, SS_B_H) + batRating(P9, J, SS_B_2B) + batRating(P9, J, SS_B_3B) * 2 + batRating(P9, J, SS_B_HR) * 3) / batRating(P9, J, SS_B_AB)

                    If slug! >= theMax! Then
                        theMax! = slug!
                        TPlayNo = J
                        UFldNo = L
                    End If

                Case USE_LH
                    JJ = J

                    'IF A PITCHER THEN 268
                    stealPct! = ( batRating(P9, JJ, SS_B_H) - batRating(P9, JJ, SS_B_3B) - batRating(P9, JJ, SS_B_HR) + batRating(P9, JJ, SS_B_W) ) * .7

                    If stealPct! = 0 Then stealPct! = 1

                    stealPct! = batRating(P9, JJ, SS_B_SB) / stealPct!

                    If stealPct! >= .02 Then
                        QQ! = stealPct! + .18
                        R! = Int(QQ! * (18 + 2 * Abs((QQ! < .34))))
                    Else
                        R! = 2 + Int(stealPct! * 100)
                        If batRating(P9, JJ, 1) = 2 Then R! = R! - 1
                    End If

                    If (batRating(P9, JJ, 1) = 6 Or batRating(P9, JJ, 1) = 8) And R! < 7 Then R! = R! + 1

                    If R! > 9 Then R! = 9
                    If R! < 0 Then R! = 1

                    batAvg% = 100 * batRating(P9, J, SS_B_H) / batRating(P9, J, SS_B_AB) - 27
                    If batAvg% < 0 Then batAvg% = 3 * batAvg%

                    JJ! = 3 * R! + batAvg%

                    If JJ! > theMax! Then
                        theMax! = JJ!
                        TPlayNo = J
                        UFldNo = L
                    End If

                Case USE_OPS
                    slug! = ( batRating(P9, J, SS_B_H) + batRating(P9, J, SS_B_2B) + batRating(P9, J, SS_B_3B) * 2 + batRating(P9, J, SS_B_HR) * 3 ) / batRating(P9, J, SS_B_AB)
                    oba!  = ( batRating(P9, J, SS_B_H) + batRating(P9, J, SS_B_W) ) / ( batRating(P9, J, SS_B_AB) + batRating(P9, J, SS_B_W) )
                    ops! = slug! + oba!

                    If ops! >= theMax! Then
                        theMax! = ops!
                        TPlayNo = J
                        UFldNo = L
                    End If

                Case USE_OBA_SB
                    oba!  = ( batRating(P9, J, SS_B_H) + batRating(P9, J, SS_B_W) + batRating(P9, J, SS_B_SB) ) / ( batRating(P9, J, SS_B_AB) + batRating(P9, J, SS_B_W + batRating(P9, J, SS_B_CS)) )

                    If oba! >= theMax! Then
                        theMax! = oba!
                        TPlayNo = J
                        UFldNo = L
                    End If

        End Select

        End If 'Done checking found

    Next L

    playNo%(B) = TPlayNo
    fldNo(B) = UFldNo

End Sub


'----------------------------------------
'       SelectPitchers Subroutine
'----------------------------------------
'This subroutine walks the user through
'all available options for selecting a
'rotation of pitchers.
Sub SelectPitchers (idx%, cancelPitchers%, computerRotations%)

    Shared gameT1%(), mgrFile(), startPitcher()

    ' *** PITCHING ROTATIONS ***
    Inotloop% = 99
    skipDisablePitchers% = 0

    Call PrintPitcherInfo(idx%)

    Locate 15, 57: Print "0-MANUALLY PICK STARTER"
    Locate 16, 57: Print "1-USE PRE-SET ROTATION"
    Locate 17, 57: Print "2-ENTER A NEW ROTATION"

    If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = idx%) Then
        'For automated games, use preset rotation
        'This will end up being computer-generated
        choice = 1

    Else

        Do
            I$ = GetKeyPress$
            If I$ <> Chr$(27) Then
                choice = Val(I$)
            End If
        Loop Until choice >= 0 And choice <= 2 Or I$ = Chr$(27)

    End If

    If I$ <> Chr$(27) Then

        Select Case choice

            Case 0:
                'manual pick starter means we are
                'not going to disable anyone, but
                'just simply pick 1 pitcher
                skipDisablePitchers = 1
                startPitcher(idx%) = -1

            Case 1:
                'Pre-Set Rotation
                Cls

                If Not mgrFile(idx%) Or MG%(idx%, 99) <> 999 Then

                    If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = idx%) Then
                        computerRotations% = 1
                    Else
                        'This should instead go to automatic disable of rotations???
                        Cls
                        Print "ERROR:: ROTATION NOT FOUND"
                        Print
                        Print "DO YOU WANT TO ENTER A ROTATION? (Y/N)"

                        Do
                            I$ = GetKeyPress$
                        Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"
                    End If

                    'Don't we want to skipDisablePitchers
                    'when we are doing computerRotations
                    'as well ????
                    If computerRotations% = 0 Then

                        If UCase$(I$) = "N" Then
                            skipDisablePitchers% = 1
                        Else
                            Print "COMPUTER SET ROTATION? (Y/N) ";

                            Do
                                I$ = GetKeyPress$
                            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                            If UCase$(I$) = "Y" Then computerRotations% = 1

                        End If

                    End If

                    Call PitchingRotations(computerRotations%, idx%)

                Else
                    skipDisablePitchers% = 0
                End If

            Case 2:

                Do
                    Cls
                    Print "COMPUTER SET ROTATION? (Y/N) ";
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then computerRotations% = 1

                Call PitchingRotations(computerRotations%, idx%)

        End Select

        If skipDisablePitchers% <> 1 Then
            'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

            'For I = 0 To MG%(idx%, 1) - 1
            '    pitchRating(idx%, MG%(idx%, 3 + I), SS_P_Avail) = 99
            'Next

            Do
                Call d100(RN)

                ' If MG%(idx%, 2) = -1 Then MG%(idx%, 2) = 0  '[bm] KLUDGE'

                pitchRating(idx%, MG%(idx%, MG%(idx%, 2) + 3), SS_P_Avail) = 0

                If RN <= MG%(idx%, MG%(idx%, 2) + 10 + 3) Or MG%(idx%, MG%(idx%, 2) + 10 + 3) = 0 Then
                    startPitcher(idx%) = MG%(idx%, MG%(idx%, 2) + 3)
                    doneAutoDisable% = 1
                Else

                    Call d100(RN)
                    SP% = 0

                    For J = 21 To 36

                        If MG%(idx%, J) <> -1 And MG%(idx%, J) >= 0 And MG%(idx%, J) <= 21 Then

                            SP% = SP% + MG%(idx%, J + 20)

                            If RN <= SP% And MG%(idx%, J) <> -1 Then
                                pitchRating(idx%, MG%(idx%, J), SS_P_Avail) = 0
                                startPitcher(idx%) = MG%(idx%, J)
                                doneAutoDisable% = 1
                                Exit For
                            End If

                        End If

                    Next

                End If

            Loop Until doneAutoDisable% = 1

            MG%(idx%, 2) = MG%(idx%, 2) + 1

            If MG%(idx%, 2) = MG%(idx%, 1) Then MG%(idx%, 2) = 0

            MG%(idx%, 99) = 999
            MG%(idx%, 0) = 999
            mgrFile(idx%) = _TRUE

            teamYear$ = RTrim$(diskIDs$(idx%))

            Open diskPaths$(3) + MGR_FILE_NAME$ + "." + teamYear$ For Random As #1 Len = MGR_SIZE_BYTES

            For I = 0 To 999
                Field #1, I * 2 As Q$, 2 As Q2$(I), MGR_SIZE_BYTES - I * 2 - 2 As Q$
            Next

            For I = 0 To 999: LSet Q2$(I) = MKI$(MG%(idx%, I)): Next

            Put #1, gameT1%(idx%)

            Close #1

        End If

        Do

            Inotloop% = 99

            Call PrintPitcherInfo(idx%)

            Locate 15, 58: Print "DISABLE PITCHER (0-L)"
            Locate , 58: Print "Y-GRANT DAY OFF"
            Locate , 58: Print "Z-CONTINUE"
            Locate , 58: Print "X-ACTIVATE ALL PITCHERS"

            If autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = idx%) Then
                I$ = "Z"
            End If

            While UCase$(I$) <> "Z" And I$ <> Chr$(27)

                I$ = GetKeyPress$

                If I$ <> Chr$(27) And UCase$(I$) <> "Z" Then

                    Call LetterToNumber(I$, I1)

                    Select Case UCase$(I$)
                        Case "Y":
                            ':GRANT DAY OFF
                            If MAIN_useRest = 1 Then
                                For I1 = 0 To 21
                                    If pitchRating(idx%, I1, 35) > 0 Then pitchRating(idx%, I1, 35) = pitchRating(idx%, I1, 35) - 1
                                Next
                            End If

                        Case "X":
                            'Activate all pitchers
                            For I3 = 0 To 21
                                If pitchRating(idx%, I3, 15) = 99 And pitchRating(idx%, I3, 34) = 0 Then pitchRating(idx%, I3, 15) = 0: Locate I3 + 2, 3: Print pitchers$(idx%, I3)
                            Next I3

                        Case Else:
                            If I1 <= 21 Then
                                'If a pitcher was selected, disable them
                                If pitchRating(idx%, I1, 15) = 0 Then
                                    pitchRating(idx%, I1, 15) = 99: Locate I1 + 2, 3: Color 0, 7: Print pitchers$(idx%, I1): Color 15, 0
                                Else
                                    If pitchRating(idx%, I1, 15) <> 0 Then pitchRating(idx%, I1, 15) = 0: Locate I1 + 2, 3: Color 15, 0: Print pitchers$(idx%, I1): Color 15, 0
                                End If
                            End If

                    End Select

                End If

            Wend

        Loop Until UCase$(I$) = "Z" Or I$ = Chr$(27)

        'cancelPitchers% = 0
    Else
        cancelPitchers% = 1
    End If 'Done checking for ESC from rotation choice

    If I$ <> Chr$(27) Then
        'We pressed Z
        'Reset the value
        Inotloop% = 0

    Else
        'We cancelled
        cancelPitchers% = 1
    End If 'Done checking for ESC

End Sub


'----------------------------------------
'      PrintPitcherInfo Subroutine
'----------------------------------------
'This routine outputs the information for
'the target pitcher to the display.
Sub PrintPitcherInfo (idx%)

    '1520
    Cls
    Print "#";: Color teamRat_GAME(idx%, 11), teamRat_GAME(idx%, 12): Print gameTeams$(idx%);
    Color 14, 0: Locate , 16
    Print "T  IP   H  G  GS DR  BB  SO   W- L   ERA"

    For I = 0 To 21

        If pitchers$(idx%, I) = "XXX" Or Left$(pitchers$(idx%, I), 1) = " " Or pitchers$(idx%, I) = "" Then pitchRating(idx%, I, 15) = 98

        If pitchRating(idx%, I, 15) <> 1 And pitchers$(idx%, I) <> "XXX" Then
            Color 2, 0

            If I <= 9 Then
                Print Using "#"; I;: Color 15, 0
            Else
                'I > 9
                Print alpha$(I - 10);: Color 15, 0
            End If

            If pitchRating(idx%, I, 15) >= 98 Then Color 0, 7

            Locate , 3: Print pitchers$(idx%, I);
            Color 15, 0

            Locate , 16: Print handed$(pitchRating(idx%, I, 0) + 2);
            '                          IP  H   G   GS DR BB  SO
            Locate , 17: Print Using " ### ### ### ## ## ### ###"; _
                pitchRating(idx%, I, SS_P_IP); pitchRating(idx%, I, SS_P_H); pitchRating(idx%, I, SS_P_G); pitchRating(idx%, I, SS_P_GS); pitchRating(idx%, I, SS_P_Rest); pitchRating(idx%, I, SS_P_BB); pitchRating(idx%, I, SS_P_K);
            '                           W                                                       L  ERA
            Locate , 43: Print Using "  ##"; pitchRating(idx%, I, SS_P_W); : Print "-"; : Print Using "## ##.##"; pitchRating(idx%, I, SS_P_L); pitchRating(idx%, I, SS_P_ERA100) / 100
        End If

    Next I

    If Inotloop% <= 0 Then

        Call PitchingStarter(idx%)
        'If Inotloop% <= 5 And gameScore(P9, 0) - gameScore(1 - P9, 0) > 0 Then P2%(P9) = currPitcher%(P9)

    End If

End Sub


'----------------------------------------
'        PitchingStarter routine
'----------------------------------------
'This subroutine works with the user to
'choose a starting pitcher. The choices
'will have already been printed to the
'screen from other routines.
Sub PitchingStarter (idx%)

    Shared startPitcher()

    Do

        Locate 19, 58: Print "SELECT STARTER"

        If startPitcher(idx%) <> -1 Then

            I1 = startPitcher(idx%)

        Else

            Do
                I$ = GetKeyPress$
                Call LetterToNumber(I$, I1)
            Loop Until I1 <= 21 And pitchRating(idx%, I1, 15) <= 0

        End If

        Locate 20, 58

        Print pitchers$(idx%, I1); " "; handed$(pitchRating(idx%, I1, 0) + 2)

        Print

        If Not (autoPlay And (playerMode = PLAY_MODE_PCvsPC Or playerMode = PLAY_MODE_COMPUTER And compTeam = idx%)) Then

            Do
                Locate 21, 58: Print "ANY CHANGE? (Y/N)"
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then
                Locate 20, 58
                Print Space$(16)
                startPitcher(idx%) = -1
            End If

        Else

            I$ = "N"

        End If

    Loop Until UCase$(I$) = "N"

    currPitcher%(idx%) = I1
    gamePitcher%(idx%) = 1
    pitchRating(idx%, I1, 15) = gamePitcher%(idx%)

    pitchEff(idx%, 0) = Int(((pitchRating(idx%, currPitcher%(idx%), SS_P_BB) + pitchRating(idx%, currPitcher%(idx%), SS_P_H)) / pitchRating(idx%, currPitcher%(idx%), SS_P_G)) + .5)
    pitchEff(idx%, 1) = 3

    If gamePitcher%(idx%) > 1 Then pitchEff(idx%, 1) = 0

End Sub

