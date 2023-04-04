$Debug

Const GAME_VERSION$ = "6.65"
'Const MAX_CONFERENCES = 40
'Const TEAMS_PER_CONFERENCE = 30
'Const TEAMS_PER_DIV = 15
'Const DIVS_PER_CONFERENCE = 2

Const NUM_STATRECORDS = 162
Const MAX_TEAMS = 500
Const MAX_GAMES = 1500
Const MAX_FILES = 1024 '1048576

DefInt A-Z

'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'PadRight.BI'
'$INCLUDE: 'Game_Routines.BI'
'$INCLUDE: 'Game_Routines_Gfx.BI'
'$INCLUDE: 'SOURCE.BI'
'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BI'
'INCLUDE: 'Variables.BI'




' Long INT vs. Single
' 4 bytes either way
' https://wiki.qb64.dev/qb64wiki/index.php/Variable_Types

'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared backToMenu, BO%, X%
Dim Shared diskPaths$(0 To 3), Q$(0 To 2056)
Dim Shared R$(999)

Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared DirCount%

Dim Shared DT$, TM$

' File size
Dim Shared Y As Long

Dim H$(NUM_STATRECORDS), L$(NUM_STATRECORDS)
Dim PL$(NUM_STATRECORDS), W$(NUM_STATRECORDS)
Dim H%(NUM_STATRECORDS), I%(NUM_STATRECORDS), V%(NUM_STATRECORDS)

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)

Dim BA$(40), ER$(40), NM$(46)

Dim L%(1 To 13), Z1%(40)
Dim T%(0 To 21)
Dim T1%(0 To 22)

Dim K9&(1)

Dim B$(0 To 23), P$(0 To 21)
Dim X$(12), Z1$(40), Z2$(40)

Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)

'----------------------------------------
'  Used across ACTIVATE.BAS,
'   DRAFT.BAS routines
'----------------------------------------
Dim draftB1$(22), draftP1$(22)

Dim actB%(0 To 22, 0 To 41), actP%(21, 36)
Dim draftB%(22, 79), draftB1%(22, 79)
Dim draftP%(21, 88), draftP1%(21, 88)
Dim L1%(13)


'----------------------------------------
'  Used across CAREER.BAS,
'   CAREDIT.BAS, NEWLDR.BAS routines
'----------------------------------------
Dim BL$(15), careerPL$(15)
Dim BL0$(18, 20), PL0$(80, 20)

Dim careerB$(100), careerP$(80)

Dim BLYR%(15), PLYR%(15)
Dim BRS%(15, 100, 17), PRS%(15, 80, 18)
Dim careerB1%(100), careerB0%(100, 23)
Dim P0%(80, 42), careerP1%(80)

Dim BLL!(15), PLL!(15)
Dim BL1!(18, 20), PL1!(16, 21)

Dim M$, NN$, S$

'----------------------------------------
' Used across CREATE.BAS
'----------------------------------------
Dim D1$, D2$, D3$, D4$

Dim createT%(34)

'----------------------------------------
' Used across LOOKY.BAS,
'   BINPUT, other routines
'----------------------------------------
Dim ERX!, statI2!

'These are integers
Dim CK
Dim EW, EL, HW, HL
Dim LTL, LTW, ORW, ORL
Dim RW, RL, W, L

Dim D0%, D1%, L2%, W2%

Dim BP$(3), BS$(22), PS$(21)

Dim statT!(22), statT1!(22)

Dim BS%(22), DYS%(21)
Dim statB0%(22, 40), statB%(0 To 22)
Dim statP%(0 To 21), statP0%(21, 41)

Dim INJ%(22), PS%(32), SA%(24), SA1%(24), TS%(11), TS1%(11)

Dim SS%(22, 6)
Dim TB%(15), TP%(33)

'----------------------------------------
'   Used across COMPARE.BAS routines
'----------------------------------------
Dim compareB0%(22, 40), compareP0%(21, 41)
Dim TP&(33)

'----------------------------------------
'   Used across LEAGCMPL.BAS routines
'----------------------------------------
Dim BAT%(24), PIT%(23)
Dim cmplZ2$(298), cmplZ3$(298)
Dim cmplZ1!(298, 2)

'----------------------------------------
'   Used across NEWLDR.BAS,
'   NEWLDRST.BAS routines
'----------------------------------------
'NEWLDR

Dim leaderPL$(1 To 880)

'-- corresponds to TP$, TP1$, TP2$
'40x22
Dim TM$(40, 22), TMP$(1 To 880)

Dim TP$(0 To 31), TP1$(0 To 29), TP2$(0 To 39)

'40x23
Dim leaderBL$(1 To 920), TMB$(1 To 920), TMM$(1 To 920), TPP$(1 To 920), TT2$(40, 23), TYY$(1 To 920)

Dim BK%(1 To 920, 22), PM%(1 To 880, 23)

Dim TYP!(1 To 920), TYP1!(1 To 920)
Dim TT!(40, 22), TT1!(40, 23)

Dim GM1!(920)

'NEWLDRST
Dim CLT!(40), CWS!(40), ELL!(40), EWW!(40)
Dim HLL!(40), HWW!(40), LLS!(40), LWS!(40)
Dim LL!(40), LTW!(40), LTL!(40)
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
' Used across PROGMAN.BAS routines
'----------------------------------------
Dim AB%(9), progMG%(1000), progB%(22, 44)
Dim progB3%(0 To 9), progB7%(0 To 9), progB9%(0 To 9)
Dim progM%(0 To 9)

Dim progB1$(4), progC$(10), PT$(1), mgrX$(0 To 21)

Dim yesNo$(1)

Dim mgr_idx%

'----------------------------------------
' Used across POSSRT.BAS routines
'----------------------------------------
Dim SS
Dim PB$(22), TYN$(40)

'----------------------------------------
' Used across SCHEDULE.BAS routines
'----------------------------------------
Dim BS%, NS%
Dim N$
Dim scheduleNG%(MAX_GAMES, 18)

Dim E%(13)
Dim scheduleAP%(1), scheduleL%(13)
Dim scheduleT%(34), scheduleZ1%(40)

Dim MO$(3), TS$(0 To 1)
Dim scheduleH$(1 To 20), scheduleV$(1 To 20)
Dim scheduleYN$(MAX_GAMES, 1)

'----------------------------------------
' Used across SINPUT.BAS routines
'----------------------------------------
'confirm how this is used;
'   already commented out lines
'B0%(22, 22)

Dim HW%(50), AW%(50), HL%(50), AL%(50), R1%(200), R2%(200), R3%(200), R4%(200)
Dim THW%(4), TAW%(4), THL%(4), TAL%(4), TR1%(4), TR2%(4), TR3%(4), TR4%(4)

Dim DV$(4)

'----------------------------------------
' Used across TRADE.BAS routines
'----------------------------------------
Dim A1$(1), SA$(2)
Dim tradeB$(2, 23), tradeB1$(2, 23), tradeP$(2, 22), tradeP1$(2, 22)

Dim tradeD0%(1), D1%(1), L2%(1), TEAM%(1, 22)
Dim tradeB%(2, 23, 79), tradeB0%(2, 22, 22), tradeB3%(2, 23)
Dim tradeL%(1, 14)
Dim tradeP%(2, 22, 88), tradeP0%(2, 22, 42), tradeP3%(2, 22)
Dim tradeSA%(1, 24), tradeSS%(1, 22, 6), tradeT1%(2, 23), tradeTS%(1, 11)
Dim W2%(1)

Dim M$(2), S$(2), NN$(2)

Dim tradeS%(1), tradeInfo%(1)

'----------------------------------------
' Used across Game Routines
'----------------------------------------

' POST GAME
'---------------
Dim DB, HR, TR, SB
Dim Shared DB$(0 To 22), E$(0 To 22), HR$(0 To 22), SB$(0 To 22), TR$(0 To 22)
Dim Shared A$(1), gameB$(0 To 1, 0 To 22), B1$(4), C$(11)


' LIMITED USE
'---------------
Dim BB%(0 To 9), gameT1%(0 To 1), MF%(1)
Dim SP%(1), U%(50), UV%(200)

Dim BP(0 To 9), F(0 To 9)

Dim E!

Dim fileLength&


' Shared / Global
'---------------
Dim Shared Y$, YY$
Dim Shared pbpDelay!, WF!
Dim Shared attendance&, gameTime$
Dim Shared Inotloop% ', pbpIdx%

Dim Shared AP%, CGERA%, DH%, DR%, EJ%
Dim Shared IJ%, IJL%, INNING%, LYN%, NLF%
Dim Shared PB%, PC%, SC%, TA%, TP%, WS%
Dim Shared F$, PARK$, WI$, W$, L$

Dim Shared H6%

Dim Shared usingGfx

Dim Shared D, P, RE, RD, U6, U9

Dim Shared B1!(9), K9!(1)

Dim Shared MON(5, 14), VV(1), VV1(1)

Dim Shared PARK$(99), Q3$(100)

Dim Shared WD$(10)
'-- WD$ was originally DIM'd as 0 to 9
'-- I think it was increased by mistake

Dim Shared EJ$(20), F$(10), G$(10), gameH$(0 To 4), H0$(1), IJ$(20)
'G$ = PBP lines

Dim Shared gameM$(1), N$(1), gameNN$(1), gameP$(1, 21), PC$(4)
Dim Shared player$(23), stadium$(1), SC$(4), temp$(4), U$(4), YN$(1)

'-- Why Q2$ vs the usual Q$ ?
Dim Shared Q2$(2056)


Dim Shared A5%(0 To 4), AP%(2), B%(0 To 1, 0 To 22, 80), gameB0%(0 To 3, 0 To 2)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(0 To 1, 0 To 9), B4%(1, 7, 9)
'B3% --> active batters?

Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(0 To 1, 0 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1, 22, 6), D0%(1), D3%(1), DP%(1), GK%(1), H0%(1)
Dim Shared gameINJ%(0 To 1, 0 To 22)
Dim Shared gameL%(1, 13), L0%(1), LB%(9, 1), M%(9), MG%(0 To 1, 999), NG%(18), O%(1)
Dim Shared P%(0 To 1, 0 To 21, 0 To 89), gameP1%(0 To 1), P2%(1), P3%(1, 21), P4%(22), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41)
Dim Shared PR%(24)
Dim Shared S%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1)
Dim Shared gameSA%(1, 24), SO%(5, 2), SU%(1, 1)
Dim Shared gameT%(22), T3%(22), gameTP%(11, 2), gameTS%(1, 11), VA%(26), X0%(1, 2, 23)

'-- Can these not be local???
Dim Shared FontColAdj%, normalFinish%







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
For I = 0 To 31: Read TP$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED
Data TIME STRUCK OUT,STOLEN BASES,CAUGHT STEALING,ERRORS,CURRENT HITTING STREAK
Data LONGEST HITTING STREAK,GAMES,GROUND INTO DP,PUTOUTS,ASSISTS
Data SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,AT BATS PER HOME RUN
Data AT BATS PER STRIKEOUT,AT BATS PER WALK,AT BATS PER RBI,AT BATS PER RUNS SCORED
Data AT BATS PER DOUBLE PLAY
Data SB ATTEMPTS AGAINST,RUNNERS CAUGHT STEALING,% CAUGHT STEALING
For I = 1 To 32: Read TP0$(I): Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKE OUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data LEADS HELD,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
Data SACRIFICE HITS,SACRIFICE FLIES,RUN SUPPORT,RETURN TO MAIN MENU
For I = 0 To 29: Read TP1$(I): Next

Data INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS
Data STRIKEOUTS,WON,LOST,GAMES,GAMES STARTED
Data COMPLETE GAMES,SHUTOUTS,SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED
Data SACRIFICE FLIES,SACRIFICE HITS
Data RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,RUN SUPPORT,QUALITY STARTS,EARNED RUN AVERAGE
Data WALKS PER 9 IP,STRIKEOUTS PER 9 IP,HITS ALLOWED PER 9 IP,DOUBLE PLAYS PER 9 IP
For I = 1 To 28: Read TP1A$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.,RUNNERS LEFT ON BASE,DOUBLE PLAYS GROUNDED INTO
Data FIELDING AVERAGE,INNINGS PITCHED,HITS,RUNS,EARNED RUNS,WALKS,STRIKE OUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLE PLAYS,HOME RUNS ALLOWED,RUNNERS INHERITED,RUNNNERS INH. SCORED,% OF INH. RUNNERS SCORING
Data BLOWN SAVES,QUALITY STARTS,EARNED RUN AVERAGE,SACRIFICE HITS,SACRIFICE FLIES,RETURN TO MAIN MENU
For I = 0 To 39: Read TP2$(I): Next

Data AT BATS,HITS,RUNS,RUNS BATTED IN,DOUBLES,TRIPLES,HOME RUNS,TIMES WALKED,TIME STRUCK OUT
Data STOLEN BASES,CAUGHT STEALING,ERRORS,LEFT ON BASE,DOUBLE PLAYS GR. INTO
Data PUTOUTS,ASSISTS,SACRIFICE HITS,SACRIFICE FLIES
Data BATTING AVERAGE,SLUGGING PCT.,ON-BASE PCT.
For I = 0 To 20: Read TP2A$(I): Next

Data INNINGS PITCHED,HITS ALLOWED,RUNS ALLOWED,EARNED RUNS,WALKS,STRIKEOUTS,COMPLETE GAMES,SHUTOUTS
Data SAVES,DOUBLEPLAYS TURNED,HOME RUNS ALLOWED,SACRIFICE HITS,SACRIFICE FLIES,RUNERS INHERITED
Data RUNNERS INHERITED SCORED,BLOWN SAVES,QUALITY STARTS,PER CENT OF RI SCORED,EARNED RUN AVERAGE
For I = 0 To 18: Read TP3$(I): Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M
Data 681,663,645,627,609,591,573,555,537
For I = 0 To 12: Read mgrX$(I): Next
For I = 1 To 9: Read AB%(I): Next

Data LP,L,S,R,RP
Data dh,"p ","c ",1b,2b,3b,ss,lf,cf,rf,ph
For I = 0 To 4: Read B1$(I): Next
For I = 0 To 10: Read C$(I): Next I

Data "OUTDOOR","INDOOR ","GRASS","TURF"
For I = 0 To 3: Read BP$(I): Next

Data HH,C1,C2,CC
For X = 0 To 3: Read MO$(X): Next

Data 15,0,15,1,19,4,4,6,3,1,2,3,14
For X = 1 To 13: Read E%(X): Next

Data A,B,C,D,E,F,G,H,I,J,K,L,M,dh," p"," c",1b,2b,3b,ss,lf,cf,rf,ph,pr
For I = 0 To 12: Read X$(I): Next I
For I = 0 To 11: Read C$(I): Next I

'Data to read in to variables
Data first,second,third,short
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
For I = 3 To 6
    Read F$(I)
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

'----------------------------------------
'               MAIN CODE
'----------------------------------------
_Title "Full Count Baseball - Menu"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

If Not _FileExists("DEV.BAS") Then
    Open "DEV.BAS" For Output As #1
    For I = 0 To 3
        Print #1, _CWD$ + "\"
    Next
    Close 1
End If

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, diskPaths$(I)
Next
Close 1


Selection% = 1

Do
    Call Main_Menu(Selection%)
Loop Until Selection% = 15

End

System


Errhandler:
Open "errlog" For Append As #9
Print #9, "Error occurred! " '; Date$; " "; Time$
Print #9, "Error #"; Err; "on program file line"; _ErrorLine
Print #9, A$(0) + " vs " + A$(1)
Print #9,
Close #9
Resume Next ' moves program to code following the error.


'----------------------------------------
'       Main_Menu Subroutine
'----------------------------------------
Sub Main_Menu (menuSelection%)

    ReDim menuChoices$(1 To 15)

    Do
        Cls
        Color 15, 0

        Call TITLE
        ReDim menuChoices$(1 To 15)
        menuChoices$(1) = "PLAY A GAME                     "
        menuChoices$(2) = "ENTER NEW/CHANGE EXISTING TEAMS "
        menuChoices$(3) = "PROGRAMMABLE MANAGER            "
        menuChoices$(4) = "AUTO SCHEDULER/BOXSCORE OPTIONS "
        menuChoices$(5) = "DRAFT A TEAM                    "
        menuChoices$(6) = "TRADE PLAYERS                   "
        menuChoices$(7) = "VIEW/PRINT COMPILED STATS       "
        menuChoices$(8) = "MODIFY REPLAY STATS             "
        menuChoices$(9) = "LEAGUE LEADERS AND STANDINGS    "
        menuChoices$(10) = "LEAGUE UTILITIES                "
        menuChoices$(11) = "ACTIVATE/DEACTIVATE PLAYERS     "
        menuChoices$(12) = "GROUNDBALL/FLYOUT CALCS         "
        menuChoices$(13) = "HARD DRIVE INSTALLATION         "
        menuChoices$(14) = "CONFIGURE DRIVES FOR TEAMS,STATS"
        menuChoices$(15) = "EXIT TO DOS                     "
        Selection% = 1
        Count% = 15
        Start% = 1
        ExitCode% = 15
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 7
        Column% = 25

        Call box0(6, 24, 22, 57, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call LOADER
                Case 2: Call BINPUT
                Case 3: Call PROGMAN
                Case 4: Call SCHEDULE
                Case 5: Call DRAFT
                Case 6: Call TRADE
                Case 7: Call LOOKY
                Case 8: Call SINPUT
                Case 9: Call Leaders_Menu
                Case 10: Call Replay_Menu
                Case 11: Call ACTIVATE
                Case 12: Call GROUND
                    'Case 13: Call HDCOPY
                Case 14: Call ConfigureDrives
                Case 15: Cls
            End Select
        End If

    Loop Until Selection% = 15 Or ExitCode% = 27

    menuSelection% = Selection%
    If ExitCode% = 27 Then menuSelection% = 15

End Sub

'----------------------------------------
'       Configure_Drives Subroutine
'----------------------------------------
'...explanation...
Sub ConfigureDrives ()

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
    Print
    Locate , 31: Print "A:"
    Locate , 31: Print "C:\FULL\TEAMS"

    Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(0), 30), FG, BG, -1
    Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
    Locate , 16: QPrint PadRight$(diskPaths$(1), 30), FG, BG, -1
    Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
    Locate , 18: QPrint PadRight$(diskPaths$(2), 30), FG, BG, -1
    Locate 13, 1: QPrint "Path for Manager Profiles ", 14, 1, -1
    Locate , 27: QPrint PadRight$(diskPaths$(3), 30), FG, BG, -1

    GetDiv1:
    Locate 7, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(0) = _Trim$(diskPaths$(0))

    GetDiv2:
    Locate 9, 16 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(1) = _Trim$(diskPaths$(1))

    GetDiv3:
    Locate 11, 18 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
    If XCode = 2 GoTo Done 'all done entering data
    diskPaths$(2) = _Trim$(diskPaths$(2))

    GetDiv4:
    Locate 13, 27 'location of beginning of field
    CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
    Call TextIn(diskPaths$(3), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
    If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
    diskPaths$(3) = _Trim$(diskPaths$(3))

    Done:
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
        diskPaths$(R) = diskPaths$(R) + "\"
        Print #1, diskPaths$(R)
    Next R

    Close #1

    '620 GoTo 1
End Sub

'----------------------------------------
'       Leaders_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Leaders_Menu ()

    Do

        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 8)
        menuChoices$(1) = "CREATE/EDIT A LEAGUE                 "
        menuChoices$(2) = "UPDATE STANDINGS/LEADERS FOR A LEAGUE"
        menuChoices$(3) = "EXPANDED LEADERS AND STANDINGS       "
        menuChoices$(4) = "VIEW RESULTS FOR A LEAGUE            "
        menuChoices$(5) = "PRINT RESULTS FOR A LEAGUE           "
        menuChoices$(6) = "DELETE ALL STATS FOR A LEAGUE        "
        menuChoices$(7) = "RETURN TO FULL COUNT MAIN MENU       "
        menuChoices$(8) = "EXIT TO DOS                          "
        Selection% = 1
        Count% = 8
        Start% = 1
        ExitCode% = 8
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 23
        Call box0(7, 22, 16, 60, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call CREATE
                Case 2: Call LEAGCMPL
                Case 3: Call NEWLDR
                Case 4: Call SEELEAG
                Case 5: Call PRTLEAG
                Case 6: Call BLANK
                Case 7: Cls
            End Select
        End If

    Loop Until Selection% >= 7 Or ExitCode% = 27

    If Selection% = 8 Then End

End Sub

'----------------------------------------
'       Replay_Menu Subroutine
'----------------------------------------
'...explanation...
Sub Replay_Menu ()

    Do
        Cls

        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 6)
        menuChoices$(1) = "COMPARE COMPILED STATS VS. ACTUAL"
        menuChoices$(2) = "MERGE TWO STAT FILES             "
        menuChoices$(3) = "VIEW TEAM HEAD TO HEAD RESULTS   "
        menuChoices$(4) = "CAREER FILE OPTIONS              "
        menuChoices$(5) = "SORT PLAYERS BY POSITION         "
        menuChoices$(6) = "RETURN TO FULL COUNT MAIN MENU   "
        Selection% = 1
        Count% = 6
        Start% = 1
        ExitCode% = 6
        FileFGColor% = 15: FileBGColor% = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 1
        Row% = 8
        Column% = 23
        Call box0(7, 22, 14, 56, 2, 15, 1)
        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), Selection%, Start%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0
        If ExitCode% <> 27 Then
            Select Case Selection%
                Case 1: Call COMPARE
                Case 2: Call MERGE
                Case 3: Call REC
                Case 4: Call CAREER
                Case 5: Call POSSRT
                Case 6: Cls
            End Select
        End If

    Loop Until Selection% = 6 Or ExitCode% = 27

End Sub


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
'$INCLUDE: 'PadRight.BM'

'$INCLUDE: 'LOADER.BAS'
'$INCLUDE: 'Game_Routines.BM'
'$INCLUDE: 'Game_Routines_Gfx.BM'
'$INCLUDE: 'SOURCE.BM'

'$INCLUDE: 'MainRoutines.BI'
'$INCLUDE: 'TITLE.BM'
'$INCLUDE: 'ACTIVATE.BAS'
'$INCLUDE: 'ALIGN.BAS'
'$INCLUDE: 'BINPUT.BAS'
'$INCLUDE: 'BLANK.BAS'
'$INCLUDE: 'CAREDIT.BAS'
'$INCLUDE: 'CARVIEW.BAS'
'$INCLUDE: 'CAREER.BAS'
'$INCLUDE: 'CARLDR.BAS'
'$INCLUDE: 'COMPARE.BAS'
'$INCLUDE: 'CREATE.BAS'
'$INCLUDE: 'DRAFT.BAS'
'$INCLUDE: 'GROUND.BAS'
'$INCLUDE: 'LEAGCMPL.BAS'
'$INCLUDE: 'LOOKY.BAS'
'$INCLUDE: 'MERGE.BAS'
'$INCLUDE: 'NEWLDR.BAS'
'$INCLUDE: 'NEWLDRRC.BAS'
'$INCLUDE: 'NEWLDRST.BAS'
'$INCLUDE: 'POSSRT.BAS'
'$INCLUDE: 'POSCR.BAS'
'$INCLUDE: 'PROGMAN.BAS'
'$INCLUDE: 'PRTLEAG.BAS'
'$INCLUDE: 'REC.BAS'
'$INCLUDE: 'SCHEDULE.BAS'
'$INCLUDE: 'SEELEAG.BAS'
'$INCLUDE: 'SINPUT.BAS'
'$INCLUDE: 'TRADE.BAS'
'$INCLUDE: 'UMPMGR.BAS'

