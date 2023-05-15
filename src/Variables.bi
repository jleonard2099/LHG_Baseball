'----------------------------------------
' Used mostly for HELLO
'----------------------------------------
Dim opSysType$

Dim statH$(NUM_STATRECORDS), statHR$(NUM_STATRECORDS)

Dim L$(NUM_STATRECORDS), LR$(NUM_STATRECORDS)
Dim PL$(NUM_STATRECORDS), PLR$(NUM_STATRECORDS)
Dim W$(NUM_STATRECORDS), WR$(NUM_STATRECORDS)

Dim H%(NUM_STATRECORDS), I%(NUM_STATRECORDS), V%(NUM_STATRECORDS)
Dim HR%(NUM_STATRECORDS), IR%(NUM_STATRECORDS), VR%(NUM_STATRECORDS)


'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared backToMenu, BO%, X%
Dim Shared diskPaths$(0 To 3), Q$(0 To 2056), Q2$(0 To 2056)
Dim Shared R$(999)

Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared DirCount%

Dim Shared DT$, TM$


' File size
Dim Shared Y As Long

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)

Dim ratings%(1 To 13), Z1%(40)
Dim statT%(0 To 21)
Dim statT1%(0 To 22)

Dim K9&(1)

Dim statB$(0 To 22), statP$(0 To 21)
Dim Z1$(40), Z2$(40)
'Dim X$(12)


'----------------------------------------
'  Used across ALIGN, MERGE routines
'----------------------------------------
Dim Ycurr%, Yroad%
Dim LR2%, WR2%

Dim BN$(23), PN$(22)
Dim BR$(23), PR$(22)

Dim draftB0%(23, 22), draftPR%(22)

Dim B0N%(23, 22), BN%(23)
Dim P0N%(22, 42), PN%(22)
Dim S0N%(22, 6)

Dim alignB0%(22, 22), B0R%(23, 22), BR%(23)
Dim mergeB%(0 To 22)
Dim mergeP%(0 To 21), P0R%(22, 42)
Dim S0R%(22, 6), S0%(22, 6), SR%(22, 6)

Dim draftTR(22), draftTR1(23)

'----------------------------------------
'  Used across ACTIVATE,
'   DRAFT routines
'----------------------------------------
Dim draftB1$(22), draftP1$(22)

Dim L1%(13)
Dim draftB%(0 To 22, 0 To 79), draftB1%(22, 79)
Dim draftP%(21, 88), draftP1%(21, 88)

Dim Manager$, teamAbbrev$, Stadium$

'----------------------------------------
'  Used across CAREER,
'   CAREDIT, NEWLDR routines
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


'----------------------------------------
' Used across CREATE
'----------------------------------------
Dim D1$, D2$, D3$, D4$

Dim createT%(34)

'----------------------------------------
' Used across LOOKY,
'   BINPUT, other routines
'----------------------------------------
Dim ERX!, statI2!

'These are integers
Dim CK
Dim EW, EL, HW, HL
Dim LTL, LTW, ORW, ORL
Dim RW, RL, Wins, L

Dim D0%, D1%, L2%, W2%

Dim BP$(3), BS$(22), PS$(21)

Dim statT!(22), statT1!(22)

Dim BS%(22), DYS%(21)
Dim statB0%(22, 40), statB%(23)
Dim statP%(0 To 21), statP0%(22, 41)

Dim inputINJ%(22), PS%(32), inputSA%(24), SA1%(24), inputTS%(11), TS1%(11)

Dim SS%(22, 6)
Dim TB%(15), inputTP%(33)

'----------------------------------------
'   Used across COMPARE routines
'----------------------------------------
Dim compareB0%(22, 40), compareP0%(21, 41)
Dim TP&(33)

'----------------------------------------
'   Used across LEAGCMPL routines
'----------------------------------------
Dim BA$(40), ER$(40)
Dim cmplZ2$(298), cmplZ3$(298)
Dim NM$(46)

Dim BAT%(24), PIT%(23)

Dim cmplZ1!(298, 2)
Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)


'----------------------------------------
'   Used across NEWLDR,
'   NEWLDRST routines
'----------------------------------------
Dim leaderPL$(1 To 880)

'-- corresponds to TP$, TP1$, TP2$
Dim TM$(40, 22), TMP$(1 To 880)

Dim TP$(0 To 31), TP1$(0 To 29), TP2$(0 To 39)

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
' Used across PROGMAN routines
'----------------------------------------
Dim AB%(9)
Dim progB%(22, 44), progB3%(0 To 9), progB7%(0 To 9), progB9%(0 To 9)
Dim progM%(0 To 9), progMG%(1000)

Dim progB1$(4), progC$(10), progPT$(1), mgrX$(0 To 21)

Dim yesNo$(1)

Dim mgr_idx%

'----------------------------------------
' Used across POSSRT routines
'----------------------------------------
Dim SS
Dim PB$(22), TYN$(40)

'----------------------------------------
' Used across SCHEDULE routines
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
' Used across SINPUT routines
'----------------------------------------
Dim HW%(50), AW%(50), HL%(50), AL%(50), R1%(200), R2%(200), R3%(200), R4%(200)
Dim THW%(4), TAW%(4), THL%(4), TAL%(4), TR1%(4), TR2%(4), TR3%(4), TR4%(4)

Dim DV$(4)

'----------------------------------------
' Used across TRADE routines
'----------------------------------------
Dim tradeA$(1), A1$(1), SA$(2)
Dim tradeB$(2, 23), tradeB1$(2, 23), tradeP$(2, 22), tradeP1$(2, 22)

Dim tradeD0%(1), D1%(1), L2%(1), TEAM%(1, 22)
Dim tradeB%(2, 23, 79), tradeB0%(2, 22, 22), tradeB3%(2, 23)
Dim tradeInfo%(1), tradeL%(1, 14)
Dim tradeP%(2, 22, 88), tradeP0%(2, 22, 42), tradeP3%(2, 22)
Dim tradeS%(1), tradeSA%(1, 24), tradeSS%(1, 22, 6), tradeT1%(2, 23), tradeTS%(1, 11)
Dim W2%(1)

Dim Managers$(2), Stadiums$(2), teamAbbrevs$(2)


'----------------------------------------
' Used across Game Routines
'----------------------------------------
'Used by POSTGAME
Dim DB, HR, TR, SB, PPH
Dim BP(0 To 9), F(0 To 9)
Dim BB%(0 To 9), MF%(1), SP%(1), T1%(0 To 1)

Dim PT$(1)

Dim E!


'Used by LOADER
Dim dayNight$(0 To 1), DH$(1), DL$(1)
Dim LGT$(2), month$(1 To 12)
Dim PU$(8), U6$(2), UMP$(50)
Dim VI$(1), WE$(1)

Dim U%(50)

Dim fileLength&


' Shared / Global
Dim Shared Y$, YY$
Dim Shared F$, PARK$, S$, WI$, W$, L$

Dim Shared B1!(9), K9!(1)
Dim Shared pbpDelay!, WF!

Dim Shared attendance&, gameTime$

Dim Shared Inotloop% ', pbpIdx%

Dim Shared AP%, CGERA%, DH%, DR%, EJ%
Dim Shared D, P, RE, RD, U6, U9
Dim Shared IJ%, IJL%, INNING%, LYN%, NLF%
Dim Shared PB%, PC%, SC%, TA%, TP%, W%, WS%
Dim Shared H6%, MON
Dim Shared usingGfx

'These are just for tracing values / troubleshooting
Dim Shared testIdx, dudVal



Dim Shared MON(5, 14), VV(1), VV1(1)

Dim Shared A$(1), B$(0 To 1, 0 To 22), B1$(4), C$(11)
Dim Shared DB$(0 To 22), E$(0 To 22), EJ$(20), F$(10)
Dim Shared G$(10), H$(0 To 4), H0$(1), HR$(0 To 22)
Dim Shared IJ$(20), M$(1), N$(1), NN$(1)
Dim Shared P$(1, 21), PARK$(99), PC$(4), player$(23)
Dim Shared Q3$(100), S$(1), SB$(0 To 22), SC$(4), TR$(0 To 22)
Dim Shared U$(4), WD$(10), X$(12), YN$(1)

Dim Shared A5%(0 To 4), AP%(2), B%(0 To 1, 0 To 22, 80), B0%(0 To 3, 0 To 2)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(0 To 1, 0 To 9), B4%(1, 7, 9)
'B3% --> active batters?

Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(0 To 1, 0 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1, 22, 6), D0%(1), D3%(1), DP%(1), GK%(1), H0%(1)
Dim Shared INJ%(0 To 1, 0 To 22), L%(1, 13), L0%(1), LB%(9, 1)
Dim Shared M%(9), MG%(0 To 1, 999), NG%(18), O%(1)
Dim Shared P%(0 To 1, 0 To 21, 0 To 89), P1%(0 To 1), P2%(1), P3%(1, 21), P4%(22), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41)
Dim Shared PR%(24)

Dim Shared inningScore%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1)
Dim Shared SA%(1, 24), SO%(5, 2), SU%(1, 1)
Dim Shared T%(22), T3%(22), TP%(11, 2), TS%(1, 11), VA%(26), X0%(1, 2, 23)

Dim Shared FontColAdj%, normalFinish%
