
' Long INT vs. Single
' 4 bytes either way
' https://wiki.qb64.dev/qb64wiki/index.php/Variable_Types

'----------------------------------------
' Used across more than one source file
'----------------------------------------
Dim Shared backToMenu, BO%, X%
Dim Shared diskPaths$(0 To 3), Q$(0 To 2056)

'Dim Shared diskPaths$(0 To 3), Q$(0 To 2056)
Dim Shared R$(999)

Dim Shared teamNames$(MAX_TEAMS)
Dim Shared teamIndex%(MAX_TEAMS)
Dim Shared DirCount%

Dim Shared DT$, TM$


' File size
Dim Shared Y As Long

Dim homeTeam$(MAX_GAMES), visitingTeam$(MAX_GAMES)
Dim homeScore%(MAX_GAMES), visitingScore%(MAX_GAMES)

'Dim H$(NUM_STATRECORDS), L$(NUM_STATRECORDS)
'Dim PL$(NUM_STATRECORDS), W$(NUM_STATRECORDS)

'Dim H%(NUM_STATRECORDS), I%(NUM_STATRECORDS), V%(NUM_STATRECORDS)

Dim L%(1 To 13), Z1%(40)
Dim T%(0 To 21)
Dim T1%(0 To 22)

Dim K9&(1)

Dim B$(0 To 23), P$(0 To 21)
Dim X$(12), Z1$(40), Z2$(40)

'----------------------------------------
'  Used across ACTIVATE.BAS,
'   DRAFT.BAS routines
'----------------------------------------
Dim draftB1$(22), draftP1$(22)

Dim B%(0 To 22, 0 To 41), L1%(13), P%(21, 36)
Dim draftB%(22, 79), draftB1%(22, 79)
Dim draftP%(21, 88), draftP1%(21, 88)

'----------------------------------------
'  Used across CAREER.BAS,
'   CAREDIT.BAS, NEWLDR.BAS routines
'----------------------------------------
Dim BL$(15), careerPL$(15)
Dim BL0$(18, 20), PL0$(80, 20)

Dim careerB$(100), careerP$(80)

Dim BLYR%(15), PLYR%(15)
Dim BRS%(15, 100, 17), PRS%(15, 80, 18)
Dim B1%(100), B0%(100, 23)
Dim P0%(80, 42), P1%(80)

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
'Dim cmplZ2$(298), cmplZ3$(298)
'Dim BA$(40), ER$(40), NM$(46)
Dim BAT%(24), PIT%(23)
'Dim cmplZ1!(298, 2)
'Dim DFT!(40, 20), JS!(46, 42), OFT!(40, 20)

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
Dim AB%(9), MG%(1000), progB%(22, 44)
Dim B3%(0 To 9), B7%(0 To 9), B9%(0 To 9)
Dim M%(0 To 9)

Dim B1$(4), C$(10), PT$(1), mgrX$(0 To 21)

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
Dim NG%(MAX_GAMES, 18)

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
Dim A$(1), A1$(1), SA$(2)
Dim tradeB$(2, 23), tradeB1$(2, 23), tradeP$(2, 22), tradeP1$(2, 22)

Dim D0%(1), D1%(1), L2%(1), TEAM%(1, 22)
Dim tradeB%(2, 23, 79), tradeB0%(2, 22, 22), tradeB3%(2, 23)
Dim tradeL%(1, 14)
Dim tradeP%(2, 22, 88), tradeP0%(2, 22, 42), tradeP3%(2, 22)
Dim tradeSA%(1, 24), tradeSS%(1, 22, 6), tradeT1%(2, 23), tradeTS%(1, 11)
Dim W2%(1)

Dim M$(2), S$(2), NN$(2)

Dim S%(1), tradeInfo%(1)

'----------------------------------------
' Used across Game Routines
'----------------------------------------
