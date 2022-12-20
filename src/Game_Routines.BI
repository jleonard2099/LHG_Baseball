DECLARE FUNCTION OUTFLDLOCATION$ (W%)

Declare Sub PitchingStarter (idx%)
Declare Sub ADDPIT (D, I5, I6)
Declare Sub ADDSTTS (P, D)
Declare Sub ADJRUNNERS (S2%, P)
Declare Sub ADV1BASE (i)
Declare Sub ADV2BASES (i)
Declare Sub ADV3BASES (I3, P, D)
Declare Sub ADVANCEMENT (P, D, S2%, F%, I3)
Declare Sub BATDISP (PG, P9)
Declare Sub BATOFF (P, I5, I6)
Declare Sub BATTER1 (i)
Declare Sub BATTER2 (i)
Declare Sub BATTER3 (I3, P, D)
Declare Sub CAUGHTFLY (i, S2%, D)
Declare Sub CHANGELINEUP (P9, P)
Declare Sub CHARGERUN (D, i)
Declare Sub CHECKSAVE (D, P, S4%)
Declare Sub CLEARBASE (i)
Declare Sub CREDITASSIST (W%, D)
Declare Sub CREDITPUTOUT (I3, FB2P%, AST%, D)
Declare Sub CREDITREGPO (W%, D)
Declare Sub DELAY ()
Declare Sub DELIVERY (D)
Declare Sub DISPFLDG (PG, P9, TG)
Declare Sub DISPRUN (I2, P, X)
Declare Sub DPPBP ()
Declare Sub DPPIVOT (W%, D)
Declare Sub DPRELAY ()
Declare Sub DPSAFE ()
Declare Sub DRAWBOX (J)
Declare Sub EJECTINC (EJ%, X$)
Declare Sub EJECTIONINJURY (u6, P, u9, D, po%, P9, W5, W6)
Declare Sub FLDAVG (D, W%, I3)
Declare Sub DisplayField (BC%, P, U%, D)
Declare Sub FLDERR (F%, W%, D, I3, SB%, S2%, P)
Declare Sub GETFIELDER (W%, P, I1, D)
Declare Sub GETFIELDNAME (W%, D)
Declare Sub GETINFIELDER (W%, P, I1, D, S2%, P2)
Declare Sub GETOUTFIELDER (W%, P, I1, D)
Declare Sub GROUNDERPBP (W%, D)
Declare Sub GROUNDERS (W%, D)
Declare Sub HITS2GAP (W%, P, I1, D)
Declare Sub HOLDSTATUS (H0%)
Declare Sub INCBATDF (D, I5, I6)
Declare Sub INCBATOFF (P, I5, I6)
Declare Sub INCPITOFF (P, I5, I6)
Declare Sub INCREMENTHIT (P, D, MO)
Declare Sub INJPIT (P, D, IP%)
Declare Sub INJURYLENGTH (IJL%, D, po%, u6, P, u9, P9, W5, W6)
Declare Sub INNDISP (currInning, teamLine)
Declare Sub LDRIVE ()
Declare Sub LONGFLY (D, W%)
Declare Sub MAINSCREEN (D, P, BC%, U%, MO)
Declare Sub WINDEX ()
Declare Sub NEWPITCHER
Declare Sub NOJUMP (D)
Declare Sub OFFPIT (P9, I5, I6)
Declare Sub OUTATFIRST (W%, D, fbu%, FB2P%)
Declare Sub OUTFIELDERR ()
Declare Sub OUTFLDNAME (W%, D)
Declare Sub PICKEDOFF (D, i)
Declare Sub PINCHRUNNER (MS%, A1, S4%, I2, P, PR%, BC%, U%, D, MO)
Declare Sub PITCHERNAME (W%, D)
Declare Sub PITCHOUT (S4%, TGTBASE%, Q0)
Declare Sub PITDISP (P9)
Declare Sub PITOFF (P, I5, I6)
Declare Sub PITPLACEMENT (P, X$)
Declare Sub PITSTAM (P9)
Declare Sub PITSTRAT ()
Declare Sub PITSWAP (i)
Declare Sub PLAYERNAME (P)
Declare Sub PLYRHRT (P, P9, W5, W6, BC%, U%, D, MO)
Declare Sub PINCHITTER (P, P9, W5, W6)
Declare Sub POPUP ()
Declare Sub POPUPBAT (WK, M)
Declare Sub POPUPPIT (WK, M)
Declare Sub POPUPSTATS ()
Declare Sub PUTOUT (i, D, P)
Declare Sub d100 (randNumVar)
Declare Sub RecordOut (i, I3)
Declare Sub RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
Declare Sub OPTIONS ()
Declare Sub CONVNAME (QQ, I)
Declare Sub DISPBAVG (D)
Declare Sub GETSTATS ()
Declare Sub HITTINGLINES '(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
Declare Sub LRADJ (A!, A1!)
Declare Sub NORMALIZE (I7!, I2!, I6!, K8!)
Declare Sub BASEONBALLS (S2%, D2, D, P)
Declare Sub HITBYPITCH (S2%, D)
Declare Sub WILDPITCH (S2%, D)
Declare Sub RELIEVERS ()
Declare Sub REPLACEMENTS (I7%, I8%, i9%)
Declare Sub SCOREBOARD ()
Declare Sub SECOND2FIRST (D)
Declare Sub SINGLEPBP (S2%, D, W%)
Declare Sub SINGLEROUTINE (W%, P, I1, D, S2%, P2)
Declare Sub SOMESTATS (V, P, F%, D)
Declare Sub SQUEEZECHOICE (SH)
Declare Sub STEAL3RD (TGTBASE%, AA, Q0, H0%, D)
Declare Sub STEALCHANCE (AA, I2, TGTBASE%, CHANCE%)
Declare Sub STRIKEOUT (U%, D)
Declare Sub STRIKEOUTCS (SO)
Declare Sub TAGADVANCE (S2%, I8!, D)
Declare Sub THROW2FIRST (D)
Declare Sub TIRING (P9)
Declare Sub TRIPLEPLAY (D)
Declare Sub TRYFOREXTRABASE (LN%, S2%, P2, Z2%, D, W%, sah%, SB%, I8!, I9)
Declare Sub VIEWTEAM ()
Declare Sub WALLDOUBLE (HW%, W%)
Declare Sub WHICHRUNNER (i, S4%, D5)
Declare Sub WHOSON1ST (S4%)
Declare Sub FOULBALL ()
Declare Sub RAREPLAYS (H6%, H7%, IP%)
Declare Sub WEATHER ()
Declare Sub COLCHK (P9, I)
Declare Sub DRAWBOXA ()
Declare Sub GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
Declare Sub INFO ()
Declare Sub INITVARI ()
Declare Sub LetterToNumber (I$)
Declare Sub LINSWAP (P9)
Declare Sub NEWLINES (P9)
Declare Sub POSITION (I)
Declare Sub POSSWAP (P9)
Declare Sub SHOWBATTERS (P9)
Declare Sub SHOWFIELD (P9)
Declare Sub SHOWSPLIT (P9)
Declare Sub POSTGAME ()
Declare Sub LocateInning (I2)
Declare Sub BoxToScreen (BK$)
Declare Sub BoxToFile (NM$, BK$, E%(), SPECIAL)
Declare Sub CheckRecords ()
Declare Sub InningPrint (I2)
Declare Sub GETRPBP (pbpIdx%, Y$, D)
Declare Sub STRIPRPBP (Y$, pbpIdx%, D)
Declare Sub LDRNRCTDN ()
Declare Sub PBP (F$)
Declare Sub EndOfGame ()

DefInt A-Z

'-------------------------
' Variable declarations
'-------------------------

'Used by postgame routines
Dim DB, HR, TR, SB
'Dim DB$(0 To 22), E$(0 To 22), HR$(0 To 22), SB$(0 To 22), TR$(0 To 22)
Dim Shared DB$(0 To 22), E$(0 To 22), HR$(0 To 22), SB$(0 To 22), TR$(0 To 22)
Dim Shared A$(1), B$(0 To 1, 0 To 22), B1$(4), C$(11), diskPaths$(3)

' Restricted
'---------------
Dim BP(0 To 9), F(0 To 9)

Dim DH$(1), DL$(1), LGT$(2)
Dim PT$(1), PU$(8), U6$(2), UMP$(50)
Dim VI$(1), WE$(1)

Dim BB%(0 To 9), MF%(1)

'NB% doesn't apppear to be used
Dim NB%(1, 4), SP%(1)

Dim T1%(0 To 1), U%(50), UV%(200)

ReDim Q2$(2056)

Dim E!
Dim Y As Long

' Not used at all!?!?!?
'Dim BAT$(23), BM$(10), BMT$(10), DN$(1)
'Dim MN$(10), PIT$(22), PM$(6), PMT$(6)
'Dim BAT%(23), PIT%(22), UM%(1, 9, 4)

' Shared / Global
'---------------
Dim Shared Y$, YY$
Dim Shared B1!(9), K9!(1)
Dim Shared pbpDelay!, WF!
Dim Shared attendance&
Dim Shared INotLoopCounter% ', pbpIdx%

Dim Shared AP%, CGERA%, DH%, DR%, EJ%, IJ%, IJL%, INNING%
Dim Shared NLF%, PB%, PC%, SC%, TA%, TP%, W%, WS%
Dim Shared F$, PARK$, S$, WI$
Dim Shared D, P, RE, RD, U6, U9
Dim Shared usingGfx

Dim Shared MON(5, 14), VV(1), VV1(1)

Dim Shared PARK$(99), Q3$(100), WD$(10)
'--> should WD$ have been increased??? Probably not, just DIM'd as 0 to 9

'Dim Shared A$(1), B$(1, 22), B1$(4), C$(11), diskPaths$(3)
'G$ = PBP lines
Dim Shared EJ$(20), F$(10), G$(10), H$(0 To 4), H0$(1), IJ$(20)
Dim Shared M$(1), N$(1), NN$(1), P$(1, 21), PC$(4)
Dim Shared Q$(23), S$(1), SC$(4), TM$(4), U$(4), X$(12), YN$(1)
Dim Shared teamNames$(500)

Dim Shared A5%(0 To 4), AP%(2), B%(0 To 1, 0 To 22, 80), B0%(0 To 3, 0 To 2)
Dim Shared B1%(3, 1), B2%(1, 22), B3%(0 To 1, 0 To 9), B4%(1, 7, 9)
Dim Shared B5%(0 To 1, 0 To 22, 0 To 21), B7%(0 To 1, 0 To 9), B8%(0 To 1, 0 To 22, 0 To 21), B9%(1, 10), BT%(1, 9, 9)
Dim Shared CF%(9, 2), CSS%(1, 22, 6), D0%(1), D3%(1), DP%(1), GK%(1), H0%(1)
Dim Shared INJ%(0 To 1, 0 To 22)
Dim Shared L%(1, 13), L0%(1), LB%(9, 1), M%(9), MG%(0 To 1, 999), NG%(18), O%(1)
Dim Shared P%(0 To 1, 0 To 21, 0 To 89), P1%(0 To 1), P2%(1), P3%(1, 21), P4%(22), P5%(1, 21, 41), P6%(1), P8%(1, 21, 41), PR%(24)
Dim Shared S%(1, 26), S1%(1), S6%(1, 2), S8%(1, 1), SA%(1, 24), SO%(5, 2), SU%(1, 1)
Dim Shared T%(22), T3%(22), TP%(11, 2), TS%(1, 11), VA%(26), X0%(1, 2, 23)
Dim Shared normalFinish%

Dim Shared FontColAdj%
