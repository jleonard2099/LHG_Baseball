'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
'$INCLUDE: 'COMMON.GW'
'$INCLUDE: 'GWDECL.BAS'

DEFINT A-Z

DECLARE SUB INCBATDF (D, I5, I6)
DECLARE SUB PBP (F$)
DECLARE SUB SOMESTATS (V, p, F%, D)
DECLARE SUB ADDSTTS (p, D)
DECLARE SUB BATOFF (p, I5, I6)
DECLARE SUB INJPIT (p, D, IP%)
DECLARE SUB SECOND2FIRST (D)
DECLARE SUB THROW2FIRST (D)
DECLARE SUB GROUNDERS (W%, D)
DECLARE SUB PITCHERNAME (W%, D)
DECLARE SUB DPRELAY ()
DECLARE SUB INCREMENTHIT (p, D, MO)
DECLARE SUB DPPBP ()
DECLARE SUB STRIKEOUT (U%, D)
DECLARE SUB DELIVERY (D)
DECLARE SUB NEWLINES ()
DECLARE SUB CREDITASSIST (W%, D)
DECLARE SUB CREDITPUTOUT (I3, FB2P%, AST%, D)
DECLARE SUB CREDITREGPO (W%, D)
DECLARE SUB GETFIELDNAME (W%, D)
DECLARE SUB GETOUTFIELDER (W%, p, I1, D)
DECLARE SUB GETFIELDER (W%, p, I1, D)
DECLARE SUB REPLACEMENTS (I7%, I8%, i9%)
DECLARE SUB EJECTIONINJURY (D, po%)
DECLARE SUB INCPITOFF (P, I5, I6)
DECLARE SUB POPUPBAT (WK, M)
DECLARE SUB POPUPSTATS ()
DECLARE SUB POPUPPIT (WK, M)
DECLARE SUB CHECKSAVE (D, p, S4%)
DECLARE SUB POPUP ()
DECLARE SUB INCBATOFF (P, I5, I6)
DECLARE SUB ADDPIT (D, I5, I6)
DECLARE SUB POSTGAME ()
DECLARE SUB BATDISP (PG, P9)
DECLARE SUB DELAY ()
DECLARE SUB PITSTAM (P9)
DECLARE SUB FLDDISP (BC%, p, U%, D)
DECLARE SUB INNDISP (I2, i)
DECLARE SUB FLDERR (F%, W%, D, I3, SB%, S2%, p)
DECLARE SUB OUTFIELDERR ()
DECLARE SUB FLDAVG (D, W%, I3)
DECLARE SUB LDRIVE ()
DECLARE SUB RANDNUM (rn)
DECLARE SUB DISPBAVG (D)
DECLARE SUB DISPRUN (I2, p, X)
DECLARE SUB RecordOut (i, I3)
DECLARE SUB CLEARBASE (i)
DECLARE SUB CHARGERUN (D,i)
DECLARE SUB RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
DECLARE SUB ADV1BASE (i)
DECLARE SUB ADV2BASES (i)
DECLARE SUB ADV3BASES (I3, p, D)
DECLARE SUB ADVANCEMENT (p, D, S2%, F%,I3)
DECLARE SUB HITTINGLINES (l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
DECLARE SUB ADJRUNNERS (S2%, p)
DECLARE SUB BATTER1 (i)
DECLARE SUB BATTER2 (i)
DECLARE SUB BATTER3 (I3, p, D)
DECLARE SUB PITPLACEMENT (p, X$)
DECLARE SUB HOLDSTATUS (H0%)
DECLARE SUB MAINSCREEN (D, p, BC%, U%, MO)
DECLARE SUB BLANKLINE ()

TYPE PBP
	YY AS STRING * 30
END TYPE

DIM K9(1) AS SINGLE
DIM s$(1), A$(1), l%(1, 13), T1(1), b%(1, 22, 80), p%(1, 21, 89), b$(1, 22), p$(1, 21), P8%(1, 21, 41), H$(4)
DIM B8%(1, 22, 21), P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
DIM s%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
DIM VV(1), VV1(1), D3%(1), M%(9), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
DIM X0%(1, 2, 23), D0%(1), P5%(1, 21, 41), B5%(1, 22, 21), TP%(11, 2), L0%(1), T3%(22)
DIM TS%(1,11),SA%(1,24)
DIM MG%(1,999)
DIM CSS%(1,22,6),SU%(1,1)
DIM NG%(18), YN$(1), Q$(23), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
DIM H0%(1), CZ%(1), BP(1), BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2)
DIM PR%(24), GK%(1),AP%(2),VA%(26)

COMMON SHARED TS%(),SA%(),P8%(), B8%(), p%(), p$(), b$(), P5%(), B5%(), X0%(), b%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED s%(), P1%(), S1%(), D$(), S6%(), l%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), H$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED s$, p, u6, DH%, u9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, AP%, INNING%,BUCK AS PBP
COMMON SHARED MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%

SUB ADDPIT (D, I5, I6)
	P8%(D, I5, I6) = P8%(D, I5, I6) + 1
	P5%(D, I5, I6) = P5%(D, I5, I6) + 1
END SUB

SUB ADDSTTS (p, D)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P, I5, I6)
	I5 = P1%(D): I6 = 0: CALL ADDPIT(D, I5, I6)
END SUB

SUB ADJRUNNERS (S2%, p)
	FOR i = 1 TO 3
	IF S2% = 13 AND A5%(i) = 11 AND B1%(i, 0) = 0 THEN A5%(0) = 11
	NEXT i
	IF B1%(3, 0) = 0 OR A5%(3) = 0 THEN 6805
	i = 3
	IF A5%(3) = 10 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(3) >= 1 AND A5%(3) <= 4 THEN CALL CHARGERUN(D,i)
	IF A5%(3) = 11 THEN I3 = 4: CALL CLEARBASE(i)

6805 IF B1%(2, 0) = 0 OR A5%(2) = 0 THEN 6812
	i = 2
	IF A5%(2) = 10 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(2) = 11 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(2) = 12 AND B1%(3, 0) = 0 THEN I3 = 4: CALL CLEARBASE(i)
	IF A5%(2) = 1 AND B1%(3, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(2) >= 2 AND A5%(2) <= 4 AND B1%(3, 0) = 0 THEN CALL CHARGERUN(D,i)

6812 IF B1%(1, 0) = 0 OR A5%(1) = 0 THEN 6821
	i = 1: IF A5%(1) = 8 AND B1%(2, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(1) = 10 THEN I3 = 1: CALL CLEARBASE(i)
	IF A5%(1) = 11 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(1) = 12 AND B1%(3, 0) = 0 AND B1%(2, 0) = 0 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(1) = 13 AND B1%(3, 0) = 0 AND B1%(2, 0) = 0 THEN I3 = 4: CALL CLEARBASE(i)
	IF A5%(1) = 1 AND B1%(2, 0) = 0 THEN CALL BATTER1(i)
	IF A5%(1) = 2 AND B1%(2, 0) = 0 AND B1%(3, 0) = 0 THEN CALL BATTER2(i)
	IF A5%(1) >= 3 AND A5%(1) <= 4 AND B1%(2, 0) = 0 AND B1%(3, 0) = 0 THEN CALL CHARGERUN(D,i)

6821 i = 0: B1%(0, 0) = B1!(p): B1%(0, 1) = P1%(D)
	IF A5%(0) = 0 THEN 6835
	IF A5%(0) > 0 AND A5%(0) < 4 THEN I3 = A5%(0): CALL BATTER3(I3, p, D)
	IF A5%(0) = 4 THEN I3 = A5%(0): CALL CHARGERUN(D,i)
	IF A5%(0) = 11 THEN I3 = 1: CALL CLEARBASE(i)
	IF A5%(0) = 12 THEN I3 = 2: CALL CLEARBASE(i)
	IF A5%(0) = 13 THEN I3 = 3: CALL CLEARBASE(i)
	IF A5%(0) = 14 THEN I3 = 4: CALL CLEARBASE(i)

6835 '
END SUB

SUB ADV1BASE (i)
	FOR I4 = 0 TO 2
	B0%(i + 1, I4) = B0%(i, I4): B0%(i + 1, 2) = 0: B0%(i, I4) = 0
	NEXT I4
	Q$(i + 1) = Q$(i): Q$(i) = ""
END SUB

SUB ADV2BASES (i)
	FOR I4 = 0 TO 2
	B0%(i + 2, I4) = B0%(i, I4): B0%(i + 1, 2) = 0: B0%(i, I4) = 0
	NEXT I4
	Q$(i + 2) = Q$(i): Q$(i) = ""
END SUB

SUB ADV3BASES (I3, p, D)
	B0%(I3, 0) = B1!(p): B0%(I3, 1) = P1%(D): Q$(I3) = Q$(0): B0%(I3, 2) = 0
	B0%(0, 0) = 0
END SUB

SUB ADVANCEMENT (p, D, S2%, F%,I3)
	IF B0%(3, 0) = 0 OR A5%(3) = 0 THEN 3805
	i = 3
	IF A5%(3) = 10 THEN I3 = 3: CALL RecordOut(i, I3)
	IF A5%(3) >= 1 AND A5%(3) <= 4 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF A5%(3) = 11 THEN I3 = 4: CALL RecordOut (i, I3)

3805 IF B0%(2, 0) = 0 OR A5%(2) = 0 THEN 3812
	i = 2
	IF A5%(2) = 10 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(2) = 11 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(2) = 12 AND B0%(3, 0) = 0 THEN I3 = 4: CALL RecordOut (i, I3)
	IF A5%(2) = 1 AND B0%(3, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(2) >= 2 AND A5%(2) <= 4 AND B0%(3, 0) = 0 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

3812 IF B0%(1, 0) = 0 OR A5%(1) = 0 THEN 3821
	i = 1: IF A5%(1) = 8 AND B0%(2, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(1) = 10 THEN I3 = 1: CALL RecordOut (i, I3)
	IF A5%(1) = 11 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(1) = 12 AND B0%(3, 0) = 0 AND B0%(2, 0) = 0 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(1) = 13 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN I3 = 4: CALL RecordOut (i, I3)
	IF A5%(1) = 1 AND B0%(2, 0) = 0 THEN CALL ADV1BASE (i)
	IF A5%(1) = 2 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN CALL ADV2BASES (i)
	IF A5%(1) >= 3 AND A5%(1) <= 4 AND B0%(2, 0) = 0 AND B0%(3, 0) = 0 THEN CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)

3821 i = 0: B0%(0, 0) = B1!(p): B0%(0, 1) = P1%(D)
	IF A5%(0) = 0 THEN 3835
	IF A5%(0) > 0 AND A5%(0) < 4 THEN I3 = A5%(0): CALL ADV3BASES (I3, p, D)
	IF A5%(0) = 4 THEN I3 = A5%(0): CALL RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF A5%(0) = 11 THEN I3 = 1: CALL RecordOut (i, I3)
	IF A5%(0) = 12 THEN I3 = 2: CALL RecordOut (i, I3)
	IF A5%(0) = 13 THEN I3 = 3: CALL RecordOut (i, I3)
	IF A5%(0) = 14 THEN I3 = 4: CALL RecordOut (i, I3)

3835 IF F% = 0 AND O%(1) < 3 THEN CALL ADJRUNNERS (S2%, p)
	FOR i = 0 TO 3: A5%(i) = 0: NEXT i
END SUB

SUB BATDISP (PG, P9)
	LOCATE 1, 1
	COLOR 14: PRINT "#"; : COLOR l%(P9, 11), l%(P9, 12): PRINT A$(P9); : COLOR 14, 0
	LOCATE , 16: PRINT "B   POS    G  AB 2B 3B HR RBI  BB  SO SRAB  SB CS G%  BA"
	FOR i = 2 TO 13: LOCATE i, 1: PRINT STRING$(78, 32): NEXT: LOCATE 2, 1
	IF PG = 0 THEN AA = 0: BB = 12
	IF PG = 1 THEN AA = 13: BB = 22
	FOR i = AA TO BB: LOCATE , 1
	IF b%(P9, i, 21) <> 0 THEN 150
	COLOR 2, 0
	IF i < 10 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	PRINT TAB(3); b$(P9, i); TAB(17); B1$(b%(P9, i, 0) + 2);
	FOR xx = 20 TO 23
	PRINT TAB(xx); : IF b%(P9, i, xx + 2) <> -1 THEN PRINT USING "#"; b%(P9, i, xx + 2);
	NEXT
	PRINT TAB(24);
	PRINT USING " ### ### ## ## ## ### ### ### #"; b%(P9, i, 3); b%(P9, i, 4); b%(P9, i, 7); b%(P9, i, 8); b%(P9, i, 9); b%(P9, i, 10); b%(P9, i, 11); b%(P9, i, 12); b%(P9, i, 16);
	PRINT USING "#"; b%(P9, i, 17); : PRINT USING "#"; b%(P9, i, 15); : PRINT USING "# "; b%(P9, i, 30);
	PRINT USING "### ## ## "; b%(P9, i, 13); b%(P9, i, 14); b%(P9, i, 18); : PRINT TAB(69); ".";
	IF b%(P9, i, 4) <> 0 THEN PRINT USING "###"; INT(((b%(P9, i, 6) / b%(P9, i, 4)) + .0005) * 1000): GOTO 150
	PRINT

150 NEXT i
END SUB

SUB BATOFF (p, I5, I6)
	B8%(p, I5, I6) = B8%(p, I5, I6) - 1: B5%(p, I5, I6) = B5%(p, I5, I6) - 1
	IF I6 > 8 OR I6 = 2 OR I6 = 3 THEN 100
	IF P%(1 - P, P1%(1 - P), 0) = 1 THEN 100
	IF I6 > = 4 THEN CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) - 1:GOTO 100
	CSS%(P, I5, I6) = CSS%(P, I5, I6) - 1

100 END SUB

SUB BATTER1 (i)
	FOR I4 = 0 TO 1
	B1%(i + 1, I4) = B1%(i, I4): B1%(i, I4) = 0
	NEXT I4
END SUB

SUB BATTER2 (i)
	FOR I4 = 0 TO 1
	B1%(i + 2, I4) = B1%(i, I4): B1%(i, I4) = 0
	NEXT I4
END SUB

SUB BATTER3 (I3, p, D)
	B1%(I3, 0) = B1!(p): B1%(I3, 1) = P1%(D)
END SUB

SUB BLANKLINE ()
	PRINT "                                            ";
END SUB

SUB CAUGHTFLY (i, S2%, D)
	CALL RANDNUM(rn)
	IF rn <= 96 THEN i = 1
	IF rn = 97 THEN i = 2
	IF rn = 98 THEN i = 3
	IF rn = 99 THEN i = 4
	IF rn = 100 THEN i = 6
	ON i GOTO 4352, 4354, 4356, 4358, 4361, 4362

4352 IF S2% = 17 THEN 4353
	Y% = INT(RND(1) * 60) + 1230
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Q$(4)+" "+Y$)

4353 IF S2% = 17 AND l%(1, 13) = 37 THEN CALL PBP ("he climbs the terrace")
	Y% = INT(RND(1) * 20) + 1290
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)
	GOTO 4359

4354 CALL PBP (Q$(4)+" makes a sliding catch"):CALL PBP ("What a play!!!"): GOTO 4359

4356 CALL PBP (Q$(4)+" makes a leaping grab"):CALL PBP ("What a catch!!!"): GOTO 4359

4362 CALL PBP (Q$(4)+" makes an incredible catch!!"): GOTO 4359

4358 CALL PBP (Q$(4)+" makes a diving catch!!!")

4359 '

4361 '
END SUB

SUB CHANGELINEUP (P9, p)
	LOCATE 14, 1: COLOR 2: PRINT " # LINEUP       PS  FA  OB"
	COLOR 15
	FOR i = 1 TO 9: LOCATE , 1
	IF B1!(P9) = i THEN PRINT "*";
	LOCATE , 2: PRINT USING "#"; i; : LOCATE , 4
	IF B7%(P9, i) = 1 THEN PRINT p$(P9, P1%(P9));
	IF B7%(P9, i) <> 1 THEN PRINT b$(P9, B3%(P9, i));
	IF B7%(P9, i) <> 0 AND B7%(P9, i) < 10 THEN LOCATE , 20: PRINT b%(P9, B3%(P9, i), 19);
	LOCATE , 17: PRINT C$(B7%(P9, i));
	LOCATE , 25
	FOR I1 = 1 TO 3
	IF B0%(I1, 0) = i AND P9 = p THEN COLOR 3: PRINT I1; : COLOR 15
	NEXT I1
	PRINT " "
	NEXT i
END SUB

SUB CHARGERUN (D,i)
	IF O%(1) >= 3 THEN CALL CLEARBASE (i): GOTO 6841
	I5 = B1%(i, 1): I6 = 3: CALL ADDPIT (D, I5, I6): CALL CLEARBASE (i)

6841 '
END SUB

SUB CHECKSAVE (D, p, S4%)
	I3 = S6%(D, 0) - S6%(p, 0): S1%(D) = -1: IF I3 < 1 THEN 1807
	IF S4% = 0 AND I3 <= 2 THEN S1%(D) = P1%(D)
	IF S4% > 0 AND S4% < 4 AND I3 <= 3 THEN S1%(D) = P1%(D)
	IF S4% > 3 AND S4% < 7 AND I3 <= 4 THEN S1%(D) = P1%(D)
	IF S4% = 7 AND I3 <= 5 THEN S1%(D) = P1%(D)
	IF S4% = 0 AND O%(0) = 0 AND I3 <= 3 THEN S1%(D) = P1%(D)
	IF INNING% > 5 AND I3 > 0 AND INNING% < 8 THEN S1%(D) = P1%(D)

1807 RI = 0: IF S4% = 0 THEN 1820
	IF S4% = 1 OR S4% = 2 OR S4% = 3 THEN RI = 1
	IF S4% = 4 OR S4% = 5 OR S4% = 6 THEN RI = 2
	IF S4% = 7 THEN RI = 3
	P5%(D, P1%(D), 17) = P5%(D, P1%(D), 17) + RI: P8%(D, P1%(D), 17) = P8%(D, P1%(D), 17) + RI

1820 '
END SUB

SUB CLEARBASE (i)
	B1%(i, 0) = 0: B1%(i, 1) = 0
END SUB

SUB CREDITASSIST (W%, D)
	'*** CREDIT ASSISTS ***
	IF W% = 1 THEN I5 = P1%(D): I6 = 39: CALL ADDPIT(D, I5, I6): GOTO 3408
	I5 = B9%(D, W%): I6 = 17: CALL INCBATDF(D, I5, I6)

3408 '
END SUB

SUB CREDITPUTOUT (I3, FB2P%, AST%, D)
	'*** CREDIT INFIELD PUTOUTS ***
	if i3>4 then i3=1
	po% = I3 + 2: ' 1=1st 2=2nd 3=3rd 4=home
	IF po% = 6 THEN po% = 2: GOTO 3417
	IF po% = 3 AND FB2P% = 1 THEN FB2P% = 0: po% = 1: GOTO 3415
	IF po% = 3 THEN 3417
	IF po% = 4 AND AST% = 4 THEN po% = 6: GOTO 3417
	IF po% = 4 AND AST% = 6 THEN po% = 4: GOTO 3417
	GOTO 3417

3415 IF po% = 1 THEN I5 = P1%(D): I6 = 38: CALL ADDPIT(D, I5, I6): GOTO 3418

3417 AST% = 0
	I5 = B9%(D, po%): I6 = 16: CALL INCBATDF(D, I5, I6)

3418 '
END SUB

SUB CREDITREGPO (W%, D)
	'*** CREDIT REGULAR PUTOUT  ***
	IF W% = 1 THEN I5 = P1%(D): I6 = 38: CALL ADDPIT(D, I5, I6): GOTO 3429
	I5 = B9%(D, W%): I6 = 16: CALL INCBATDF(D, I5, I6)

3429 '
END SUB

SUB DELAY ()
	IF G4 = 0 THEN 6097
	U! = TIMER: WHILE TIMER < U! + G4: WEND

6097 '
END SUB

SUB DELIVERY (D)
	CALL RANDNUM(RN)
	IF RN<=20 THEN
		Y% = INT(RND(1) * 20) + 50
		CALL GETRPBP(Y%, Y$, D)
		CALL STRIPRPBP(Y$, Y%, D)
		CALL PBP(Y$)
	END IF
	'*** setup the delivery ***
	Y% = INT(RND(1) * 46) + 1
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP(Y$)
END SUB

SUB DISPFLDG (PG, P9, TG)
	'DIS FLDG
	LOCATE 1, 16: PRINT "P1 FAVG A R   P2 FAVG A R   P3 FAVG A R   P4 FAVG A R      "
	FOR i = 2 TO 13: LOCATE i, 1: PRINT STRING$(78, 32): NEXT
	LOCATE 2
	IF PG = 0 THEN AA = 0: BB = 12
	IF PG = 1 THEN AA = 13: BB = 22
	FOR i = AA TO BB: IF b%(P9, i, 21) <> 0 THEN 5802
	IF i < 10 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	PRINT TAB(3); b$(P9, i);
	LOCATE , 17: IF b%(P9, i, 22) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 22); b%(P9, i, 26); b%(P9, i, 33); b%(P9, i, 34);
	LOCATE , 31: IF b%(P9, i, 23) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 23); b%(P9, i, 27); b%(P9, i, 35); b%(P9, i, 36);
	LOCATE , 45: IF b%(P9, i, 24) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 24); b%(P9, i, 28); b%(P9, i, 37); b%(P9, i, 38);
	LOCATE , 59: IF b%(P9, i, 25) <> -1 THEN PRINT USING "# #### # #"; b%(P9, i, 25); b%(P9, i, 29); b%(P9, i, 39); b%(P9, i, 40);
	PRINT

5802 NEXT i: TG = TG + 1
END SUB

SUB DISPRUN (I2, p, X)
	IF I2=2 THEN CALL GMove2VE (37,304,10,32,VA%(11),-1)
	IF I2=3 THEN CALL GMove2VE (15,336,10,32,VA%(12),-1)
	IF I2=1 THEN CALL GMove2VE (56,336,10,32,VA%(13),-1)
	J = B0%(I2, 0): IF J = 0 THEN 363
	Y=22
	IF I2=2 THEN Y=20
	IF B7%(p, J) = 1 AND p%(p, P1%(p), 11) <> 999 THEN F$=p$(p, P1%(p)):CALL SPA(F$,10):GPRINT1VE Y,X,F$,15:GPRINT1VE Y+1,X,"1/  0",15 :GOTO 363
	IF B7%(p, J) = 1 THEN F$=p$(p, P1%(p)):CALL SPA(F$,10):GPRINT1VE Y,X,F$,15:F$=FUSING$(STR$(p%(p, P1%(p), 31)),"#")+"/"+FUSING$(STR$(p%(p, P1%(p), 29)),"###"):GPRINT1VE Y+1,X,F$,15 :GOTO 363
	F$=b$(p, B3%(p, J)):CALL SPA(F$,10):GPRINT1VE Y,X,F$,15:F$=FUSING$(STR$(b%(p, B3%(p, J), 16)),"#")+"/"+FUSING$(STR$(b%(p, B3%(p, J), 13)),"###"):GPRINT1VE Y+1,X,F$,15

363 'DONE
END SUB

SUB DPPBP ()
	CALL RANDNUM(rn)
	IF rn <= 20 THEN F$="may be a double play ball"
	IF rn > 20 AND rn <= 40 THEN F$="they might turn two"
	IF rn > 40 AND rn <= 60 THEN F$="they could get two"
	IF rn > 60 AND rn <= 80 THEN F$="could be two"
	IF rn > 80 THEN F$="could be a double play"
	CALL PBP (F$)
END SUB

SUB DPPIVOT (W%, D)
	'*** double play pivot ***
	IF W% = 4 THEN ww% = 6
	IF W% = 6 THEN ww% = 4
	IF W% = 3 THEN ww% = 1
	Y% = INT(RND(1) * 20) + 1910
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	F$=Y$+" "+b$(D, B9%(D, ww%))
	CALL PBP (F$)
END SUB

SUB DPRELAY ()
	CALL RANDNUM(rn)
	IF rn <= 10 THEN CALL PBP ("Give me the Hoover!"): GOTO 3716
	IF A5%(0) <> 11 THEN 3716
	CALL RANDNUM(rn)
	IF rn <= 8 THEN F$="over to 1st..."
	IF rn > 8 AND rn <= 16 THEN F$="throw to 1st..."
	IF rn > 16 AND rn <= 24 THEN F$="on to first..."
	IF rn > 24 AND rn <= 30 THEN F$=Q$(4)+" pivots and throws"
	IF rn > 30 AND rn <= 36 THEN F$=Q$(4)+" gets the throw off"
	IF rn > 36 AND rn <= 44 THEN F$=Q$(4)+" is hit as he throws"
	IF rn > 44 AND rn <= 50 THEN F$=Q$(4)+" leaps and throws"
	IF rn > 50 AND rn <= 56 THEN F$=Q$(4)+" jumps over the sliding "+Q$(1)
	IF rn > 56 AND rn <= 64 THEN F$="the relay to first..."
	IF rn > 64 AND rn <= 72 THEN F$="quick turn by "+Q$(4)
	IF rn > 72 AND rn <= 80 THEN F$="relay to 1st..."
	IF rn > 80 AND rn <= 88 THEN F$="to 1st..."
	IF rn > 88 AND A5%(0) = 11 THEN F$="over to 1st..."
	CALL PBP (F$)

3716 '
END SUB

SUB DPSAFE ()
	CALL RANDNUM(rn)
	IF rn <= 8 THEN F$="over to 1st..."
	IF rn > 8 AND rn <= 16 THEN F$="throw to 1st..."
	IF rn > 16 AND rn <= 24 THEN F$="on to first..."
	IF rn > 24 AND rn <= 30 THEN F$="pivot to first"
	IF rn > 30 AND rn <= 36 THEN F$="the throw to first"
	IF rn > 36 AND rn <= 44 THEN F$="relay to first"
	IF rn > 44 AND rn <= 50 THEN F$="quick turn to 1st"
	IF rn > 50 AND rn <= 56 THEN F$="on to first..."
	IF rn > 56 AND rn <= 64 THEN F$="the relay to first..."
	IF rn > 64 AND rn <= 72 THEN F$="quick turn"
	IF rn > 72 AND rn <= 80 THEN F$="relay to 1st..."
	IF rn > 80 AND rn <= 88 THEN F$="to 1st..."
	IF rn > 88 AND A5%(0) = 11 THEN F$="over to 1st..."
	CALL PBP (F$)
	Y% = INT(RND(1) * 20) + 1890
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)
END SUB

SUB DRAWBOX (J)
	LOCATE 8, 8: COLOR l%(1, 11), l%(1, 12)
	PRINT " Ö"; STRING$(61, 196); "· ";
	FOR II = 1 TO J
	LOCATE 8 + II, 8
	PRINT " º"; STRING$(61, 32); "º ";
	NEXT
	LOCATE 9 + J, 8: PRINT " Ó"; STRING$(61, 196); "½ ";
END SUB

SUB EJECTINC (EJ%, X$)
	EJ% = EJ% + 1: EJ$(EJ%) = X$
END SUB

SUB FLDAVG (D, W%, I3)
	IF b%(D, B9%(D, W%), 19) >= 999 THEN I3 = 999: GOTO 4454
	IF W% >= 7 THEN I3 = b%(D, B9%(D, W%), 19): GOTO 4454
	IF W% = 3 OR W% = 2 THEN I3 = (b%(D, B9%(D, W%), 19)) * .96: GOTO 4454
	'IF W%<>1 AND W%<>4 AND W%<>6 THEN I3=(B%(D,B9%(D,W%),19))*.985

4454 I4 = b%(D, B9%(D, W%), 22)
	I5 = b%(D, B9%(D, W%), 23)
	I6 = b%(D, B9%(D, W%), 24)
	I7 = b%(D, B9%(D, W%), 25)
	IF W% = I4 OR W% = I5 OR W% = I6 OR W% = I7 THEN 4462
	IF W% = 4 OR W% > 6 THEN I3 = 982
	IF W% = 2 THEN I3 = 987
	IF W% = 3 THEN I3 = 980
	IF W% = 6 OR W% = 5 THEN I3 = 964
	IF W% >= 7 THEN I3 = I3 - 10: GOTO 4462
	I3 = I3 - 20
	IF W% = 2 OR W% >= 4 AND W% <= 6 THEN I3 = I3 - 80: GOTO 4464

4462 IF W% = 4 OR W% = 6 THEN I3 = (b%(D, B9%(D, W%), 19)) * .995
	IF W% = 5 THEN I3 = (b%(D, B9%(D, W%), 19)) * .985

4464 REM
END SUB

SUB SPA (F$,L)
	F$=LEFT$(F$,L)

10 IF LEN(F$)<L THEN F$=F$+" ":GOTO 10
END SUB

SUB FLDDISP (BC%, p, U%, D)
REDIM FORMATTED$(3)
	X% = 1: Y% = 9
	IF INNING% > 9 THEN X% = 10: Y% = 18
	IF INNING% > 18 THEN X% = 19: Y% = 27
	FOR I=23 TO 30
	GPRINT0VE I,66,"               ",0
	NEXT
	FOR I=28 TO 30
	GPRINT0VE I,15,"                                                   ",0
	NEXT
	FOR X1% = 0 TO 2
	FORMATTED$(X1%)=FUSING$(STR$(X% + 3 * X1%),"##")+" "+FUSING$(STR$(X% + 3 * X1% + 1),"##")+" "+FUSING$(STR$(X% + 3 * X1% + 2),"##")+"  "
	NEXT
	FORMATTED$=FORMATTED$(0)+FORMATTED$(1)+FORMATTED$(2)+" R  H  E B"+STR$(BC%)
	GPRINT0VE 28,23,FORMATTED$,15
	 'PRINT "WIND: "; : COLOR 14: PRINT WI$
	FOR i = 0 TO 1
	GPRINT0VE I+29,15,LEFT$(A$(i), 3)+NN$(i),15
	 FOR I2 = X% TO Y%
	IF p = 0 AND i = 0 AND I2 > INNING% - 1 AND s%(i, INNING%) = 0 OR p = 0 AND i = 0 AND I2 > INNING% AND s%(i, INNING%) > 0 OR p = 1 AND I2 > INNING% AND i = 0 THEN 310
	IF p = 0 AND i = 1 AND I2 > INNING% - 1 OR p = 1 AND i = 1 AND s%(i, INNING%) = 0 AND I2 > INNING% - 1 OR p = 1 AND i = 1 AND s%(i, INNING%) > 0 AND I2 > INNING% THEN 310
	CALL INNDISP(I2, i)
	NEXT I2

310 FORMATTED$=FUSING$(STR$(S6%(I,0)),"##")+" "+FUSING$(STR$(S6%(I,1)),"##")+" "+FUSING$(STR$(S6%(I,2)),"##")+" "
	IF i = 0 THEN FORMATTED$=FORMATTED$+"S"+STR$(U%)
	'PRINT "SKY:  "; : COLOR 14: PRINT SC$(SC%)
	IF i = 1 THEN FORMATTED$=FORMATTED$+"O"+STR$(O%(0))
	'PRINT "TEMP:"; : COLOR 14: PRINT TP%
	GPRINT0VE I+29,53,FORMATTED$,15
	NEXT i
	CALL GMove2VE (38,224,10,32,VA%(2),-1)
	CALL GMove2VE (7,256,10,32,VA%(3),-1)
	CALL GMove2VE (65,256,10,32,VA%(4),-1)
	CALL GMove2VE (30,272,10,32,VA%(5),-1)
	CALL GMove2VE (45,272,10,32,VA%(6),-1)
	CALL GMove2VE (15,304,10,32,VA%(7),-1)
	CALL GMove2VE (61,304,10,32,VA%(8),-1)
	CALL GMove2VE (35,336,13,32,VA%(9),-1)
	CALL GMove2VE (35,416,15,16,VA%(10),-1)
	 F$=LEFT$(b$(D, B9%(D, 8)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 15,38,F$,0
	F$="A"+FUSING$(STR$(b%(D, B9%(D, 8), 15)),"#")+" R"+FUSING$(STR$(b%(D, B9%(D, 8), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 8), 19)),"###")
	GPRINT1VE 16,38,F$,0
	 F$=LEFT$(b$(D, B9%(D, 7)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 17,7,F$,0
	F$="A"+FUSING$(STR$(b%(D, B9%(D, 7), 15)),"#")+" R"+FUSING$(STR$(b%(D, B9%(D, 7), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 7), 19)),"###")
	GPRINT1VE 18,7,F$,0
	 F$=LEFT$(b$(D, B9%(D, 9)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 17,65,F$,0
	F$="A"+FUSING$(STR$(b%(D, B9%(D, 9), 15)),"#")+" R"+FUSING$(STR$(b%(D, B9%(D, 9), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 9), 19)),"###")
	GPRINT1VE 18,65,F$,0
	 F$=LEFT$(b$(D, B9%(D, 6)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 18,30,F$,0
	F$="R"+FUSING$(STR$(b%(D, B9%(D, 6), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 6), 19)),"###")
	GPRINT1VE 19,30,F$,0
	 F$=LEFT$(b$(D, B9%(D, 4)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 18,45,F$,0
	F$="R"+FUSING$(STR$(b%(D, B9%(D, 4), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 4), 19)),"###")
	GPRINT1VE 19,45,F$,0
	 F$=LEFT$(b$(D, B9%(D, 5)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 20,15,F$,0
	F$="R"+FUSING$(STR$(b%(D, B9%(D, 5), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 5), 19)),"###")
	GPRINT1VE 21,15,F$,0
	 F$=LEFT$(b$(D, B9%(D, 3)), 10)
	CALL SPA (F$,10)
	GPRINT1VE 20,61,F$,0
	F$="R"+FUSING$(STR$(b%(D, B9%(D, 3), 17)),"#")+" F"+FUSING$(STR$(b%(D, B9%(D, 3), 19)),"###")
	GPRINT1VE 21,61,F$,0
	 F$="["+B1$(p%(D, P1%(D), 0) + 2)+"] "+LEFT$(p$(D, P1%(D)), 10)
	CALL SPA (F$,13)
	GPRINT1VE 22,35,F$,0
	F$="H"+FUSING$(STR$(P%(D, P1%(D), 18)),"#")+" F"+FUSING$(STR$(P%(D, P1%(D), 17)),"###")
	GPRINT1VE 23,39,F$,0
	 F$=LEFT$(b$(D, B9%(D, 2)), 10)
	CALL SPA (F$,10)
	F$=F$+"A"+FUSING$(STR$(b%(D, B9%(D, 2), 15)),"#")
	GPRINT1VE 27,35,F$,0
	END SUB

SUB FLDERR (F%, W%, D, I3, SB%, S2%, p)
	F% = 0: IF W% = 1 THEN I3 = p%(D, P1%(D), 17) + 30
	IF W% = 1 AND SB% = 1 THEN I3 = 995
	IF W% <> 1 THEN CALL FLDAVG (D, W%, I3)
	IF I3 >= 999 THEN I6 = 1: GOTO 4408
	IF S2% >= 12 AND S2% <= 15 THEN I6 = 1009 - I3
	IF S2% >= 7 AND S2% <= 9 THEN I6 = 1006 - I3
	IF S2% = 16 OR S2% >= 22 THEN I6 = 1000 - I3
	IF S2% >= 17 AND S2% <= 21 THEN I6 = (1000 - I3) * .05

4408 IF W% = 7 THEN I6 = I6 - 4
	IF W% = 8 THEN I6 = I6 - 10
	IF W% = 9 THEN I6 = I6 - 15
	IF W% = 5 THEN I6 = I6 - 15
	H6% = INT(RND(1) * 1000) + 1: IF H6% > I6 THEN 4421
	F% = 1: i = INT(RND(1) * 2) + 1: I1 = 2
	IF W% = 1 THEN I5 = P1%(D): I6 = 34: CALL ADDPIT(D, I5, I6)
	IF W% <> 1 THEN I5 = B9%(D, W%): I6 = 11: CALL INCBATDF(D, I5, I6)
	IF S2% <> 12 THEN 4410
	IF S2% = 12 THEN F$=Q$(0)+" beats out an infield single": CALL PBP (F$)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6): I6 = 1: CALL INCBATOFF(P, I5, I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT(D, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6): I6 = 24: CALL INCPITOFF(P, I5, I6)

4410 IF S2% >= 17 AND S2% <= 21 THEN I1 = 3
	IF S2% >= 17 AND S2% <= 20 THEN CALL OUTFIELDERR
	IF S2% = 8 OR S2% = 9 OR S2% >= 22 OR S2% = 16 THEN I1 = 1
	IF S2% >= 12 AND S2% <= 14 AND i = 1 OR S2% = 15 THEN I1 = 1
	CALL RANDNUM(rn)
	IF I1 = 1 AND W% = 1 AND S2% = 16 THEN F$="*** WILD PICK OFF THROW BY": GOTO 4415
	IF I1 = 1 AND rn <= 50 THEN F$="*** THROWING ERROR ON": GOTO 4415
	IF I1 = 1 AND rn > 50 AND rn <= 55 THEN F$="*** BALL KICKED BY": GOTO 4415
	IF I1 = 1 THEN F$="*** "+Q$(4)+" THROWS IT AWAY ***": CALL PBP (F$):GOTO 4403
	IF I1 = 2 AND rn <= 30 THEN F$="*** FIELDING ERROR ON": GOTO 4415
	IF I1 = 2 AND rn > 30 AND rn <= 50 THEN F$="*** OFF THE GLOVE OF": GOTO 4415
	IF I1 = 2 AND rn > 50 AND rn <= 75 THEN F$="*** BALL IS BOOTED BY": GOTO 4415
	IF I1 = 2 AND rn > 75 AND rn <= 95 THEN F$="*** BALL IS BOBBLED BY": GOTO 4415
	IF I1 = 2 AND rn > 95 THEN F$="*** BALL IS THROUGH THE LEGS OF": GOTO 4415
	IF I1 = 3 AND rn <= 33 THEN F$="*** BALL DROPPED BY": GOTO 4415
	IF I1 = 3 AND rn > 33 AND rn <= 66 THEN F$="*** OFF THE GLOVE OF": GOTO 4415
	IF I1 = 3 THEN F$="*** BALL IS MISPLAYED BY"

4415 F$=F$+" "+Q$(4)+" ***":CALL PBP (F$)

4403 IF S2% = 16 OR S2% = 22 THEN I8 = INT(RND(1) * 10) + 1: IF I8 < 5 THEN A5%(1) = 2: A5%(2) = 2: A5%(3) = 1: A5%(0) = 2: GOTO 4418
	i = INT(RND(1) * 100) + 1
	A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 1
	IF S2% > 17 AND S2% <= 21 THEN 4418
	IF i <= 17 AND S2% <> 8 OR S2% = 12 THEN A5%(1) = 2: A5%(2) = 2: A5%(0) = 2

4418 IF S2% >= 17 AND S2% <= 20 AND O%(0) = 2 OR S2% >= 23 OR S2% = 16 AND W% = 2 THEN A5%(2) = 2: A5%(1) = 2: A5%(0) = 2: A5%(3) = 1
	IF S2% >= 17 AND S2% <= 19 AND i <= 33 AND O%(0) < 2 AND B0%(1, 0) = 0 THEN A5%(0) = 2
	IF S2% = 16 OR S2% = 22 THEN A5%(0) = 0
	IF I1 > 0 AND S2% <> 7 AND S2% <> 8 AND S2% <> 9 AND S2% <> 16 AND S2% <> 22 THEN O%(1) = O%(1) + 1
	IF S2% = 16 OR S2% = 7 OR S2% = 8 OR S2% = 9 OR S2% = 22 THEN A5%(0) = 0: IF I8 > 4 AND W% <> 2 THEN A5%(1) = 1: A5%(2) = 1: A5%(3) = 1
	S6%(D, 2) = S6%(D, 2) + 1
	CALL ADVANCEMENT(p, D, S2%, F%,I3)

4421 '
END SUB

SUB GETAKEY ()
	IF AP% = 1 AND u6 = 2 THEN 4094
	F$="Hit any key to continue":CALL PBP (F$)
4093 i$ = GetKeyPress$
4094 '
END SUB

SUB GETFIELDER (W%, p, I1, D)
	W% = 0
	i = INT(RND(1) * 100) + 1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) < 0 THEN I1 = -1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) > 0 THEN I1 = 1
	IF B7%(p, B1!(p)) <> 1 AND b%(p, B3%(p, B1!(p)), 0) = 0 THEN I1 = -1 * p%(D, P1%(D), 0)
	IF B7%(p, B1!(p)) = 1 THEN I1 = p%(p, P1%(p), 0)
END SUB

SUB GETFIELDNAME (W%, D)
	IF W% <> 1 THEN Q$(4) = b$(D, B9%(D, W%))
	IF W% = 1 THEN Q$(4) = p$(D, P1%(D))
END SUB

SUB GETINFIELDER (W%, p, I1, D, S2%, P2)
	CALL GETFIELDER(W%, p, I1, D)
	i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 3
	IF I1 = 1 AND i <= 92 THEN W% = 4
	IF I1 = 1 AND i <= 72 THEN W% = 6
	IF I1 = 1 AND i <= 32 THEN W% = 5
	IF I1 = -1 THEN W% = 5
	IF I1 = -1 AND i <= 88 THEN W% = 6
	IF I1 = -1 AND i <= 68 THEN W% = 4
	IF I1 = -1 AND i <= 28 THEN W% = 3
	IF i <= 12 AND S2% <> 20 THEN W% = 1
	IF i <= 4 OR i <= 12 AND S2% = 20 THEN W% = 2
	IF S2% = 21 AND W% = 2 THEN W% = 1
	IF P2 = 4 AND W% = 6 THEN W% = 1
	IF P2 = 4 AND W% = 4 OR S2% = 20 AND W% = 1 THEN W% = 2
	CALL GETFIELDNAME(W%, D)
END SUB

SUB GETOUTFIELDER (W%, p, I1, D)
	CALL GETFIELDER(W%, p, I1, D)
	i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 7
	IF I1 = 1 AND i <= 50 THEN W% = 9
	IF I1 = -1 THEN W% = 9
	IF I1 = -1 AND i <= 50 THEN W% = 7
	IF i <= 30 THEN W% = 8
	Q$(4) = b$(D, B9%(D, W%))
END SUB

SUB GETRPBP (Y%, Y$, D)
OPEN "revised.pbp" FOR RANDOM AS #1 LEN = 44
	FIELD #1, 44 AS YY$
	GET #1, Y%
	Y$ = YY$
CLOSE 1

3996 'PRINT Y$; " BEF", Y%, YY$
	IF RIGHT$(Y$, 1) = " " THEN Y$ = LEFT$(Y$, LEN(Y$) - 1): GOTO 3996
	if TGTBASE% = 3 and left$(y$, 2) = "Q1" then y$ = "Q2 " + right$(y$,len(y$)-3)
	if TGTBASE% = 3 and right$(y$, 2) = "Q1" then y$ = left$(y$,len(y$)-2) + "Q2"
	IF LEFT$(Y$, 2) = "BB" THEN Y$ = Q$(0) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q1" THEN Y$ = Q$(1) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q2" THEN Y$ = Q$(2) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "PP" THEN Y$ = p$(D, P1%(D)) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "UU" THEN Y$ = U$(1) + RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "XX" THEN Y$ = b$(D, B9%(D, 5))+" and "+b$(D, B9%(D, 6))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "TT" THEN Y$ = b$(D, B9%(D, 5))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "FB" THEN Y$ = b$(D, B9%(D, 3))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "YY" THEN Y$ = b$(D, B9%(D, 4))+" and "+b$(D, B9%(D, 3))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "ZZ" THEN Y$ = b$(D, B9%(D, 4))+" and "+b$(D, B9%(D, 6))+ RIGHT$(Y$, LEN(Y$) - 2)
	IF LEFT$(Y$, 2) = "Q4" THEN Y$ = Q$(4) + RIGHT$(Y$, LEN(Y$) - 2)
	IF RIGHT$(Y$, 3) = " PP" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + p$(D, P1%(D))
	IF RIGHT$(Y$, 3) = " BB" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(0)
	IF RIGHT$(Y$, 2) = "Q1" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(1)
	IF RIGHT$(Y$, 2) = "Q2" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(2)
	IF RIGHT$(Y$, 2) = "UU" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + U$(1)
	IF RIGHT$(Y$, 3) = " TT" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 5))
	IF RIGHT$(Y$, 2) = "FB" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 3))
	IF RIGHT$(Y$, 2) = "Q4" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + Q$(4)
	IF RIGHT$(Y$, 3) = " XX" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 5))+" and "+b$(D, B9%(D, 6))
	IF RIGHT$(Y$, 2) = "YY" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 3))+" and "+b$(D, B9%(D, 4))
	IF RIGHT$(Y$, 3) = " ZZ" THEN Y$ = LEFT$(Y$, LEN(Y$) - 2) + b$(D, B9%(D, 6))+" and "+b$(D, B9%(D, 4))
	'PRINT Y$; " AFT"
END SUB

SUB GROUNDERPBP (W%, D)
	IF W% = 2 THEN 3760
	CALL RANDNUM(rn)
	Y% = INT(RND(1) * 140) + 1930
	IF rn <= 35 THEN 3752
	IF w% = 1 THEN Y% = INT(RND(1) * 160) + 2070
	if w% = 3 then Y% = INT(RND(1) * 150) + 2230
	if w% = 4 then Y% = INT(RND(1) * 160) + 2380
	if w% = 6 then Y% = INT(RND(1) * 160) + 2540
	if w% = 5 then Y% = INT(RND(1) * 150) + 2700

3752 CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	F$=Y$+" "

3751 GOTO 3765

3760 '*** in front of plate ***
	Y% = INT(RND(1) * 25) + 2870
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)

3765 '
END SUB

SUB GROUNDERS (W%, D)
	'*** grounders to infielders ***
	X$ = Q$(4)
	IF W% = 2 THEN 3953
	Y% = INT(RND(1) * 120) + 1420
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (X$+ " "+ Y$)
	GOTO 3954

3953 ' field grounders in front of the plate
	Y% = INT(RND(1) * 13) + 2850
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	IF LEFT$(Y$, 2) = "QQ" THEN Y$ = X$ + RIGHT$(Y$, LEN(Y$) - 2)
	CALL PBP (Y$)

3954 '
END SUB

SUB HITS2GAP (W%, p, I1, D)
	CALL GETFIELDER(W%, p, I1, D): i = INT(RND(1) * 100) + 1
	IF I1 = 1 THEN W% = 7
	IF I1 = 1 AND i <= 15 THEN W% = 9
	IF I1 = -1 THEN W% = 9
	IF I1 = -1 AND i <= 15 THEN W% = 7
	IF i <= 5 THEN W% = 8
	Q$(4) = b$(D, B9%(D, W%))
END SUB

SUB HOLDSTATUS (H0%)
	call gmove2ve (53,400,12,16,VA%(0),-1)
	F$="RUNNERS:"
	IF H0%=0 THEN F$=F$+"    "
	IF H0%=1 THEN F$=F$+"HELD"
	GPRINT1VE 26,53,F$,0
END SUB

SUB INCBATDF (D, I5, I6)
	B8%(D, I5, I6) = B8%(D, I5, I6) + 1
	B5%(D, I5, I6) = B5%(D, I5, I6) + 1
END SUB

SUB INCBATOFF (p, I5, I6)
	B8%(p, I5, I6) = B8%(p, I5, I6) + 1
	B5%(p, I5, I6) = B5%(p, I5, I6) + 1
	IF I6 > 8 OR I6 = 2 OR I6 = 3 THEN 105
	IF P%(1 - P, P1%(1 - P), 0) = 1 THEN 105
	IF I6 > = 4 THEN CSS%(P, I5, I6 - 2) = CSS%(P, I5, I6 - 2) + 1:GOTO 105
	CSS%(P, I5, I6) = CSS%(P, I5, I6) + 1

105 END SUB

SUB INCPITOFF (p, I5, I6)
	P8%(p, I5, I6) = P8%(p, I5, I6) + 1
	P5%(p, I5, I6) = P5%(p, I5, I6) + 1
END SUB

SUB INCREMENTHIT (p, D, MO)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 15: CALL INCBATOFF (P, I5, I6): GOTO 3646
	I5 = P1%(p): I6 = 37: CALL INCPITOFF(P, I5, I6)
3646 I5 = P1%(D): I6 = 13: CALL ADDPIT(D, I5, I6)
	IF MO > 0 THEN MO = MO - 1
END SUB

SUB INNDISP (I2, i)
	IF I2 < 4 THEN L=3 * I2 + 7
	IF I2 > 3 AND I2 < 7 THEN L=3 * I2 + 8
	IF I2 > 6 AND I2 < 10 THEN L=3 * I2 + 9
	IF I2 > 9 AND I2 < 13 THEN L=3 * (I2 - 9) + 7
	IF I2 > 12 AND I2 < 16 THEN L=3 * (I2 - 9) + 8
	IF I2 > 15 AND I2 < 19 THEN L=3 * (I2 - 9) + 9
	IF I2 > 18 AND I2 < 22 THEN L=3 * (I2 - 18) + 7
	IF I2 > 21 AND I2 < 25 THEN L=3 * (I2 - 18) + 8
	IF I2 > 24 AND I2 < 28 THEN L=3 * (I2 - 18) + 9
	formatted$=FUSING$(STR$(s%(i,i2)),"##")
	GPRINT0VE I+29,L+13,FORMATTED$,15
END SUB

SUB LDRIVE ()
	CALL RANDNUM(rn)
	IF rn <= 20 THEN X$ = "rips"
	IF rn > 20 AND rn <= 40 THEN X$ = "hammers"
	IF rn > 40 AND rn <= 60 THEN X$ = "lashes"
	IF rn > 60 AND rn <= 80 THEN X$ = "lines"
	IF rn > 80 THEN X$ = "drives"
	F$=F$+" "+ X$+ " "
END SUB

SUB LET2NUM (i$)
	IF UCASE$(i$) = "A" THEN i$ = "10"
	IF UCASE$(i$) = "B" THEN i$ = "11"
	IF UCASE$(i$) = "C" THEN i$ = "12"
	IF UCASE$(i$) = "D" THEN i$ = "13"
	IF UCASE$(i$) = "E" THEN i$ = "14"
	IF UCASE$(i$) = "F" THEN i$ = "15"
	IF UCASE$(i$) = "G" THEN i$ = "16"
	IF UCASE$(i$) = "H" THEN i$ = "17"
	IF UCASE$(i$) = "I" THEN i$ = "18"
	IF UCASE$(i$) = "J" THEN i$ = "19"
	IF UCASE$(i$) = "K" THEN i$ = "20"
	IF UCASE$(i$) = "L" THEN i$ = "21"
	IF UCASE$(i$) = "M" THEN i$ = "22"
END SUB

SUB LONGFLY (D, W%)
	Y% = INT(RND(1) * 60) + 1310
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$+ " "+ C$(W%))
	IF WF < 1 AND RND(1) > WF THEN CALL PBP ("The wind is bringing it back..."): END SUB
	IF WF > 1 AND RND(1) <= 1 - WF THEN CALL PBP ("It's carrying well in the breeze...")
	Y% = INT(RND(1) * 50) + 1370
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)
	CALL RANDNUM (rn)
	IF (l%(1, 13) = 21 OR l%(1, 13) = 27) AND W% = 7 THEN END SUB
	IF rn <= 5 THEN CALL PBP (Q$(4) + " leaps")
	IF rn >  5 and rn <= 10 THEN CALL PBP (Q$(4) + " against the wall")
	IF rn > 10 AND rn <= 20 THEN CALL PBP (Q$(4) + " jumps at the wall")
	IF rn > 20 AND rn <= 30 THEN CALL PBP (Q$(4) + " climbs the wall")

END SUB

SUB MAINSCREEN (D, p, BC%, U%, MO)
	CALL GMove4VE (30,400,22,16,VARSEG(VC%(0)),-1)
	CALL ClearVE
	GPRINT0VE 27,67, LEFT$(p$(D, P1%(D)),14),15
	formatted$=FUSING$(STR$(p%(D, P1%(D), 1)),"##")
	F$=FORMATTED$+"-"
	formatted$=FUSING$(STR$(p%(D, P1%(D), 2)),"##")
	F$=F$+FORMATTED$+" "+FUSING$(STR$(p%(D, P1%(D), 10) / 100),"##.##")
	GPRINT0VE 28,67, F$,15
	GPRINT0VE 29,66,"  IP  H BB  K",15
	IF P8%(D, P1%(D), 0) = 0 THEN F$= " 0.0": GOTO 338
	F$ = FUSING$(STR$(INT(P8%(D, P1%(D), 0) / 3)),"##")
	F$=F$+"."+FUSING$(STR$(P8%(D, P1%(D), 0) - (INT(P8%(D, P1%(D), 0) / 3) * 3)) , "#")

338 F$=F$+" "+FUSING$(STR$(P8%(D, P1%(D), 1)),"##")+" "+FUSING$(STR$(P8%(D, P1%(D), 4)),"##")+" "+FUSING$(STR$(P8%(D, P1%(D), 5)),"##")
	GPRINT0VE 30,66,F$,15
	 J = B3%(p, B1!(p))
	IF B7%(p, B1!(p)) = 1 THEN X$ = p$(p, P1%(p)):F$=X$:CALL SPA (X$,10):CALL PITPLACEMENT(p, X$): GOTO 335
	X$ = b$(p, J):F$=X$:CALL SPA (X$,10)
	IF b%(p, J, 0) < 0 OR b%(p, J, 0) = 0 AND p%(D, P1%(D), 0) > 0 THEN GPRINT1VE 26,42,X$,15
	X = LEN(X$): IF b%(p, J, 0) > 0 OR b%(p, J, 0) = 0 AND p%(D, P1%(D), 0) < 0 THEN GPRINT1VE 26,30,X$,15

335 CALL SPA (F$,14)
	GPRINT0VE 23,67, F$,15
	GPRINT0VE 24, 66, " AVG HR  BB   K", 15
	IF B7%(p, B1!(p)) = 1 THEN 336
	IF b%(p, J, 4) = 0 THEN GPRINT0VE 25, 66, ".000  0   0   0", 15:GOTO 337
	F$=FUSING$(STR$(b%(p, J, 6) / b%(p, J, 4)),".###")
	F$ = F$ + " " + FUSING$(STR$(b%(p, J, 9)),"##") + " " + FUSING$(STR$(b%(p, J, 11)),"###")+" "+FUSING$(STR$(b%(p, J, 12)),"###")
	GPRINT0VE 25,66,F$,15
	GOTO 337

336 IF p%(p, P1%(p), 11) <> 999 THEN F$=FUSING$(STR$(p%(p, P1%(p), 11) / 1000),".###"):GPRINT0VE 25,66,F$+"           ",15: GOTO 337
	IF p%(p, P1%(p), 20) = 0 THEN GPRINT0VE 25,66,".000  0   0   0",15: GOTO 337
	F$=FUSING$(STR$(P%(p, P1%(P), 22) / P%(p, P1%(P), 20)),".###")
	F$=F$+" "+FUSING$(STR$(P%(p, P1%(P), 25)),"##")+" "+FUSING$(STR$(P%(p, P1%(P), 27)),"###")+" "+FUSING$(STR$(P%(p, P1%(P), 28)),"###")
	GPRINT0VE 25,66,F$,15

337 IF B7%(p, B1!(p)) = 1 THEN 332
	F$=FUSING$(STR$(B8%(P, B3%(P,B1!(P)), 1)),"##")+"-"+FUSING$(STR$(B8%(P, B3%(P,B1!(P)), 0)),"##"):GOTO 333

332 F$=FUSING$(STR$(P8%(P, P1%(P), 24)),"##")+"-"+FUSING$(STR$(P8%(P, P1%(P), 23)),"##")

333 F$=F$+" TODAY"
	GPRINT0VE 26,66,F$,15
	GPRINT0VE 28,63,STR$(BC%),15
	GPRINT0VE 29,63,STR$(U%),15
	GPRINT0VE 30,63,STR$(O%(0)),15
	CALL GMove2VE (52,416,14,16,VA%(1),-1)
	F$="INF:"
	IF INFPOS% = 0 THEN F$=F$+"NORMAL    "
	IF INFPOS% = 1 THEN F$=F$+"CORNERS IN"
	IF INFPOS% = 2 THEN F$=F$+"INFIELD IN"
	GPRINT1VE 27,52,F$,0
	CALL HOLDSTATUS(H0%)
'RUNNER AT SECOND
	I2 = 2: X = 37: CALL DISPRUN(I2, p, X)
	IF O%(0) <> 3 THEN MO = 0
	FOR WK = 1 TO 3
	IF B0%(WK, 0) > 0 THEN MO = MO + 1
	NEXT
'RUNNER AT THIRD
	I2 = 3: X = 15: CALL DISPRUN(I2, p, X)
'RUNNER AT FIRST
	I2 = 1: X = 56: CALL DISPRUN(I2, p, X)
END SUB

SUB NEWLINES ()
IF G4<>0 THEN
	PB%=0
	FOR I5=4 TO 11
	GPRINT0VE I5,20,STRING$(44,32),0
	NEXT
	FOR I5=1 TO 8:G$(I5)="":NEXT
ENDIF
END SUB

SUB NEWPITCHER
	V0 = i
	IF S1%(D) <> -1 AND P6%(D) > 1 THEN I5 = P1%(D): I6 = 22: CALL ADDPIT(D, I5, I6)
	P1%(D) = V0: P6%(D) = P6%(D) + 1: p%(D, V0, 15) = P6%(D): P9 = D: CALL PITSTAM(P9)
	FOR V0 = 1 TO 9
IF B7%(D,V0)=1 OR B7%(D,V0)=10 THEN B3%(D,V0)=P1%(D):B7%(D,V0)=1:B9%(D,1)=P1%(D): P9 = D: W5 = P9: W6 = P9: VV(P9) = V0: VV1(P9) = V0: CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(),B3%(),B4%(),K9!(),WF!)
	NEXT V0
	VV(P9)=1:VV1(P9)=9
	B9%(D, 1) = P1%(D): CALL CHECKSAVE(D, p, S4%)
	W5 = 1 - P9: W6 = 1 - P9: VV(1 - P9) = 1: VV1(1 - P9) = 9
	CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	CALL FLDDISP(BC%, p, U%, D): CALL MAINSCREEN (D, p, BC%, U%, MO): PPH = 0
	CALL NEWLINES
	CALL RANDNUM(rn)
	IF rn <= 10 THEN F$="Hold the phone...": CALL PBP(F$)
	F$=M$(D)+" is making a pitching change":CALL PBP (F$)

8895 pz=1
END SUB

SUB NOJUMP (D)
	Y% = INT(RND(1) * 20) + 1050
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)
	CALL DELAY
END SUB

SUB OFFPIT (P9, I5, I6)
	P8%(P9, I5, I6) = P8%(P9, I5, I6) + 1: P5%(P9, I5, I6) = P5%(P9, I5, I6) + 1
END SUB

SUB OUTATFIRST (W%, D, fbu%, FB2P%)
	CALL PITCHERNAME(W%, D)
	CALL GROUNDERS(W%, D)
	CALL RANDNUM(rn)
	IF W% = 3 AND rn <= 94 THEN CALL PBP ("he takes it to the bag unassisted"): fbu% = 1: GOTO 3745
	IF W% = 3 THEN W% = 1: CALL PITCHERNAME(W%, D): CALL PBP ("He flips it to "+Q$(4)+" covering"): FB2P% = 1: W% = 3
	IF W% = 4 THEN CALL SECOND2FIRST(D)
	IF W% = 5 OR W% = 6 THEN CALL THROW2FIRST(D)

3745 '
END SUB

SUB OUTFIELDERR ()
	'*** outfielders in trouble ***
	CALL RANDNUM(rn)
	IF rn <= 20 THEN F$=Q$(4)+" can't find it"
	IF rn > 20 AND rn <= 40 THEN F$=Q$(4)+" misjudges it"
	IF rn > 40 AND rn <= 60 THEN F$=Q$(4)+" turns the wrong way"
	IF rn > 60 AND rn <= 80 THEN F$=Q$(4)+" loses it"
	IF rn > 80 THEN F$=Q$(4)+" doesn't see it"
	CALL PBP (F$)
END SUB

SUB OUTFLDLOCATION (W%)
	CALL RANDNUM(rn)
	IF W% = 7 OR W% = 8 AND rn > 50 THEN F$=F$+"lcf"
	IF W% = 9 OR W% = 8 AND rn <= 50 THEN F$=F$+"rcf"
END SUB

SUB OUTFLDNAME (W%, D)
	W% = 9: i = INT(RND(1) * 100) + 1
	IF i <= 25 THEN W% = 8
	IF i <= 5 THEN W% = 7
	Q$(4) = b$(D, B9%(D, W%))
END SUB

SUB PICKEDOFF (D, i)
	'*** picked off ***
	CALL NEWLINES
	CALL DELIVERY(D): CALL RANDNUM(rn)
	IF rn <= 33 THEN CALL PBP ("NO, "+p$(D, P1%(D))+" throws to "+ H$(i - 1))
	IF rn > 33 AND rn <= 66 THEN CALL PBP ("quick move to "+ H$(i - 1))
	IF rn > 66 THEN CALL PBP ("pickoff throw to "+ H$(i - 1))
	CALL RANDNUM(rn)
	IF rn <= 50 THEN F$=Q$(i - 1)+" is picked off base!"
	IF rn > 50 THEN F$=Q$(i - 1)+" is caught napping"
	CALL PBP (F$): I8 = INT(RND(1) * 100) + 1
	IF I8 <= 25 THEN CALL PBP ("he can't believe it")
	IF I8 > 25 AND I8 <= 50 THEN CALL PBP ("what a teriffic move by "+p$(D, P1%(D)))
END SUB

SUB PINCHRUNNER (MS%, A1, S4%, I2, p, PR%, BC%, U%, D, MO)
	' *** CHECK FOR PINCH RUNNER ***
	i0% = 0
	IF A1 > 1 OR A1 < -2 OR INNING% < 8 THEN 8999
	I2=1
	IF S4% = 1 OR S4% = 5 THEN I2 = 1
	IF S4% = 2 OR S4% = 4 OR S4% = 6 THEN I2 = 2
	' J REPRESENTS THE SPOT IN THE BATTING ORDER
	J = B0%(I2, 0)
	j1 = i2
	IF J = 0 THEN 8999
	IF MG%(P,771) < > 999 THEN 8000
	FOR I1=772 TO 777
	IF MG%(P,I1) < > -1 AND B%(P,MG%(P,I1),21)=0 THEN I1=MG%(P,I1):GOTO 8001
	MG%(P,I1)=-1
	NEXT
	GOTO 8999

8001 IF B7%(p, J) = 1 THEN 8010
	FOR I = 0 TO 5
	IF MG%(P,I*4+779) = B3%(p, J) THEN 8003
	NEXT
	GOTO 8999

8003 'FOUND A MATCH FOR THE RUNNER ON BASE
	CALL RANDNUM (RN)
	IF INNING%>=MG%(P,I*4+780) AND B%(P,MG%(P,I*4+781),21)=0 AND RN<=MG%(P,I*4+782) THEN B%(P,MG%(P,I*4+781),21)=40+J:GOTO 8993
	GOTO 8999

8010 'PINCH RUNNING FOR THE PITCHER
	IF MG%(P,803)<>999 THEN 8999
	CALL RANDNUM (RN)
	IF P%(P,P1%(P),31)<=MG%(P,804) AND INNING%>=MG%(P,805) AND RN<=MG%(P,806) THEN 8993
	IF P%(P,P1%(P),31)<=MG%(P,807) AND INNING%>=MG%(P,808) AND RN<=MG%(P,809) THEN 8993
	GOTO 8999

8000 IF B7%(p, J) = 1 AND P%(P,P1%(P),31)>4 THEN 8999
	IF B7%(P,J)<>1 AND b%(p, B3%(p, J), 16) > 3 THEN 8999
	I1=-1
	FOR i = 0 TO 22
	IF b$(p, i) = "XXX" OR b%(p, i, 21) > 0 THEN 8992
	FOR I2 = 22 TO 25
	IF B%(P,I,I2) < > -1 AND B%(P,I,I2) = B7%(P,J) AND B7%(P,J) < > 1 THEN I1 = I
	NEXT
	IF b%(p, i, 16) > 5 THEN PR%(i0%) = i: i0% = i0% + 1

8992 NEXT
	IF i0% = 0 OR I1 = -1 AND B7%(P,J) < > 1 THEN 8999
	IF B7%(P,J) < > 1 THEN B%(P,I1,21) = 40 + J
	rn = INT(RND(1) * i0%)
	I1 = PR%(rn)

8993 I2 = J
	q$(j1) = b$(p,i1):b%(p, I1, 21) = 1: B3%(p, I2) = I1: B9%(p, B7%(p, I2)) = I1
	IF B7%(p, I2) = 1 OR B7%(p, I2) = 10 THEN B9%(p, 1) = 99: B9%(p, 0) = I2: B7%(p, I2) = 10
	D0%(p) = D0%(p) + 1: X0%(p, 0, D0%(p)) = B3%(p, I2)
	X0%(p, 1, D0%(p)) = 11:' WAS EQUAL TO B7%(p, I2)
	X0%(p, 2, D0%(p)) = (I2 * 10) + (D0%(p) - 9)
	W5 = p: W6 = p: VV(p) = I2: VV1(p) = I2
	FOR xx = 22 TO 25
	IF B7%(p, I2) = b%(p, I1, xx) THEN b%(p, I1, 15) = b%(p, I1, 33 + (xx - 22) * 2): b%(p, I1, 17) = b%(p, I1, 34 + (xx - 22) * 2): b%(p, I1, 19) = b%(p, I1, xx + 4): GOTO 8995
	NEXT

8995 CALL NEWLINES: CALL PBP (M$(p)+" is sending in a pinch runner"): PR% = 1
	CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	CALL FLDDISP(BC%, p, U%, D)
	CALL MAINSCREEN(D, p, BC%, U%, MO)
	MS% = 1
	VV(P)=1:VV1(P)=9

8999 '
END SUB

SUB PITCHERNAME (W%, D)
	IF W% = 1 THEN Q$(4) = p$(D, P1%(D))
	IF W% <> 1 THEN Q$(4) = b$(D, B9%(D, W%))
END SUB

SUB PITCHOUT (S4%, TGTBASE%, Q0)
	IF S4% = 5 OR TGTBASE% <> 2 THEN 4883: REM*** PITCH OUT ***
	PJ = INT(RND(1) * 100) + 1
	IF PJ<=8 THEN CALL PBP ("...PITCH OUT!!!"): Q0 = Q0 - 200

4883 '
END SUB

SUB PITDISP (P9)
	CLS
	COLOR 14: PRINT "#"; : COLOR l%(P9, 11), l%(P9, 12): PRINT A$(P9); : COLOR 14, 0
	LOCATE , 18: PRINT "T  IP   H   G GS CG SV HR  BB  SO  W- L   ERA  H G%"
	AA = 0: BB = 21
	FOR i = AA TO BB: IF p%(P9, i, 15) > 0 THEN 206
	COLOR 2, 0
	IF i <= 9 THEN PRINT USING "#"; i;
	IF i > 9 THEN PRINT X$(i - 10);
	COLOR 15, 0
	LOCATE , 3: PRINT p$(P9, i); : LOCATE , 18: PRINT B1$(p%(P9, i, 0) + 2);
	PRINT USING " ### ### ### ## ## ## ## ### ### ##"; p%(P9, i, 6); p%(P9, i, 7); p%(P9, i, 4); p%(P9, i, 5); p%(P9, i, 16); p%(P9, i, 3); p%(P9, i, 13); p%(P9, i, 8); p%(P9, i, 9); p%(P9, i, 1); : PRINT "-";
	PRINT USING "##"; p%(P9, i, 2);
	LOCATE , 59: PRINT USING "##.## # ##"; p%(P9, i, 10) / 100; p%(P9, i, 18); p%(P9, i, 12)

206 NEXT i
END SUB

SUB PITOFF (p, I5, I6)
	P8%(p, I5, I6) = P8%(p, I5, I6) - 1: P5%(p, I5, I6) = P5%(p, I5, I6) - 1
END SUB

SUB PITPLACEMENT (p, X$)
	IF p%(p, P1%(p), 0) < 0 THEN GPRINT1VE 26,42,X$,15
	IF p%(p, P1%(p), 0) > 0 THEN GPRINT1VE 26,30,X$,15
END SUB

SUB PITSTAM (P9)
	IF p%(P9, P1%(P9), 4) = 0 THEN S8%(P9, 0) = 3
	IF p%(P9, P1%(P9), 4)<>0 THEN S8%(P9, 0) = INT(((p%(P9, P1%(P9), 8) + p%(P9, P1%(P9), 7)) / p%(P9, P1%(P9), 4)) + .5)
	IF S8%(P9, 0) < 3 THEN S8%(P9, 0) = 3
	S8%(P9, 1) = 3
	IF P6%(P9) > 1 THEN S8%(P9, 1) = 0
	 IF CGERA%>0 THEN S8%(P9,0)=S8%(P9,0)+1
	IF CGERA%>0 AND P6%(P9)>1 THEN S8%(P9, 1) = 1
	 '1955 - SET RUNS=1 AND HIT+BB INCREMENT BY ONE
	 IF INNING% <= 5 AND S6%(P9, 0) - S6%(1 - P9, 0) > 0 THEN P2%(P9) = P1%(P9)
END SUB

SUB PITSTRAT
	A1 = S6%(D, 0) - S6%(p, 0)
	pz=0
	IF MG%(D,60)<>999 AND INNING%<=7 THEN 8195
	IF INNING%>=8 AND MG%(D,520)<>999 AND MG%(D,90)<>999 OR MG%(D,581)<>999 THEN 8195
	IF INNING%<=7 THEN 8115
	  I2=0
	FOR I = 582 TO 587
	I2 = I2 + MG%(D,I)
	NEXT
	I3 = INT(RND(1) * I2) + 1
	I2 = 0
	FOR I=582 TO 587
	I2=I2+MG%(D,I)
	IF I3<=I2 THEN SN%=I:GOTO 8111
	NEXT
	GOTO 8195

8111 SF%=0
	IF A1 < 1 THEN 8195
	IF A1 > 0 AND A1 <= 2 THEN SF%=1
	IF S4% = 0 AND A1 <= 2 THEN SF%=1
	IF S4% > 0 AND S4% < 4 AND A1 <= 3 THEN SF%=1
	IF S4% > 3 AND S4% < 7 AND A1 <= 4 THEN SF%=1
	IF S4% = 7 AND A1 <= 5 THEN SF%=1
	IF S4% = 0 AND O%(0) = 0 AND A1 <= 3 THEN SF%=1
	  IF B1!(P)<=7 THEN FOR I=0 TO 2:I3=I3+B%(P,B3%(P, B1!(P)+I),0):NEXT
	IF B1!(P)=8 THEN FOR I=0 TO 1:I3=I3+B%(P,B3%(P, B1!(P)+I),0):NEXT:I3=I3+B%(P,B3%(P, 1),0)
	IF B1!(P)=9 THEN FOR I=1 TO 2:I3=I3+B%(P,B3%(P, I),0):NEXT:I3=I3+B%(P,B3%(P, 9),0)

8112 IF I3=0 THEN I3=INT(RND(1)*3)-1
	IF I3=0 THEN 8112
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8113
	IF INNING%>=9 AND SF%=1 AND (SN%=582 OR SN%=587) THEN 8113
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>0 AND SN%=583 THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>0 AND SN%=583 THEN 8113
	IF INNING%>=9 AND A1>0 AND SN%=583 THEN 8113
	  IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>=0 AND SN%=584 THEN 8114
	IF INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>=0 AND SN%=584 THEN 8113
	IF INNING%>=9 AND A1>=0 AND SN%=584 THEN 8113
	  IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>0 AND SN%=585 THEN 8114
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>0 AND SN%=585 THEN 8113
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%>=9 AND A1>0 AND SN%=585 THEN 8113
	  IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=0 AND A1>=0 AND SN%=586 THEN 8114
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%=8 AND MG%(D,588)=8 AND MG%(D,589)=1 AND A1>=0 AND SN%=586 THEN 8113
	IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) AND INNING%>=9 AND A1>=0 AND SN%=586 THEN 8113
	  GOTO 8195

8114
	I2=0
	IF I3<0 THEN I1=81
	IF I3>0 THEN I1=84
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3 = INT (RND(1) * I2) + 1 : I2 = 0
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER:SU%(D,0)=-1: GOTO 8195
	NEXT
	'NO SETUP MAN FOUND
	GOTO 8195

8113
	I2=0
	IF I3<0 THEN I1=521
	IF I3>0 THEN I1=524
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3=INT(RND(1)*I2)+1
	I2=0
	FOR I=I1 TO I1+2
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER:SU%(D,1)=-1:GOTO 8195
	NEXT
	'NO CLOSER FOUND
	'GOTO 8195
	SU%(D,0)=-1:SU%(D,1)=-1

8115 I2=0
	FOR I=61 TO 70
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	NEXT
	I3=INT(RND(1)*I2)+1
	I2=0
	FOR I=61 TO 70
	IF MG%(D,I)>=0 AND P%(D,MG%(D,I),15)=0 THEN I2=I2+MG%(D,I+10)
	IF I3<=I2 THEN V0=MG%(D,I):MG%(D,I)=-1:I=V0:CALL NEWPITCHER: GOTO 8195
	NEXT
	'NO MIDDLE RELIEVER FOUND
	GOTO 8195

8195 '
END SUB

SUB PITSWAP (i)
	IF i = 4 THEN V = 3: CALL SOMESTATS(V, p, F%, D): V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
	IF i = 3 THEN V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
	IF i = 2 THEN V = 2: CALL SOMESTATS(V, p, F%, D): V = 1: CALL SOMESTATS(V, p, F%, D)
END SUB

SUB PLAYERNAME (p)
	IF B7%(p, B1!(p)) = 1 THEN Q$(0) = p$(p, P1%(p)): GOTO 8002
	Q$(0) = b$(p, B3%(p, B1!(p)))

8002 FOR i = 1 TO 3
	IF B0%(i, 0) > 0 AND B7%(p, B0%(i, 0)) <> 1 THEN Q$(i) = b$(p, B3%(p, B0%(i, 0)))
	IF B0%(i, 0) > 0 AND B7%(p, B0%(i, 0)) = 1 THEN Q$(i) = p$(p, P1%(p))
	NEXT i
END SUB

SUB PLYRHRT (P9, W5, W6, BC%, U%, D, MO)
	'*** Offensive Player Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN 2805
	CALL PBP ("You must replace "+Q$(0)+" with a pinch-hitter"):GOTO 2849

2805 FOR i = 0 TO 22: IF b%(p, i, 21) > 0 OR b$(p, i) = "XXX" THEN 2808
	IF B7%(p, B1!(p)) <> b%(p, i, 22) AND B7%(p, B1!(p)) <> b%(p, i, 23) AND B7%(p, B1!(p)) <> b%(p, i, 24) AND B7%(p, B1!(p)) <> b%(p, i, 25) THEN 2808

2807 CALL PBP (Q$(0)+" will be replaced by "+b$(p, i))
	D0%(p) = D0%(p) + 1: X0%(p, 0, D0%(p)) = i: X0%(p, 1, D0%(p)) = B7%(p, B1!(p)): X0%(p, 2, D0%(p)) = (B1!(p) * 10) + (D0%(p) - 9)
	b%(p, i, 21) = 1: B3%(p, B1!(p)) = i: B9%(p, B7%(p, B1!(p))) = i: IF B7%(p, B1!(p)) = 1 OR B7%(p, B1!(p)) = 10 THEN B9%(p, 1) = 99: B9%(p, 0) = B1!(p): B7%(p, B1!(p)) = 10
	P9 = p: W5 = P9: W6 = P9: VV(P9) = B1!(p): VV1(P9) = B1!(p)
	VV(P9)=1:VV1(P9)=9
	Q$(0) = b$(p, B3%(p, B1!(p)))
	IF B7%(p, B1!(p)) = 1 THEN Q$(0) = p$(p, P1%(D))
	GOTO 2849

2808 NEXT i
	FOR i = 0 TO 22: IF b%(p, i, 21) > 0 OR b$(p, i) = "XXX" THEN 2810
	GOTO 2807

2810 NEXT i

2849 '
END SUB

SUB POPUP ()
	' *** POPUP LINEUPS FOR TEAMS ***
	CALL SCREEN0(0)
	J = 11
	LOCATE 8, 7: COLOR l%(1, 11), l%(1, 12): PRINT " Ö"; STRING$(62, 196); "· ";
	FOR II = 1 TO J: LOCATE 8 + II, 7: PRINT " º"; STRING$(62, 32); "º "; : NEXT
	LOCATE 9 + J, 7: PRINT " Ó"; STRING$(62, 196); "½ ";
	LOCATE 9, 14: PRINT A$(0); TAB(40); A$(1)
	FOR J = 0 TO 1
	FOR X = 1 TO 9: LOCATE X + 9, 9 + J * 26
	IF B1!(J) = X THEN PRINT "->";
	LOCATE , 11 + J * 26
	IF B7%(J, X) <> 1 THEN 841
	PRINT B1$(p%(J, P1%(J), 0) + 2); TAB(14 + J * 26); LEFT$(p$(J, P1%(J)), 10); " p"; TAB(28 + J * 26);
	IF p%(J, P1%(J), 11) <> 999 THEN PRINT USING " .###"; p%(J, P1%(J), 11) / 1000: GOTO 842
	IF p%(J, P1%(J), 20) = 0 THEN PRINT " .000": GOTO 842
	IF p%(J, P1%(J), 20) = p%(J, P1%(J), 22) THEN PRINT "1.000": GOTO 842
	PRINT USING " .###"; p%(J, P1%(J), 22) / p%(J, P1%(J), 20)
	GOTO 842

841 PRINT B1$(b%(J, B3%(J, X), 0) + 2); TAB(14 + J * 26); LEFT$(b$(J, B3%(J, X)), 10); " "; C$(B7%(J, X)); TAB(28 + J * 26);
	IF b%(J, B3%(J, X), 4) = 0 THEN PRINT " .000": GOTO 842
	IF b%(J, B3%(J, X), 6) = b%(J, B3%(J, X), 4) THEN PRINT "1.000": GOTO 842
	PRINT USING " .###"; b%(J, B3%(J, X), 6) / b%(J, B3%(J, X), 4)

842 NEXT: NEXT: COLOR 15, 0
843 i$ = GetKeyPress$
END SUB

SUB POPUPBAT (WK, M)
	IF TS%(WK,0)=999 THEN LOCATE 4,56:PRINT "VS LHP  .000";
	IF TS%(WK,0)=999 AND B%(WK,M,42)<>0 THEN LOCATE 4,64:PRINT USING ".###";B%(WK,M,43)/B%(WK,M,42)
	IF TS%(WK,0)=999 THEN LOCATE 5,56:PRINT "VS RHP  .000";
	IF TS%(WK,0)=999 AND B%(WK,M,49)<>0 THEN LOCATE 5,64:PRINT USING ".###";B%(WK,M,50)/B%(WK,M,49)
	LOCATE 8, 6: PRINT "SEASON"; : LOCATE , 15
	IF b%(WK, M, 4) = 0 THEN PRINT ".---  .---":GOTO 734
	PRINT USING ".###  .### ### ### ### ### ## ## ## ### ### ### "; b%(WK, M, 6) / b%(WK, M, 4); (b%(WK, M, 6) + b%(WK, M, 11)) / (b%(WK, M, 4) + b%(WK, M, 11)); b%(WK, M, 3); b%(WK, M, 4); b%(WK, M, 5); b%(WK, M, 6); b%(WK, M, 7); b%(WK, M, 8); b%( _
WK, M, 9); b%(WK, M, 10);
	PRINT USING "### ### ### ##"; b%(WK, M, 11); b%(WK, M, 12); b%(WK, M, 13); b%(WK, M, 14)

734 LOCATE , 6: PRINT "LEAGUE";
	IF B5%(WK, M, 0) = 0 OR B5%(WK, M, 1) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 735
	IF B5%(WK, M, 0) = B5%(WK, M, 1) THEN LOCATE , 14: PRINT "1.000"; : GOTO 735
	LOCATE , 15: PRINT USING ".###"; B5%(WK, M, 1) / B5%(WK, M, 0);

735 IF B5%(WK, M, 0) + B5%(WK, M, 7) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 736
	IF B5%(WK, M, 1) + B5%(WK, M, 7) = B5%(WK, M, 0) + B5%(WK, M, 7) THEN LOCATE , 20: PRINT "1.000"; : GOTO 736
	LOCATE , 21: PRINT USING ".###"; (B5%(WK, M, 1) + B5%(WK, M, 7)) / (B5%(WK, M, 0) + B5%(WK, M, 7));

736 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B5%(WK, M, 14); B5%(WK, M, 0); B5%(WK, M, 2); B5%(WK, M, 1); B5%(WK, M, 4); B5%(WK, M, 5); B5%(WK, M, 6); B5%(WK, M, 3); B5%(WK, M, 7); B5%(WK, M, 8); B5%(WK, M, 9);  _
B5%(WK, M, 10); B5%(WK, M, 12); B5%(WK, M, 13)
	LOCATE , 6: PRINT "GAME";
	IF B8%(WK, M, 0) = 0 OR B8%(WK, M, 1) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 737
	IF B8%(WK, M, 0) = B8%(WK, M, 1) THEN LOCATE , 14: PRINT "1.000"; : GOTO 737
	LOCATE , 15: PRINT USING ".###"; B8%(WK, M, 1) / B8%(WK, M, 0);

737 IF B8%(WK, M, 0) + B8%(WK, M, 7) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 738
	IF B8%(WK, M, 1) + B8%(WK, M, 7) = B8%(WK, M, 0) + B8%(WK, M, 7) THEN LOCATE , 20: PRINT "1.000"; : GOTO 738
	LOCATE , 21: PRINT USING ".###"; (B8%(WK, M, 1) + B8%(WK, M, 7)) / (B8%(WK, M, 0) + B8%(WK, M, 7));

738 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; B8%(WK, M, 14); B8%(WK, M, 0); B8%(WK, M, 2); B8%(WK, M, 1); B8%(WK, M, 4); B8%(WK, M, 5); B8%(WK, M, 6); B8%(WK, M, 3); B8%(WK, M, 7); B8%(WK, M, 8); B8%(WK, M, 9);  _
B8%(WK, M, 10); B8%(WK, M, 12); B8%(WK, M, 13)
END SUB

SUB POPUPPIT (WK, M)
	LOCATE 14, 3: : COLOR l%(WK, 11), l%(WK, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	IF TS%(WK,0)=999 THEN LOCATE 15,56:PRINT "VS LHB  .000";
	IF TS%(WK,0)=999 AND P%(WK,M,51)<>0 THEN LOCATE 15,64:PRINT USING ".###";P%(WK,M,52)/P%(WK,M,51)
	IF TS%(WK,0)=999 THEN LOCATE 16,56:PRINT "VS RHB  .000";
	IF TS%(WK,0)=999 AND P%(WK,M,58)<>0 THEN LOCATE 16,64:PRINT USING ".###";P%(WK,M,59)/P%(WK,M,58)
	LOCATE 15, 6: PRINT "Stats for "; p$(WK, M)
	LOCATE , 6: PRINT "THROWS: "; B1$(p%(WK, M, 0) + 2); "    "; : PRINT USING "G/A%: ##     HOLD: #"; p%(WK, M, 12); p%(WK, M, 18)
	LOCATE , 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          W  L SV   ERA  G GS CG   IP    H  BB  SO   R  ER HR"
	LOCATE , 6: PRINT "SEASON   "; : PRINT USING "## ## ## ##.## ## ## ## ###   ### "; p%(WK, M, 1); p%(WK, M, 2); p%(WK, M, 3); p%(WK, M, 10) / 100; p%(WK, M, 4); p%(WK, M, 5); p%(WK, M, 16); p%(WK, M, 6); p%(WK, M, 7);
	PRINT USING "### ###         ##"; p%(WK, M, 8); p%(WK, M, 9); p%(WK, M, 13)
	LOCATE , 6: PRINT "LEAGUE   "; : PRINT USING "## ## ## "; P5%(WK, M, 6); P5%(WK, M, 7); P5%(WK, M, 12);
	IF P5%(WK, M, 3) = 0 OR P5%(WK, M, 0) = 0 THEN PRINT " 0.00"; : GOTO 805
	PRINT USING "##.##"; P5%(WK, M, 3) / (P5%(WK, M, 0) / 3) * 9;

805 PRINT USING " ## ## ## "; P5%(WK, M, 8); P5%(WK, M, 9); P5%(WK, M, 10);
	IF P5%(WK, M, 0) = 0 THEN PRINT "  0.0"; : GOTO 806
	PRINT USING "###"; INT(P5%(WK, M, 0) / 3);
	PRINT "."; : PRINT USING "#"; P5%(WK, M, 0) - (INT(P5%(WK, M, 0) / 3) * 3);

806 PRINT USING " ### ### ### ### ### ##"; P5%(WK, M, 1); P5%(WK, M, 4); P5%(WK, M, 5); P5%(WK, M, 2); P5%(WK, M, 3); P5%(WK, M, 14)
	LOCATE , 6: PRINT "GAME     "; : PRINT USING "## ## ## "; P8%(WK, M, 6); P8%(WK, M, 7); P8%(WK, M, 12);
	IF P8%(WK, M, 3) = 0 OR P8%(WK, M, 0) = 0 THEN PRINT " 0.00"; : GOTO 807
	PRINT USING "##.##"; P8%(WK, M, 3) / (P8%(WK, M, 0) / 3) * 9;

807 PRINT USING " ## ## ## "; P8%(WK, M, 8); P8%(WK, M, 9); P8%(WK, M, 10);
	IF P8%(WK, M, 0) = 0 THEN PRINT "  0.0"; : GOTO 808
	PRINT USING "###"; INT(P8%(WK, M, 0) / 3);
	PRINT "."; : PRINT USING "#"; P8%(WK, M, 0) - (INT(P8%(WK, M, 0) / 3) * 3);

808 PRINT USING " ### ### ### ### ### ##"; P8%(WK, M, 1); P8%(WK, M, 4); P8%(WK, M, 5); P8%(WK, M, 2); P8%(WK, M, 3); P8%(WK, M, 14)
END SUB

SUB POPUPSTATS ()
	CALL SCREEN0(0)
	J = B3%(p, B1!(p))
	LOCATE 3, 3: COLOR l%(p, 11), l%(p, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	LOCATE 4, 6: PRINT "Stats for ";
	IF B7%(p, B1!(p)) = 1 AND p%(p, P1%(p), 11) <> 999 THEN PRINT p$(p, P1%(p)): GOTO 510
	IF B7%(p, B1!(p)) = 1 THEN PRINT p$(p, P1%(p)): LOCATE , 6: PRINT "BATS: "; B1$(p%(p, P1%(p), 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; 52; p%(p, P1%(p), 32); p%(p, P1%(p), 31): GOTO 510
	PRINT b$(p, J): LOCATE , 6: PRINT "BATS: "; B1$(b%(p, J, 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; b%(p, J, 18); b%(p, J, 30); b%(p, J, 16)

510 IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 THEN LOCATE 4,56:PRINT "VS LHP  .000";
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 AND P%(P,P1%(P),37)<>0 THEN LOCATE 4,64:PRINT USING ".###";P%(P,P1%(P),38)/P%(P,P1%(P),37)
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 THEN LOCATE 5,56:PRINT "VS RHP  .000";
	IF B7%(P, B1!(P)) = 1 AND TS%(P,0)=999 AND P%(P,P1%(P),44)<>0 THEN LOCATE 5,64:PRINT USING ".###";P%(P,P1%(P),45)/P%(P,P1%(P),44)
	LOCATE 6, 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
	IF B7%(p, B1!(p)) = 1 AND p%(p, P1%(p), 11) <> 999 THEN LOCATE , 6: PRINT "SEASON"; : LOCATE , 15: PRINT USING ".###"; p%(p, P1%(p), 11) / 1000: GOTO 520
	IF B7%(p, B1!(p)) = 1 THEN 517
	WK = p
	M = J
	CALL POPUPBAT(WK, M)
	GOTO 621

517 LOCATE , 6: PRINT "SEASON"; : LOCATE , 15
	IF p%(p, P1%(p), 20) = 0 THEN PRINT ".---  .---":GOTO 520
	PRINT USING ".###  .### ### ### ### ### ## "; p%(p, P1%(p), 22) / p%(p, P1%(p), 20); (p%(p, P1%(p), 22) + p%(p, P1%(p), 27)) / (p%(p, P1%(p), 27) + p%(p, P1%(p), 20)); p%(p, P1%(p), 19); p%(p, P1%(p), 20); p%(p, P1%(p), 21); p%(p, P1%(p), 22);  _
p%(p, P1%(p), 23);
	PRINT USING "## ## ### ### ### ### ##"; p%(p, P1%(p), 24); p%(p, P1%(p), 25); p%(p, P1%(p), 26); p%(p, P1%(p), 27); p%(p, P1%(p), 28); p%(p, P1%(p), 29); p%(p, P1%(p), 30)

520 LOCATE , 6: PRINT "LEAGUE"; : J = P1%(p)
	IF P5%(p, J, 23) = 0 OR P5%(p, J, 24) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 521
	IF P5%(p, J, 23) = P5%(p, J, 24) THEN LOCATE , 14: PRINT "1.000"; : GOTO 521
	LOCATE , 15: PRINT USING ".###"; P5%(p, J, 24) / P5%(p, J, 23);

521 IF P5%(p, J, 23) + P5%(p, J, 30) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 522
	IF P5%(p, J, 24) + P5%(p, J, 30) = P5%(p, J, 23) + P5%(p, J, 30) THEN LOCATE , 20: PRINT "1.000"; : GOTO 522
	LOCATE , 21: PRINT USING ".###"; (P5%(p, J, 24) + P5%(p, J, 30)) / (P5%(p, J, 23) + P5%(p, J, 30));

522 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P5%(p, J, 8); P5%(p, J, 23); P5%(p, J, 25); P5%(p, J, 24); P5%(p, J, 27); P5%(p, J, 28); P5%(p, J, 29); P5%(p, J, 26); P5%(p, J, 30); P5%(p, J, 31); P5%(p, J, 32);  _
P5%(p, J, 33); P5%(p, J, 35); P5%(p, J, 36)
	LOCATE , 6: PRINT "GAME";
	IF P8%(p, J, 23) = 0 OR P8%(p, J, 24) = 0 THEN LOCATE , 15: PRINT ".000"; : GOTO 523
	IF P8%(p, J, 23) = P8%(p, J, 24) THEN LOCATE , 14: PRINT "1.000"; : GOTO 523
	LOCATE , 15: PRINT USING ".###"; P8%(p, J, 24) / P8%(p, J, 23);

523 IF P8%(p, J, 23) + P8%(p, J, 30) = 0 THEN LOCATE , 21: PRINT ".000"; : GOTO 524
	IF P8%(p, J, 24) + P8%(p, J, 30) = P8%(p, J, 23) + P8%(p, J, 30) THEN LOCATE , 20: PRINT "1.000"; : GOTO 524
	LOCATE , 21: PRINT USING ".###"; (P8%(p, J, 24) + P8%(p, J, 30)) / (P8%(p, J, 23) + P8%(p, J, 30));

524 LOCATE , 26: PRINT USING "### ### ### ### ## ## ## ### ### ### ### ## ## ##"; P8%(p, J, 8); P8%(p, J, 23); P8%(p, J, 25); P8%(p, J, 24); P8%(p, J, 27); P8%(p, J, 28); P8%(p, J, 29); P8%(p, J, 26); P8%(p, J, 30); P8%(p, J, 31); P8%(p, J, 32);  _
P8%(p, J, 33); P8%(p, J, 35); P8%(p, J, 36)

621 WK = D
	M = P1%(D)
	CALL POPUPPIT(WK, M)
650 i$ = GetKeyPress$
	COLOR 7, 0
END SUB

SUB PUTOUT (i, D, p)
	IF B7%(p, B0%(i - 1, 0)) = 1 THEN I5 = P1%(p): I6 = 33: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B0%(i - 1, 0)) <> 1 THEN I5 = B3%(p, B0%(i - 1, 0)): I6 = 10: CALL INCBATOFF(P, I5, I6)
END SUB

SUB RANDNUM (rn)
	rn = INT(RND(1) * 100) + 1
END SUB

SUB RecordOut (i, I3)
	CALL PBP (Q$(i)+" out @ "+H$(I3))
	B0%(i, 0) = 0: B0%(i, 1) = 0: B0%(i, 2) = 0
	Q$(i) = ""
END SUB

SUB RecordRun (sah%, i, D, F%, PQ, S2%, W%, INFPOS%)
	IF sah% = 0 THEN CALL PBP (Q$(i)+" scores")
	sah% = 0
	I5 = B0%(i, 1): I6 = 2: CALL ADDPIT (D, I5, I6)
	IF P1%(D) <> B0%(i, 1) THEN I5 = P1%(D): I6 = 18: CALL ADDPIT (D, I5, I6)
	IF P6%(p) = 1 THEN P8%(p, P1%(p), 20) = P8%(p, P1%(p), 20) + 1
	IF B7%(p, B0%(i, 0)) = 1 THEN I5 = P1%(p): I6 = 25: CALL INCPITOFF (P, I5, I6)
	IF B7%(p, B0%(i, 0)) <> 1 THEN I5 = B3%(p, B0%(i, 0)): I6 = 2: CALL INCBATOFF (P, I5, I6)
	IF F% = 1 AND O%(0) = 2 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% <= 3 AND S2% <> 23 OR F% = 1 AND W% = 5 AND S2% <> 23 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% = 4 AND INFPOS% = 2 THEN PQ = 8: GOTO 4220
	IF F% = 1 AND W% = 6 AND INFPOS% = 2 THEN PQ = 8: GOTO 4220
	IF S2% >= 4 AND S2% <= 6 THEN PQ = 8: GOTO 4220

4220 '
	IF PQ = 8 THEN 1825
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 26: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 3: CALL INCBATOFF (P, I5, I6)
	IF F% = 1 THEN PQ = 8

1825 s%(p, INNING%) = s%(p, INNING%) + 1: S6%(p, 0) = S6%(p, 0) + 1
	IF S6%(p, 0) - S6%(D, 0) = 1 THEN P2%(D) = B0%(i, 1): P2%(p) = P1%(p): S1%(1) = -1: S1%(0) = -1: GOTO 1155
	IF S6%(p, 0) - S6%(D, 0) = 0 AND S1%(D) <> -1 THEN I5 = P1%(D): I6 = 19: CALL ADDPIT(D, I5, I6): S1%(D) = -1

1155 B0%(i, 0) = 0: B0%(i, 1) = 0: B0%(i, 2) = 0: Q$(i) = ""
	IF s%(p, INNING%) = 4 AND INT(RND(1)*100)+1<=25 THEN CALL PBP ("They're tearin' up the peapatch!")
	IF INNING% >= 9 AND p = 1 AND S6%(p, 0) - S6%(D, 0) = 1 AND F% = 0 AND O%(1) < 3 AND S2% <> 10 THEN CALL CHARGERUN (D,i)
	IF INNING% >= 9 AND p = 1 AND S6%(p, 0) - S6%(D, 0) = 1 AND S2% <> 10 THEN A5%(0) = 0: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
END SUB

SUB INJURYLENGTH (D, po%)
	IJL% = IJL% + 1
	IJ$(IJL%) = b$(D, B9%(D, po%))
	F$=b$(D, B9%(D, po%))+" is injured for":CALL PBP (F$)
	'*** Determine Injury Length ***
	il = 0
	IF b%(D, B9%(D, po%), 3) = 162 THEN F$= " remainder of game": GOTO 2115
	GOSUB 2116
	IF IL=0 THEN 2115
	IF b%(D, B9%(D, po%), 3) = 161 THEN F$= " this game and next game": il = 1: GOTO 2115
	il = INT(RND(1) * (162 - b%(D, B9%(D, po%), 3))) + 1
	F$ = STR$(il) + " games"

2115 CALL PBP (F$)
	b%(D, B9%(D, po%), 32) = b%(D, B9%(D, po%), 32) + il
	CALL DELAY: CALL DELAY
	CALL EJECTIONINJURY(D, po%)
	GOTO 2120

2116 ct = 0
	FOR I = 0 TO 22
	FOR I1 = 22 TO 25
	IF B%(D, I, I1) = PO% AND B%(D, I, 32) = 0 AND I <> B9%(D, po%) THEN ct = ct + 1
	if ct > 3 then il = 1:goto 2117
	NEXT
	NEXT
	F$= " remainder of game":IL=0

2117 RETURN

2120
END SUB

SUB EJECTIONINJURY (D, po%)
	'*** Defensive Player Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN 2704
	F$="You must replace "+b$(D, B9%(D, po%)): CALL PBP (F$): GOTO 2749

2704 FOR I2 = 1 TO 9
	IF B7%(D, I2) = po% THEN 2705
	NEXT

2705 FOR i = 0 TO 22
	IF b%(D, i, 21) > 0 OR b$(D, i) = "XXX" THEN 2708
	IF B7%(D, I2) <> b%(D, i, 22) AND B7%(D, I2) <> b%(D, i, 23) AND B7%(D, I2) <> b%(D, i, 24) AND B7%(D, I2) <> b%(D, i, 25) THEN 2708

2707 F$=b$(D, B9%(D, po%))+" will be replaced by "+b$(D, i): CALL PBP (F$)
	D0%(D) = D0%(D) + 1
	X0%(D, 0, D0%(D)) = i
	X0%(D, 1, D0%(D)) = B7%(D, I2)
	X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)
	b%(D, i, 21) = 1
	B3%(D, I2) = i
	I7% = D: I8% = I2: i9% = i
	CALL REPLACEMENTS (I7%, I8%, i9%)
	B9%(D, B7%(D, I2)) = i
	P9 = D: W5 = P9: W6 = P9
	VV(P9)=1:VV1(P9)=9
	GOTO 2749

2708 NEXT i
	FOR i = 0 TO 22
	IF b%(D, i, 21) > 0 OR b$(D, i) = "XXX" THEN 2710
	GOTO 2707

2710 NEXT i

2749 '
END SUB

SUB INJPIT (p, D, IP%)
	'*** Defensive Pitcher Ejected/Injured ***
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN 2755
	F$="You must replace "+p$(D, P1%(D)): CALL PBP (F$): GOTO 2799

2755 IP%=1:'CALL RELIEVERS

2799 '
END SUB

SUB OPTIONS ()
DIM u6$(3)
	CALL SCREEN0(0)
	U6$(0) = "HUMAN OPPONENT      "
	U6$(1) = "COMPUTER VISIT TEAM "
	U6$(2) = "COMPUTER HOME TEAM  "
	U6$(3) = "COMPUTER VS COMPUTER"

IF U6 = 2 THEN U6 = 3:GOTO 170
IF U6 = 1 AND U9 = 1 THEN U6 = 2:GOTO 170
IF U6 = 1 AND U9 = 0 THEN U6 = 1

170 CLS

171 COLOR , 0: LOCATE 3, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT U6$(U6)
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT USING "DISPLAY INTERVAL: #.# "; G4
	COLOR 14: PRINT "<ENTER> ";:COLOR 15:PRINT "RETURN TO GAME PLAY"

183 COLOR 7, 0
	I$=GetKeyPress$
	I = VAL(I$)
	IF I < 0 OR I > 2 THEN 183
	if i = 0 then 184
	IF I = 1 THEN U6 = U6 + 1
	IF U6 > 3 THEN U6 = 0
	IF I = 1 THEN 171
	IF I = 2 THEN LOCATE 4, 22: INPUT G4: GOTO 171

184 IF U6 = 0 THEN 185
IF U6 = 3 THEN U6 = 2:GOTO 185
IF U6 = 2 THEN U6 = 1:u9 = 1:GOTO 185
U6 = 1:u9 = 0

185 '
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
