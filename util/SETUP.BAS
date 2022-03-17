'$INCLUDE: 'COMMON.GW'
'$INCLUDE: 'GWDECL.BAS'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE SUB LRADJ (A!, A1!)
DECLARE SUB NORMALIZE (I7!, I2!, I6!, K8!)
DECLARE SUB ADDPIT (D,I5,I6)
DECLARE SUB TAGADVANCE (S2%, I8, D)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB SINGLEPBP (S2%, D, W%)
DECLARE SUB INCPITOFF (P,I5,I6)
DECLARE SUB DISPBAVG (D)
DECLARE SUB PBP (F$)
DECLARE SUB GETSTATS ()
DECLARE SUB CONVNAME (QQ,I)

TYPE PBP
	YY AS STRING * 30
END TYPE

DIM K9(1) AS SINGLE
DIM A$(1), l%(1, 13), T1(1), b%(1, 22, 80), p%(1, 21, 89), b$(1, 22), p$(1, 21), H$(4)
DIM P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
DIM S%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
DIM VV(1), VV1(1), D3%(1), M%(9), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
DIM X0%(1, 2, 23), D0%(1), TP%(11, 2), L0%(1), T3%(22),AP%(2)
DIM P5%(1, 21, 41), B5%(1, 22, 21), P8%(1, 21, 41), B8%(1, 22, 21)
DIM TS%(1,11),SA%(1,24)
DIM CSS%(1,22,6),SU%(1,1)
DIM NG%(18), YN$(1), Q$(23), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
DIM H0%(1), CZ%(1), BP(1), BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2)
DIM PR%(24), GK%(1),MG%(1,999),VA%(26)

COMMON SHARED TS%(),SA%(),P8%(), B8%(), p%(), p$(), b$(), P5%(), B5%(), X0%(), b%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED S%(), P1%(), S1%(), D$(), S6%(), l%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), H$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED S$, p, U6, DH%, U9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, AP%,INNING%,BUCK AS PBP
COMMON SHARED MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%

DATA first,second,third,short
DATA 6,12,37,55,66,74,86,96,100
DATA 5,10,32,55,67,75,86,96,100
DATA 4,8,27,54,68,76,86,96,100
DATA 3,7,25,54,69,76,86,96,100
DATA 3,6,22,54,69,77,86,96,100
DATA 2,5,20,53,69,78,86,96,100
DATA 2,4,17,53,70,78,86,96,100
DATA 1,3,12,52,71,79,86,96,100
DATA 1,2,7,51,72,80,86,96,100
DATA 6,14,42,55,63,74,86,96,100
DATA 5,12,37,55,64,75,86,96,100
DATA 4,10,32,55,65,76,86,96,100
DATA 3,8,30,55,66,77,86,96,100
DATA 3,7,27,55,66,77,86,96,100
DATA 3,6,24,55,66,77,86,96,100
DATA 2,5,22,55,67,78,86,96,100
DATA 1,4,17,55,68,79,86,96,100
DATA 1,3,12,55,69,80,86,96,100
DATA 0,0,100
DATA 0,15,100
DATA 0,30,100
DATA 0,45,100
DATA 0,60,100
DATA 1,75,100
DATA 3,90,100
DATA 4,97,100
DATA 100,0,90,100,75,100,60,100,45,100,30,100,15,100,8,100
DATA 100,0,0,90,100,0,70,95,100,50,80,100,30,65,100,10,50,100
DATA 0,90,100,5,87,100,10,85,100,15,80,100,20,77,100,25,75,100
DATA 30,73,100,35,70,100,40,67,100,45,65,100,50,63,100,60,65,100

SUB CONVNAME (QQ,I)
	N$(I) = A$(I)
	FOR xx = 4 TO 11
	IF MID$(N$(I), xx, 1) = " " OR MID$(N$(I), xx, 1) = "." THEN MID$(N$(I), xx, 1) = "_"
	NEXT xx
	QQ = 8
	FOR xx = 11 TO 4 STEP -1
	IF MID$(N$(I), xx, 1) = "_" THEN QQ = xx - 4: GOTO 118
	GOTO 119

118 NEXT xx

119 '
END SUB

SUB DISPBAVG (D)
	IF INNING% > 5 AND S8%(D, 0) < P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) < P8%(D, P1%(D), 2) THEN LOCATE 10, 17: COLOR 30, 0: PRINT "-T-": COLOR 15, 0
	LOCATE 5, 1
	FOR X = 1 TO 9: COLOR 3, 0: LOCATE X + 4, 22
	IF B1!(p) = X THEN COLOR 11, 0
	LOCATE , 22: IF B7%(p, X) <> 1 THEN 358
	PRINT B1$(p%(p, P1%(p), 0) + 2); TAB(25); LEFT$(p$(p, P1%(p)), 10); " p"; TAB(39);
	IF p%(p, P1%(p), 11) <> 999 THEN PRINT USING " .###"; p%(p, P1%(p), 11) / 1000: GOTO 359
	IF p%(p, P1%(p), 20) = 0 THEN PRINT " .000": GOTO 359
	IF p%(p, P1%(p), 20) = p%(p, P1%(p), 22) THEN PRINT "1.000": GOTO 359
	PRINT USING " .###"; p%(p, P1%(p), 22) / p%(p, P1%(p), 20)
	GOTO 359

358 PRINT B1$(b%(p, B3%(p, X), 0) + 2); TAB(25); LEFT$(b$(p, B3%(p, X)), 10); " "; C$(B7%(p, X)); TAB(39);
	IF b%(p, B3%(p, X), 4) = 0 THEN PRINT " .000": GOTO 359
	IF b%(p, B3%(p, X), 6) = b%(p, B3%(p, X), 4) THEN PRINT "1.000": GOTO 359
	PRINT USING " .###"; b%(p, B3%(p, X), 6) / b%(p, B3%(p, X), 4)

359 NEXT: COLOR 15, 0
END SUB

SUB GETSTATS ()
	I = 0

100 COLOR 15, 0: CLS : CALL CONVNAME (QQ,I): F$ = MID$(N$(I), 4, QQ): R$ = F$ + "." + LEFT$(N$(I), 2)

102 FILENAME$ = D$(1) + R$
	IF NOT _FILEEXISTS(FILENAME$) THEN 110
OPEN D$(1) + R$ FOR INPUT AS #1
	INPUT #1, A$: INPUT #1, W2%: INPUT #1, L2%
	FOR I1 = 0 TO 22: INPUT #1, K$: INPUT #1, K%: NEXT I1
	FOR I1 = 0 TO 21: INPUT #1, K$: INPUT #1, K%: NEXT I1
	FOR I1 = 0 TO 22: FOR I2 = 0 TO 21: INPUT #1, B5%(I, I1, I2): NEXT: NEXT
	FOR I1 = 0 TO 21: FOR I2 = 0 TO 41: INPUT #1, P5%(I, I1, I2): NEXT: NEXT

110 CLOSE #1
	IF I = 0 THEN I = 1: GOTO 100
END SUB

SUB HITTINGLINES (l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
	RANDOMIZE TIMER
	'CLS : PRINT "Please Wait..."
	'PRINT
	'PRINT "Calculating Pitcher/Batter Park Effects..."
	 FOR I = 0 TO 1
	I1 = 1 - I

'CALCULATE BASE ON BALLS PERCENTAGES
	 FOR I3 = VV(I) TO VV1(I)
	I6! = l%(I1, 3) / 1000
	IF p%(I1, P1%(I1), 6) * 3 * .94 + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8)=0 THEN I2!=0:GOTO 221
	I2! = p%(I1, P1%(I1), 8) / (p%(I1, P1%(I1), 6) * 3 * .94 + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8))
	'I2! = p%(I1, P1%(I1), 8) / (((p%(I1, P1%(I1), 6) * 3) * .94) + p%(I1, P1%(I1), 7) + p%(I1, P1%(I1), 8))

221 GOSUB 500
	 IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 3) / 1000:I2! = P%(I1, P1%(I1),56+PY%*7)*.9/ (P%(I1, P1%(I1),51+PY%*7) + P%(I1, P1%(I1),56+PY%*7)*.9)
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = .037: GOTO 223
	IF B7%(I, I3) = 1 AND (p%(I, P1%(I), 27) + p%(I, P1%(I), 20) = 0) THEN I7! = 0: GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 AND P%(I, P1%(I),37+BY%*7) + P%(I, P1%(I),42+BY%*7) = 0 THEN I7! = 0: GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)<>999 THEN I7! = p%(I, P1%(I), 27) / (p%(I, P1%(I), 27) + p%(I, P1%(I), 20)): GOTO 223
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 THEN I7! = P%(I, P1%(I),42+BY%*7)*.9 / (P%(I, P1%(I),42+BY%*7)*.9 + P%(I, P1%(I),37+BY%*7)): GOTO 223
	 IF b%(I, B3%(I, I3), 4) + b%(I, B3%(I, I3), 11) = 0 THEN I7! = 0: GOTO 223
	IF TS%(I,0)=999 AND b%(I, B3%(I, I3), 47+BY%*7) + b%(I, B3%(I, I3), 42+BY%*7) = 0 THEN I7! = 0: GOTO 223
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 47+BY%*7)*.9 / (b%(I, B3%(I, I3), 42+BY%*7) + b%(I, B3%(I, I3), 47+BY%*7)*.9):GOTO 223
	I7! = b%(I, B3%(I, I3), 11) / (b%(I, B3%(I, I3), 4) + b%(I, B3%(I, I3), 11))
	
223 CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 5, I3) = K8!
	NEXT I3

' CALCULATE STRIKEOUT PERCENTAGES
	FOR I3 = VV(I) TO VV1(I)
	IF p%(I1, P1%(I1), 6) = 0 THEN I2!=0:GOTO 222
	I2! = p%(I1, P1%(I1), 9) / ((p%(I1, P1%(I1), 6) * 3) * .94)

222 I6! = l%(I1, 2) / 1000
	GOSUB 500
	IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),51+PY%*7) - P%(I1, P1%(I1),52+PY%*7) = 0 THEN I2!=0:GOTO 224
	IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I2! = P%(I1, P1%(I1),57+PY%*7) / (P%(I1, P1%(I1),51+PY%*7) - P%(I1, P1%(I1),52+PY%*7))

224 IF TS%(I1,0)=999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 2) / 1000
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = .42: GOTO 282
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 20) - p%(I, P1%(I), 22) = 0 THEN I7! = 0: GOTO 282
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 AND P%(I, P1%(I),37+BY%*7) - P%(I, P1%(I),38+BY%*7)=0 THEN I7! = 0: GOTO 282
	IF B7%(I, I3) = 1 AND TS%(I,0)=999 THEN I7! = P%(I, P1%(I),43+BY%*7) / (P%(I, P1%(I),37+BY%*7) - P%(I, P1%(I),38+BY%*7)): GOTO 282
	IF B7%(I, I3) = 1 THEN I7! = p%(I, P1%(I), 28) / (p%(I, P1%(I), 20) - p%(I, P1%(I), 22)): GOTO 282
	 IF b%(I, B3%(I, I3), 4) = 0 OR b%(I, B3%(I, I3), 4) - b%(I, B3%(I, I3), 6) = 0 THEN I7! = 0: GOTO 282
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 AND b%(I, B3%(I, I3), 42+BY%*7) - b%(I, B3%(I, I3), 43+BY%*7) = 0 THEN I7! = 0: GOTO 282
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 48+BY%*7) / (b%(I, B3%(I, I3), 42+BY%*7) - b%(I, B3%(I, I3), 43+BY%*7))
	I7! = b%(I, B3%(I, I3), 12) / (b%(I, B3%(I, I3), 4) - b%(I, B3%(I, I3), 6))

282 'IF I7!>=1 THEN I7%=.8
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 4, I3) = K8!
	NEXT I3

' CALCULATE THE BATTING AVERAGE
	 FOR I3 = VV(I) TO VV1(I)
	I2! = l%(I1, 1) / 1000
	I6! = p%(I1, P1%(I1), 14) / 1000
	IF p%(I1, P1%(I1), 6) + p%(I1, P1%(I1), 7)=0 THEN I7!=0:GOTO 283
	I7! = p%(I1, P1%(I1), 7) / (((p%(I1, P1%(I1), 6) * 3) * .94) + p%(I1, P1%(I1), 7))

283 CALL NORMALIZE (I7!, I2!, I6!, K8!)
	i9! = K8! / 1000
	GOSUB 500
	 IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = P%(I1, P1%(I1), TY%*6 + 65) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),51+PY%*7)=0 THEN I7! = 0:GOTO 225
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I7! = P%(I1, P1%(I1),52+PY%*7) / P%(I1, P1%(I1),51+PY%*7)

225 IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I2! = SA%(I1,TY%*6+ 1) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN CALL NORMALIZE (I7!, I2!, I6!, K8!)
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN i9! = K8! / 1000
	 IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN I7! = p%(I, P1%(I), 11) / 1000: I6! = p%(I, P1%(I), 14) / 1000: GOTO 228
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 20) = 0 THEN I7! = 0:GOTO 227
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 AND P%(I, P1%(I),37+BY%*7) = 0 THEN I7! = 0:GOTO 227
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN I7! = P%(I, P1%(I),38+BY%*7) / P%(I, P1%(I),37+BY%*7):GOTO 227
	IF B7%(I, I3) = 1 THEN I7! = p%(I, P1%(I), 22) / p%(I, P1%(I), 20)

227 IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN I6! = P%(I, P1%(I), TY%*6 + 65) / 1000:GOTO 228
	IF B7%(I, I3) = 1 THEN I6! = p%(I, P1%(I), 14) / 1000: GOTO 228
	 IF b%(I, B3%(I, I3), 4) = 0 THEN I7! = 0: GOTO 230
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80)<>0 AND b%(I, B3%(I, I3), 42+BY%*7) = 0 THEN I7! = 0:GOTO 230
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I7! = b%(I, B3%(I, I3), 43+BY%*7) / b%(I, B3%(I, I3), 42+BY%*7):GOTO 230
	I7! = b%(I, B3%(I, I3), 6) / b%(I, B3%(I, I3), 4)

230 I6! = b%(I, B3%(I, I3), 20) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I6! = b%(I, B3%(I, I3), 56+TY%*6) / 1000

228 I2! = l%(I, 1) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I2! = SA%(I,TY%*6+ 1) / 1000
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	I7! = K8! / 1000
	I6! = l%(I, 1) / 1000
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN I6! = SA%(I,TY%*6+ 1) / 1000
	I2! = i9!
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 0, I3) = K8!
'    PRINT B$(I, B3%(I, I3)),B4%(I, 0, I3)
	NEXT I3
'222 I$=GetKeyPress$
' CALCULATE THE EXTRA BASE HITS RATIOS
	 FOR I3 = VV(I) TO VV1(I)
	GOSUB 500
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 11) <> 999 THEN B4%(I, 1, I3) = (130 * (i9! * 1000)) / l%(I1, 1): B4%(I, 2, I3) = (19 * (i9! * 1000)) / l%(I1, 1): B4%(I, 3, I3) = 37: GOTO 235
	E2! = (i9! * 1000) / l%(I1, 1)
	IF TS%(I1,0) =999 THEN E2! = (I9! * 1000) / SA%(I1,TY%*6+ 1)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 22) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF B7%(I, I3) <> 1 AND b%(I, B3%(I, I3), 6)=0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 AND P%(I, P1%(I),38+BY%*7) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	 IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 1, I3) = INT((P%(I, P1%(I),39+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000) * E2!:' doubles times  * .95
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 2, I3) = INT((P%(I, P1%(I),40+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000) * E2!
	IF B7%(I, I3) = 1 AND TS%(I,0) = 999 THEN B4%(I, 3, I3) = INT((P%(I, P1%(I),41+BY%*7) / P%(I, P1%(I),38+BY%*7)) * 1000): GOTO 235
	 IF B7%(I, I3) = 1 THEN B4%(I, 1, I3) = INT((p%(I, P1%(I), 23) / p%(I, P1%(I), 22)) * 1000) * E2!:' doubles times  * .95
	IF B7%(I, I3) = 1 THEN B4%(I, 2, I3) = INT((p%(I, P1%(I), 24) / p%(I, P1%(I), 22)) * 1000) * E2!
	IF B7%(I, I3) = 1 THEN B4%(I, 3, I3) = INT((p%(I, P1%(I), 25) / p%(I, P1%(I), 22)) * 1000): GOTO 235
	 IF b%(I, B3%(I, I3), 6) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 AND b%(I, B3%(I, I3), 43+BY%*7) = 0 THEN B4%(I, 1, I3) = 0: B4%(I, 2, I3) = 0: B4%(I, 3, I3) = 0: GOTO 235
	 IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 1, I3) = INT((b%(I, B3%(I, I3), 44+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000) * E2!:'see note on pitchers * .95
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 2, I3) = INT((b%(I, B3%(I, I3), 45+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000) * E2!
	IF TS%(I,0) = 999 AND B%(I, B3%(I ,I3), 80) <>0 THEN B4%(I, 3, I3) = INT((b%(I, B3%(I, I3), 46+BY%*7) / b%(I, B3%(I, I3), 43+BY%*7)) * 1000):GOTO 235
	 B4%(I, 1, I3) = INT((b%(I, B3%(I, I3), 7) / b%(I, B3%(I, I3), 6)) * 1000) * E2!:'see note on pitchers * .95
	B4%(I, 2, I3) = INT((b%(I, B3%(I, I3), 8) / b%(I, B3%(I, I3), 6)) * 1000) * E2!
	B4%(I, 3, I3) = INT((b%(I, B3%(I, I3), 9) / b%(I, B3%(I, I3), 6)) * 1000)

235 I6! = l%(I1, 4) / 1000
	I7! = B4%(I, 3, I3) / 1000
	I2! = 0
	IF p%(I1, P1%(I1), 7)<>0 THEN I2! = p%(I1, P1%(I1), 13) / p%(I1, P1%(I1), 7)
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 THEN I6! = SA%(I1,TY%*6+ 4) / 1000
	IF TS%(I1,0) = 999 AND P%(I1, P1%(I1), 89)<>0 AND P%(I1, P1%(I1),52+PY%*7)<>0 THEN I2! = P%(I1, P1%(I1),55+PY%*7) / P%(I1, P1%(I1),52+PY%*7)
	CALL NORMALIZE (I7!, I2!, I6!, K8!)
	B4%(I, 3, I3) = K8!
	NEXT I3
' ADJUST LEFTY/RIGHTY THE OLD WAY
	 I4 = p%(I1, P1%(I1), 0)
	FOR I3 = VV(I) TO VV1(I)
	IF TS%(I,0)=999 AND B%(I, B3%(I ,I3), 80) <>0 THEN 236
	I2 = b%(I, B3%(I, I3), 0)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND p%(I1, P1%(I1), 0) = 1 THEN A = 1.04: A1 = .88: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND p%(I1, P1%(I1), 0) = -1 THEN A = .93: A1 = 1.16: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND p%(I1, P1%(I1), 0) = -1 THEN A = 1.02: A1 = .98: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND p%(I1, P1%(I1), 0) = 1 THEN A = .97: A1 = 1.1: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -2 AND I4 = 1 THEN A = 1.04: A1 = .96: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -2 AND I4 = -1 THEN A = .78: A1 = 1.22: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 2 AND I4 = 1 THEN A = .78: A1 = 1.22: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 2 AND I4 = -1 THEN A = 1.04: A1 = .96: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -1 AND I4 = -1 THEN A = .93: A1 = 1.16: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = -1 AND I4 = 1 THEN A = 1.04: A1 = .88: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 1 AND I4 = 1 THEN A = .97: A1 = 1.1: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 1 AND I4 = -1 THEN A = 1.02: A1 = .98: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 0 AND I4 = -1 THEN A = 1.01: A1 = .86: CALL LRADJ (A!, A1!)
	IF B7%(I, I3) <> 1 AND I2 = 0 AND I4 = 1 THEN A = .99: A1 = .93: CALL LRADJ (A!, A1!)

236 B4%(I, 6, I3) = B4%(I, 3, I3): B4%(I, 7, I3) = B4%(I, 3, I3)
	NEXT I3

' ADJUST AVERAGES FOR THE HOME TEAM
	 FOR I3 = VV(I) TO VV1(I)
	IF I = 1 THEN B4%(I, 0, I3) = B4%(I, 0, I3) + 5
	IF I <> 1 THEN B4%(I, 0, I3) = B4%(I, 0, I3) - 5
	NEXT I3

' CALCULATE THE HOME PARK FACTORS
	 IF N = 2 THEN 273
	FOR I2 = VV(I) TO VV1(I)
	IF I = 1 THEN B4%(I, 1, I2) = INT(B4%(I, 1, I2) * (l%(I, 6) / 100) + .5): B4%(I, 2, I2) = INT(B4%(I, 2, I2) * (l%(I, 7) / 100) + .5)
	IF I <> 1 THEN B4%(I, 1, I2) = INT(B4%(I, 1, I2) * (l%(I1, 6) / 100) * ((200 - l%(I, 6)) / 100) + .5): B4%(I, 2, I2) = INT(B4%(I, 2, I2) * (l%(I1, 7) / 100) * ((200 - l%(I, 7)) / 100) + .5)
	IF K9(I) > 1000 THEN I3! = 100: I6! = INT(K9(I) / 1000) * WF!: I7! = (K9(I) - INT((K9(I) / 1000)) * 1000) * WF!
	IF K9(I1) > 1000 THEN I4! = 100: K8! = INT(K9(I1) / 1000) * WF!: i9! = (K9(I1) - INT((K9(I1) / 1000)) * 1000) * WF!
	IF K9(I) < 1000 THEN I3! = K9(I) * WF!: I6! = I3!: I7! = I3!
	IF K9(I1) < 1000 THEN I4! = K9(I1) * WF!: K8! = I4!: i9! = I4!
	IF I = 1 THEN B4%(I, 3, I2) = INT((B4%(I, 3, I2) * (I3! / 100)) + .5): B4%(I, 6, I2) = INT((B4%(I, 6, I2) * (I6! / 100)) + .5): B4%(I, 7, I2) = INT((B4%(I, 7, I2) * (I7! / 100)) + .5)
	IF I<>1 THEN B4%(I, 3, I2)=INT((B4%(I, 3, I2)*(I4! / 100) * ((200 - I3!) / 100)) + .5): B4%(I, 6, I2) = INT((B4%(I, 6, I2) * (K8! / 100) * ((200 - I6!) / 100)) + .5): B4%(I, 7, I2) = INT((B4%(I, 7, I2) * (i9! / 100) * ((200-I7!)/100))+ .5)
	NEXT I2
	NEXT I
	GOTO 273

500 II = b%(I, B3%(I, I3), 0)
	I4 = p%(I1, P1%(I1), 0)
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND I4 = 1 THEN TY%=1
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = -1 AND I4 = -1 THEN TY%=0
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND I4 = -1 THEN TY%=2
	IF B7%(I, I3) = 1 AND p%(I, P1%(I), 0) = 1 AND I4 = 1 THEN TY%=3
	IF B7%(I, I3) <>1 AND II<0 AND I4=1 THEN TY%=1
	IF B7%(I, I3) <>1 AND II<0 AND I4=-1 THEN TY%=0
	IF B7%(I, I3) <>1 AND II>0 AND I4=-1 THEN TY%=2
	IF B7%(I, I3) <>1 AND II>0 AND I4=1 THEN TY%=3
	IF B7%(I, I3) <>1 AND II=0 AND I4=1 THEN TY%=1
	IF B7%(I, I3) <>1 AND II=0 AND I4=-1 THEN TY%=2
	IF TY%=0 OR TY%=1 THEN PY%=0
	IF TY%=2 OR TY%=3 THEN PY%=1
	IF TY%=0 OR TY%=2 THEN BY%=0
	IF TY%=1 OR TY%=3 THEN BY%=1
	RETURN

273 '
END SUB

SUB LRADJ (A!, A1!)
	FOR I6 = 0 TO 3
	B4%(I, I6, I3) = B4%(I, I6, I3) * A!
	NEXT I6
	B4%(I, 5, I3) = B4%(I, 5, I3) * A!: ' NEWLY ADDED
	B4%(I, 4, I3) = B4%(I, 4, I3) * A1!
END SUB

SUB NORMALIZE (I7!, I2!, I6!, K8!)
	 I0! = (I7! * I2! * (1 - I6!))
	IF (I0! + ((1 - I7!) * (1 - I2!) * I6!))=0 THEN K8!=0:GOTO 15
	K8! = I0! / (I0! + ((1 - I7!) * (1 - I2!) * I6!))

15  K8! = INT((K8! * 1000) + .0005)

END SUB

SUB SETUP ()
		FOR I = 3 TO 6: READ F$(I): NEXT
	H0$(0) = " ": H0$(1) = "H"
	TM$(1) = "HOT": TM$(2) = "WARM": TM$(3) = "COOL": TM$(4) = "COLD"
	SC$(1) = "CLEAR": SC$(2) = "PARTLY CLOUDY": SC$(3) = "OVERCAST": SC$(4) = "DOME"
	PC$(1) = "THUNDERSTORMS": PC$(2) = "SHOWERS": PC$(3) = "DRIZZLE": PC$(4) = "NONE"

10  CALL GETSTATS
	CALL DISPBAVG(D)
	W5 = 0: W6 = 1
	FOR I = 0 TO 1: FOR I1 = 1 TO 9: FOR I2 = 1 TO 9: READ BT%(I, I1, I2): NEXT: NEXT: NEXT
	FOR I = 2 TO 9: FOR I1 = 0 TO 2: READ CF%(I, I1): NEXT: NEXT
	FOR I = 2 TO 9: FOR I1 = 0 TO 1: READ LB%(I, I1): NEXT: NEXT
	FOR I = 0 TO 5: FOR I1 = 0 TO 2: READ SO%(I, I1): NEXT: NEXT
	FOR I = 11 TO 0 STEP -1: FOR I1 = 0 TO 2: READ TP%(I, I1): NEXT: NEXT
END SUB

SUB BASEONBALLS (S2%,D2,D,P)
	S2% = 2
	bv = INT(RND(1) * 100) + 1: CALL RANDNUM (RN)
	IF D2 = 2 AND bv <= 50 THEN F$=Q$(0)+" is walked intentionally..."
	if d2 = 2 and bv >  50 then F$=Q$(0)+" is given a purpose pass..."
	IF D2 = 2 THEN
		S8%(D, 0) = S8%(D, 0) + 1
		CALL PBP (F$)
		GOTO 3241
	end if
	  IF RN <= 30 THEN CALL PBP ("FULL COUNT...")
	CALL DELIVERY (D)
	  Y% = INT(RND(1) * 80) + 780
	CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)
	  Y% = INT(RND(1) * 30) + 860
	CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)

3241 call randnum(rn)
	if s4% = 7 and  rn<=30 then call pbp("that forces a run in")
	rn=int(rnd(1)*4)+1
	if s4%>3 and s4%<7 and rn=1 then call pbp("and the bases are loaded!")
	if s4%>3 and s4%<7 and rn=2 then call pbp("and the sacks are jammed!")
	if s4%>3 and s4%<7 and rn=3 then call pbp("and the bases are full!")
	if s4%>3 and s4%<7 and rn=4 then call pbp("and that loads them up!")
	  I5 = P1%(D): I6 = 4: CALL ADDPIT (D,I5,I6)
	IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 30: CALL INCPITOFF (P,I5,I6): GOTO 3243
	I5 = B3%(P, B1!(P)): I6 = 7: CALL INCBATOFF (P,I5,I6)

3243 '
END SUB

SUB HITBYPITCH (S2%,D)
	S2% = 1
	CALL DELIVERY (D)
	y% = INT(RND(1) * 20) + 720
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)
	y% = INT(RND(1) * 11) + 740
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)
	y% = INT(RND(1) * 20) + 760
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
	CALL PBP (Y$)
	call randnum(rn)
	if rn<=50 and s4% = 7 then call pbp("tough way to get an RBI")
END SUB

SUB WILDPITCH (S2%,D)
	S2% = 6
	CALL RANDNUM (RN)
	IF RN <= 16 THEN F$="and it gets away from "+B$(D, B9%(D, 2))
	IF RN > 16 AND RN <= 32 THEN CALL PBP ("pitch is WAY high"):F$=B$(D, B9%(D, 2))+" can't get it"
	IF RN > 32 AND RN <= 48 THEN CALL PBP ("It's in the dirt"):F$="The runners move up 90 ft."
	IF RN > 48 AND RN <= 64 THEN CALL PBP ("It bounces in front of the plate"): CALL PBP ("it's past the catcher"): F$="the runners advance"
	IF RN > 64 AND RN <= 80 THEN F$="it's over "+ B$(D, B9%(D, 2))+"'s head"
	IF RN > 67 THEN CALL PBP ("wild pitch by "+P$(D, P1%(D))): GOTO 3235
	CALL RANDNUM (RN)
	CALL PBP (F$)
	IF RN <= 33 THEN F$="It's ruled a wild pitch"
	IF RN > 33 AND RN <= 66 THEN F$="score it a wild pitch"
	IF RN > 66 THEN F$="credit "+P$(D, P1%(D))+" with a wild pitch"
	CALL PBP (F$)

3235 '
END SUB

SUB RELIEVERS
	CALL PITSTRAT
	if pz=1 then pz=0:goto 8885
	I2 = 0
	A1 = S6%(D, 0) - S6%(p, 0)
	FOR I = 0 TO 21
	A2 = p%(D, i, 4) - p%(D, i, 5) - p%(d, i, 3)*1.3
	IF p%(D, i, 15) > 0 OR P$(D, i) = "XXX" OR A2=0 THEN 8874
	IF INNING% <= 7 AND A2 > 0 THEN I2 = I2 + A2
	IF INNING% > 7 AND A1 < 0 AND A2 > 0 THEN I2 = I2 + A2
	IF INNING% > 7 AND A1 >= 0 THEN I2 = I2 + p%(D, i, 3)

8874 NEXT i
	I3 = INT(RND(1) * I2) + 1
	I2 = 0
	FOR i = 0 TO 21
	A2 = p%(D, i, 4) - p%(D, i, 5) - p%(d, i, 3)*1.3
	IF p%(D, i, 15) > 0 OR p$(D, i) = "XXX" OR A2 = 0 THEN 8883
	IF INNING% <= 7 AND A2 > 0 AND I3 <= I2 + A2 THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% <= 7 AND A2 > 0 THEN I2 = I2 + A2: GOTO 8883
	  IF INNING% > 7 AND A1 < 0 AND A2 > 0 AND I3 <= I2 + A2 THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% > 7 AND A1 < 0 AND A2 > 0 THEN I2 = I2 + A2: GOTO 8883
	IF INNING% > 7 AND A1 >= 0 AND I3 > I2 AND I3 <= I2 + p%(D, i, 3) THEN CALL NEWPITCHER: GOTO 8885
	IF INNING% > 7 AND A1 >= 0 THEN I2 = I2 + p%(D, i, 3)

8883 NEXT i
	FOR i = 0 TO 21
	IF p%(D, i, 15) = 0 AND p$(D, i) <> "XXX" THEN CALL NEWPITCHER: GOTO 8885
	NEXT i

8885 '
END SUB

SUB REPLACEMENTS (I7%, I8%, i9%)
	' REPLACEMENT AVERAGE CALCS, I7%,I8%,I9%
	FOR xx = 22 TO 25
	IF B7%(I7%, I8%) = b%(I7%, i9%, xx) THEN b%(I7%, i9%, 15) = b%(I7%, i9%, 33 + (xx - 22) * 2): b%(I7%, i9%, 17) = b%(I7%, i9%, 34 + (xx - 22) * 2): b%(I7%, i9%, 19) = b%(I7%, i9%, xx + 4): GOTO 7110
	NEXT
	J1 = B7%(I7%, I8%)
	IF J1 = 2 THEN J2 = 1: J3 = 0: J4 = 910 + INT(RND(1) * 10) + 1
	IF J1 = 3 THEN J2 = 0: J3 = 3: J4 = 960 + INT(RND(1) * 10) + 1
	IF J1 = 4 THEN J2 = 0: J3 = 3: J4 = 912 + INT(RND(1) * 10) + 1
	IF J1 = 5 OR J1 = 6 THEN J2 = 0: J3 = 3: J4 = 902 + INT(RND(1) * 10) + 1
	IF J1 >= 7 THEN J2 = 2: J3 = 2: J4 = 972 + INT(RND(1) * 10) + 1
	b%(I7%, i9%, 15) = J2
	b%(I7%, i9%, 17) = J3
	b%(I7%, i9%, 19) = J4

7110 '
END SUB

SUB SCOREBOARD ()
	S6%(p, 1) = S6%(p, 1) + 1
	FORMATTED$=FUSING$(STR$(S6%(P,1)),"##")
	GPRINT0VE P+29,56,FORMATTED$,15
END SUB

SUB SECOND2FIRST (D)
	'*** throw to 1st from 2nd ***
	Y% = INT(RND(1) * 30) + 1540
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$+" to first")
END SUB

SUB SINGLEPBP (S2%, D, W%)
	'*** play by play for a single s2%=7, s2%=12 ***
	if s2% = -12 then s2% = 12:goto 8014
	CALL DELIVERY(D)
	CALL RANDNUM(rn)
	IF S2% = 12 THEN CALL PBP (Q$(0)+" beats out an infield hit"): GOTO 8013
	IF rn <= 50 THEN Y% = INT(RND(1) * 50) + 1610: CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D): CALL PBP (Y$+" "+C$(W%)): GOTO 8013
	IF W% = 7 THEN Y% = INT(RND(1) * 70) + 1660
	IF W% = 9 THEN Y% = INT(RND(1) * 70) + 1730
	IF W% = 8 THEN Y% = INT(RND(1) * 90) + 1800
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)
	CALL DELAY

8013 '

8014 END SUB

SUB SINGLEROUTINE (W%, p, I1, D, S2%, P2)
	CALL GETOUTFIELDER (W%, p, I1, D)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6): I6 = 1: CALL INCBATOFF(P, I5, I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT(D, I5, I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6): I6 = 24: CALL INCPITOFF(P, I5, I6)
	if h7% > sn then CALL SINGLEPBP(S2%, D, W%)
	FOR I = 0 TO 3
	A5%(I) = 1
	NEXT I
	IF S2% = 12 OR B0%(2, 0) = 0 THEN 3075
	H6% = INT(RND(1) * 100) + 1
	IF B7%(p, B0%(2, 0)) <> 1 THEN BR% = b%(p, B3%(p, B0%(2, 0)), 16)
	IF B7%(p, B0%(2, 0)) = 1 THEN BR% = p%(p, P1%(p), 31)
	IF O%(0) = 0 AND W% = 7 THEN I3= 16 + BR% * 3
	IF O%(0) = 1 AND W% = 7 THEN I3= 36 + BR% * 3
	IF O%(0) = 2 AND W% = 7 THEN I3= 79 + BR% * 3
	IF O%(0) = 0 AND W% = 8 THEN I3= 49 + BR% * 3
	IF O%(0) = 1 AND W% = 8 THEN I3= 61 + BR% * 3
	IF O%(0) = 2 AND W% = 8 THEN I3= 82 + BR% * 3
	IF O%(0) = 0 AND W% = 9 THEN I3= 27 + BR% * 3
	IF O%(0) = 1 AND W% = 9 THEN I3= 44 + BR% * 3
	IF O%(0) = 2 AND W% = 9 THEN I3= 79 + BR% * 3
	IF H6% <= I3 OR P2 = 2 AND S2% <> 12 THEN A5%(2) = 2

3075 IF B0%(1, 0) = 0 OR S2% = 12 OR B0%(2, 0) > 0 AND A5%(2) = 1 THEN 3092
	H6% = INT(RND(1) * 100) + 1
	BR% = b%(p, B3%(p, B0%(1, 0)), 16)
	IF B7%(p, B0%(1, 0)) = 1 THEN BR% = p%(p, P1%(p), 31)
	I3 = 0
	IF O%(0) = 0 AND W% = 7 THEN I3=  6 + BR% * 3
	IF O%(0) = 1 AND W% = 7 THEN I3= 10 + BR% * 3
	IF O%(0) = 2 AND W% = 7 THEN I3= 11 + BR% * 3
	IF O%(0) = 0 AND W% = 8 THEN I3= 25 + BR% * 3
	IF O%(0) = 1 AND W% = 8 THEN I3= 26 + BR% * 3
	IF O%(0) = 2 AND W% = 8 THEN I3= 45 + BR% * 3
	IF O%(0) = 0 AND W% = 9 THEN I3= 38 + BR% * 3
	IF O%(0) = 1 AND W% = 9 THEN I3= 40 + BR% * 3
	IF O%(0) = 2 AND W% = 9 THEN I3= 62 + BR% * 3
	IF H6% <= I3 OR P2 = 2 AND S2% <> 12 THEN A5%(1) = 2

3092 CALL ADVANCEMENT(p, D, S2%, F%,I3)
END SUB

SUB SOMESTATS (V, p, F%, D)
	IF B0%(V, 0) > 0 AND B7%(p, B0%(V, 0)) = 1 AND F% = 0 THEN I5 = P1%(p): I6 = 32: CALL INCPITOFF(P, I5, I6)
	IF B0%(V, 0) > 0 AND B7%(p, B0%(V, 0)) <> 1 AND F% = 0 THEN I5 = B3%(p, B0%(V, 0)): I6 = 9: CALL INCBATOFF(P, I5, I6)
END SUB

SUB SQUEEZECHOICE (SH)
	'*** SQUEEZE ***
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN SH = 2: GOTO 4242
	CALL PBP ("1-SQUEEZE PLAY 2-BUNT FOR SACRIFICE/HIT")
4241 i$ = GetKeyPress$
	SH = VAL(i$): IF SH < 1 OR SH > 2 THEN 4241

4242 '
END SUB

SUB STEAL3RD (TGTBASE%, AA, Q0, H0%, D)
	IF TGTBASE% = 3 AND AA >= 24 THEN Q0 = 830: ' *** SUCCESS OF 3B STEAL ***
	IF TGTBASE% = 3 AND AA < 24 THEN Q0 = 700
	IF TGTBASE% = 4 THEN Q0 = Q0 * .5
	' *** H0%=0, runner not held ***
	IF H0% = 0 THEN Q0 = Q0 + 50: GOTO 4874
	IF H0% = 1 THEN Q0 = Q0 - ((p%(D, P1%(D), 18) - 5) * 25):' - 50

4874 REM
END SUB

SUB STEALCHANCE (AA, I2, TGTBASE%,CHANCE%)
DIM DD AS SINGLE
DIM TT AS SINGLE
DIM RR AS SINGLE
DIM SS AS SINGLE
DIM GG AS SINGLE
DIM I1 AS SINGLE
	AA = b%(p, I2, 13)
	BB = b%(p, I2, 13) + b%(p, I2, 14)
	CC = b%(p, I2, 7): ' *** CHANCE TO STEAL 2B & 3B ***
	IF AA <= 43 THEN DD = .16: GOTO 4853
	IF AA <= 60 THEN DD = .21: GOTO 4853
	DD = .3

4853 TT = CC + BB - (BB * DD): ' *** TOTAL CHANCES TO STEAL 3RD ***
	RR = (TT * .19) * 1.4: ' *** "      "      "   "     "  1 OUT ***
	SS = (TT * .56) * 1.4: ' ***                            0 OUT ***
	GG = RR + SS:    ' ***                            (0,1 OUT) ***
	IF TGTBASE% = 3 THEN 4862
	FF = b%(p, I2, 6) + b%(p, I2, 11) - b%(p, I2, 9)
	IF FF=0 THEN I1=0:GOTO 4868
	IF AA >= 12 THEN I1 = INT(((BB - BB * DD) / FF) * 1500)
	IF AA < 12 THEN I1 = INT((BB / FF) * 1500)
	IF I1 < 0 THEN I1 = 0
	GOTO 4868

4862 IF BB < 10 AND O%(0) = 0 THEN I1 = 0
	IF BB < 10 AND O%(0) = 1 THEN I1 = 10
	IF O%(0) = 2 OR TT=0 THEN I1 = 0
	IF BB >= 10 AND O%(0) = 1 THEN I1 = BB * DD / TT * 1000
	IF BB >= 10 AND O%(0) = 0 THEN I1 = BB * DD / TT * 1000
	IF I1 < 0 THEN I1 = 0

4868 CHANCE%=I1
	'
END SUB

SUB STRIKEOUT (U%, D)
	'*** strikeout pbp ***
	IF U% = 3 THEN BV = 20: GOTO 3770
	BV = INT(RND(1) * 100): CALL RANDNUM(rn)
	IF rn <= 20 THEN CALL PBP ("Full count...")
	CALL DELIVERY(D)
	IF BV = 1 THEN CALL PBP ("Strike Three!!"):CALL PBP (Q$(0)+" turns to "+U$(1)):CALL PBP ("he tees them higher than that"):CALL PBP ("at the country club!!"):GOTO 3774
	IF BV = 2 THEN CALL PBP ("Strike three..."):CALL PBP (Q$(0)+" just stands there"):CALL PBP ("like a house by the side of the road"): GOTO 3774
	IF BV > 2 AND BV <= 5 THEN CALL PBP (Q$(0)+" looks at a called strike three"): CALL PBP ("he turns to glare at "+U$(1)):GOTO 3774
	  CALL RANDNUM(RN)
	IF RN<=25 THEN
		Y% = INT(RND(1) * 40) + 890
	ELSE
		Y% = INT(RND(1) * 80) + 930
	END IF
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)

3770 Y% = INT(RND(1) * 40) + 1010
	CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$)

3774 CALL RANDNUM(rn)
	IF rn <= 20 THEN CALL PBP ("Number"+STR$(P8%(D, P1%(D), 5) + 1)+" for "+p$(D, P1%(D)))
	IF rn > 20 AND rn <= 40 THEN CALL PBP ("That's"+STR$(P8%(D, P1%(D), 5) + 1)+" for "+p$(D, P1%(D)))
	IF RN = 41 THEN call pbp (Q$(0)+" is not a happy man")
	IF RN = 42 THEN call pbp (Q$(0)+" has had better AB's")
END SUB

SUB STRIKEOUTCS (SO)
	IF SO = 7 THEN B1!(p) = B1!(p) + 1
END SUB

SUB STRIPRPBP (Y$, Y%, D)

3991 FOR BH% = 1 TO LEN(Y$)
	IF ASC(MID$(Y$, BH%, 1)) > 122 OR ASC(MID$(Y$, BH%, 1)) < 32 THEN CALL GETRPBP(Y%, Y$, D): GOTO 3991
	NEXT
END SUB

SUB TAGADVANCE (S2%, I8, D)
	IF S2% = 7 AND I8 = 2 AND B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 27: CALL INCPITOFF(P, I5, I6): GOTO 3394
	IF S2% = 7 AND I8 = 2 AND B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 4: CALL INCBATOFF(P, I5, I6): GOTO 3394
	IF S2% = 8 AND I8 = 3 AND B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 28: CALL INCPITOFF(P, I5, I6): P8%(p, P1%(p), 27) = P8%(p, P1%(p), 27) - 1: P5%(p, P1%(p), 27) = P5%(p, P1%(p), 27) - 1: GOTO 3394
	IF S2% = 8 AND I8 = 3 AND B7%(p, B1!(p)) <> 1 THEN I3 = 4: I4 = 5: I5 = B3%(p, B1!(p))
	IF S2% = 9 AND I8 = 4 THEN I3 = 5: I4 = 6: I5 = B3%(p, B1!(p))
	IF S2% = 8 AND I8 = 3 OR S2% = 9 AND I8 = 4 THEN 3393
	GOTO 3394

3393 IF P%(D,P1%(D),0) = 1 THEN 3392
	CSS%(P, I5, I3 - 2) = CSS%(P, I5, I3 - 2) - 1
	CSS%(P, I5, I4 - 2) = CSS%(P, I5, I4 - 2) + 1

3392 B8%(p, I5, I3) = B8%(p, I5, I3) - 1
	B5%(p, I5, I3) = B5%(p, I5, I3) - 1
	B8%(p, I5, I4) = B8%(p, I5, I4) + 1
	B5%(p, I5, I4) = B5%(p, I5, I4) + 1

3394 '
END SUB

SUB THROW2FIRST (D)
	'*** throw to 1st from 3rd/ss ***
	Y% = INT(RND(1) * 40) + 1570
	CALL GETRPBP(Y%, Y$, D)
	CALL STRIPRPBP(Y$, Y%, D)
	CALL PBP (Y$+" to first")

END SUB

SUB TIRING (P9)
	'tired on runs and hits
	IF S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) AND S8%(P9, 1) <= P8%(P9, P1%(P9), 2) THEN 9756
	IF S8%(P9, 1) > 0 AND (S8%(P9, 0) <= P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) OR S8%(P9, 1) <= P8%(P9, P1%(P9), 2)) THEN 9755
	IF S8%(P9, 1) > 0 THEN 9754
	IF P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) < 3 AND P8%(P9, P1%(P9), 2) = 0 THEN 9754
	IF P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 2 AND P8%(P9, P1%(P9), 0) >= 5 THEN 9755

9754 PRINT p$(P9, P1%(P9)); " seems to be OK!!"; : GOTO 9757

9755 PRINT p$(P9, P1%(P9)); " is tiring!!"; : GOTO 9757

9756 CALL RANDNUM(rn)
	IF rn <= 50 AND S8%(P9, 1) > 0 AND INNING% < 6 OR S8%(P9, 1) = 0 AND P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 THEN PRINT p$(P9, P1%(P9)); " just doesn't have it today"; : GOTO 9757
	IF rn > 50 AND S8%(P9, 1) > 0 AND INNING% < 6 OR S8%(P9, 1) = 0 AND P8%(P9, P1%(P9), 4) + P8%(P9, P1%(P9), 1) > 4 THEN PRINT p$(P9, P1%(P9)); " ain't got nuthin' on his fastball folks!"; : GOTO 9757
	PRINT p$(P9, P1%(P9)); " is tired!!";

9757 '
END SUB

SUB TRIPLEPLAY (D)
	Q$(4) = b$(D, B9%(D, 5))
	CALL PBP ("grounder to "+Q$(4)+" @ 3B")
	CALL PBP (Q$(2)+" OUT @ 3B")
	CALL PBP (Q$(1)+" OUT @ 2B")
	CALL PBP (Q$(0)+" OUT @ 1B")
	CALL PBP ("--- TRIPLE PLAY !!! ---"): CALL DELAY
	O%(0) = 3
	I5 = P1%(D): I6 = 0: FOR i = 1 TO 3: CALL ADDPIT(D, I5, I6): NEXT i
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF(P, I5, I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF(P, I5, I6)
END SUB

SUB TRYFOREXTRABASE (LN%, S2%, P2, Z2%, D, W%, sah%, SB%,I8,I9)
	IF LN% = 3308 THEN LN% = 0: GOTO 3308
	I8 = 0: H6% = INT(RND(1) * 100) + 1
	'IF S2% = 8 AND B0%(3, 0) > 0 AND H6% <= b%(p, B3%(p, B0%(3, 0)), 16) * 4 THEN I8 = 4
	IF S2% = 8 AND B0%(2, 0) > 0 AND I8 = 0 AND H6% < 6 AND B0%(3, 0) = 0 AND b%(p, B3%(p, B0%(2, 0)), 8) > 0 THEN I8 = 3
	IF S2% = 9 AND B0%(3, 0) > 0 AND H6% <= 2 AND b%(p, B3%(p, B0%(3, 0)), 9) > 0 THEN I8 = 4
	IF S2% = 7 AND B0%(3, 0) > 0 AND P2 = 2 AND H6% >= 6 AND H6% <= b%(p, B3%(p, B0%(3, 0)), 16) THEN I8 = 4
	IF S2% = 7 AND B0%(3, 0) > 0 AND Z2% = B0%(3, 0) AND P2 <> 2 AND H6% <= b%(p, B3%(p, B0%(3, 0)), 16) * 12 THEN I8 = 4
	E2% = 10
	IF B0%(2,0)>0 THEN E2%=E2% + b%(p, B3%(p, B0%(2, 0)), 16)
	IF S2% = 7 AND B0%(2, 0) > 0 AND P2 <> 2 AND H6% <= E2% AND B0%(3, 0) = 0 AND I8 = 0 THEN I8 = 3
	IF S2% = 7 AND B0%(1, 0) > 0 AND B0%(2, 0) = 0 AND I8 = 0 AND H6% <= b%(p, B3%(p, B0%(1, 0)), 16) AND b%(p, B3%(p, B0%(1, 0)), 7) > 0 THEN I8 = 2
	IF I8 = 0 THEN 3385

3308 IF u6 = 2 OR u6 = 1 AND p = u9 THEN 3310
	F$=Q$(I8 - 1)
	IF S2% < 10 THEN CALL PBP (F$+" try for "+H$(I8)+" (YN) ")
	IF S2% > 10 THEN CALL PBP (F$+" tag up (YN) ")
	CALL PBP ("against "+ Q$(4))

3310 ';" Arm:";B%(D,B9%(D,W%),15)
	i$ = "N": A1 = S6%(p, 0) - S6%(D, 0)
	IF (u6 = 2 OR u6 = 1 AND p = u9) AND A1 > -2 AND A1 < 2 AND S2% <> 19 THEN i$ = "Y": GOTO 3312
	IF (u6 = 2 OR u6 = 1 AND p = u9) AND A1 < 0 AND (INNING% >= 7 OR S2% = 19) THEN i$ = "N": GOTO 3312
	IF S2% = 8 AND O%(0) = 2 AND I8 = 3 AND (u6 = 2 OR u6 = 1 AND p = u9) THEN i$ = "N"
	IF u6 = 2 OR u6 = 1 AND p = u9 THEN 3312
3311 i$ = GetKeyPress$

3312 IF UCASE$(i$) <> "Y" THEN i9 = 1: GOTO 3385
	F$=Q$(I8 - 1)+" is "
	IF S2% < 10 THEN CALL PBP (F$+"going for "+H$(I8))
	IF S2% > 10 THEN CALL PBP (F$+"tagging")
	IF S2% = 7 AND I8 = 2 OR S2% > 10 AND S4% = 3 THEN 3317
	IF u6 = 2 OR u6 = 1 AND D = u9 THEN 3322
	CALL PBP (Q$(4)+" throw to "+H$(I8)+" (YN) ")

3322 CALL RANDNUM (RN)
	IF (u6 = 2 OR u6 = 1 AND D = u9) AND MG%(D,766)=999 AND I8=4 AND INNING%>=MG%(D,767) AND MG%(D,767)<>0 AND MG%(D,768)<=S6%(D, 0) - S6%(P, 0) THEN I$="N":GOTO 3316
	IF (u6 = 2 OR u6 = 1 AND D = u9) AND MG%(D,766)=999 AND I8=4 AND INNING%>=MG%(D,769) AND MG%(D,769)<>0 AND MG%(D,770)<=S6%(D, 0) - S6%(P, 0) THEN I$="N":GOTO 3316
	IF u6 = 2 OR u6 = 1 AND D = u9 THEN i$ = "Y": GOTO 3316
3315 i$ = GetKeyPress$

3316 i9 = 0: IF UCASE$(i$) <> "Y" THEN i9 = 1: CALL PBP ("No throw is made"): GOTO 3380

3317 CALL PBP ("Here comes the throw by "+Q$(4))
	'FOR i = 1 TO 3: PRINT "."; : CALL DELAY: NEXT i: PRINT " "
	CALL FLDERR (F%, W%, D, I3, SB%, S2%, p): IF F% = 1 THEN 3385
	I4 = INT(RND(1) * 1000) + 1: A = b%(p, B3%(p, B0%(I8 - 1, 0)), 16)
	IF B7%(p, B0%((I8 - 1), 0)) = 1 THEN A = p%(p, P1%(p), 31)
	A = A - 5: A = A * 50
	IF S2% = 17 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 1000 + A - b:' WAS 800
	IF S2% = 18 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 800 + A - b:' WAS 650
	IF S2% = 19 OR S2% < 10 THEN b = ((b%(D, B9%(D, W%), 15) - 5) * 25): I3 = 500 + A - b
	IF I4 <= I3 THEN 3380
	A5%(I8 - 1) = 11: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: CALL ADDPIT(D, I5, I6)
	CALL RANDNUM (rn)
	IF rn <= 4 AND I8 = 4 THEN CALL PBP (Q$(I8 - 1)+" collides with "+b$(D, B9%(D, 2))): CALL PBP (b$(D, B9%(D, 2))+" hangs on!!"): GOTO 3377
	IF rn <= 50 AND I8 <> 4 THEN CALL PBP (Q$(I8 - 1)+" doesn't make it!"): GOTO 3377
	IF rn <= 80 THEN CALL PBP ("throw is perfect!"): GOTO 3377
	IF rn <= 90 THEN CALL PBP ("This will be close"): GOTO 3377
	CALL PBP ("Close play.....")
	IF I8 < 4 THEN F$=U$(I8 + 1)
	IF I8 = 4 THEN F$=U$(1)
	CALL PBP (F$+" makes the call...")

3377 CALL CREDITASSIST (W%, D): CALL ADVANCEMENT (p, D, S2%, F%,I3): CALL CREDITPUTOUT(2, FB2P%, AST%, D): W% = 2
	IF O%(0) = 3 OR S2% > 10 THEN 3385

3371 I5 = INT(RND(1) * 100) + 1
	A1 = S6%(p, 0) - S6%(D, 0)
	IF A1 < -2 OR A1 < 0 AND INNING% > 8 THEN 3376
	'*** RUNNER ON 2ND GUNNED AT 3RD ON THROW HOME ***
	IF I8 - 1 > 2 AND I5 <= 15 AND B0%(2, 0) > 0 THEN CALL CREDITASSIST (W%, D): CALL PBP ("runner trying for 3rd..."):A5%(2) = 11: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 5: CALL CREDITREGPO(W%, D): I5 = P1%(D): I6 = 0: CALL ADDPIT(D, I5, I6)
	I5 = INT(RND(1) * 100) + 1
	'*** RUNNER ON 1ST GUNNED AT 2ND ON THROW TO HOME OR 3RD ***

IF I8-1>1 AND I5<=15 AND B0%(1, 0) > 0 AND A5%(2) <> 11 THEN CALL CREDITASSIST(W%, D): W% = 6: CALL CREDITREGPO(W%, D):CALL PBP("runner trying for 2nd..."): A5%(1) = 11: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5=P1%(D):I6=0:CALL ADDPIT(D, I5, I6)
	I5 = INT(RND(1) * 100) + 1
	IF B0%(2,0)>0 AND I8 - 1 > 2 AND A5%(2) <> 11 AND I5 > (80 - b%(p, B3%(p, B0%(2, 0)), 16) * 2) AND i9 = 0 THEN A5%(2) = 1:CALL PBP ("runner trying for 3rd..."):CALL PBP ("He's safe!")
	I5 = INT(RND(1) * 10) + 1
	IF B0%(1,0)>0 AND A5%(1)<>11 AND (S4%=4 OR S4%=7 AND I8-1>1 AND I5>(10 - b%(p, B3%(p, B0%(1, 0)), 16)) AND i9 = 0) THEN A5%(1) = 1: CALL PBP ("runner trying for 2nd..."):CALL PBP ("He's in there!"): GOTO 3376
	I5 = INT(RND(1) * 100) + 1
	IF B0%(1,0)>0 AND I8-1>1 AND I5>(80-b%(p,B3%(p,B0%(1, 0)),16)*2) AND i9=0 AND A5%(1)<>11 THEN CALL PBP ("runner safe at 2nd on the throw"): A5%(1) = 1

3376 CALL ADVANCEMENT(p, D, S2%, F%,I3): i9 = 0: GOTO 3385

3380 A5%(I8 - 1) = 1: IF i9 = 1 THEN 3379
	CALL RANDNUM (rn)
	IF rn <= 4 AND I8 = 4 THEN CALL PBP (Q$(I8 - 1)+" collides with "+b$(D, B9%(D, 2))):CALL PBP ("He knocks the ball loose!!"): GOTO 3379
	IF rn <= 50 AND I8 <> 4 THEN CALL PBP (Q$(I8 - 1)+" beats the throw!"): GOTO 3379
	IF rn <= 60 THEN CALL PBP (Q$(I8 - 1)+" is ahead of the throw"): GOTO 3379
	IF rn <= 80 THEN CALL PBP ("throw is off line"): GOTO 3379
	IF rn <= 90 THEN CALL PBP (Q$(I8 - 1)+" slides under the tag"): GOTO 3379
	CALL PBP ("Close play...")
	IF I8 <> 4 THEN F$=U$(I8 + 1)
	IF I8 = 4 THEN F$=U$(1)
	CALL PBP (F$+" makes the call...")

3379 CALL PBP (Q$(I8 - 1)+" is safe @ "+H$(I8))
	IF I8 = 4 THEN sah% = 1
	CALL TAGADVANCE(S2%, I8, D): IF S2% < 10 THEN 3371
	IF S2% > 10 AND B7%(p, B1!(p)) = 1 THEN P8%(p, P1%(p), 23) = P8%(p, P1%(p), 23) - 1: P5%(p, P1%(p), 23) = P5%(p, P1%(p), 23) - 1: I5 = P1%(p): I6 = 41: CALL INCPITOFF(P, I5, I6): I5 = P1%(D): I6 = 16: CALL ADDPIT(D, I5, I6): GOTO 3385
	IF S2% > 10 AND B7%(p, B1!(p)) <> 1 THEN 3381
	GOTO 3385

3381 B8%(p, B3%(p, B1!(p)), 0) = B8%(p, B3%(p, B1!(p)), 0) - 1
	B5%(p, B3%(p, B1!(p)), 0) = B5%(p, B3%(p, B1!(p)), 0) - 1
	IF P%(D,P1%(D),0) = 1 THEN 3382
	CSS%(P, B3%(p, B1!(p)), 0) = CSS%(P, B3%(p, B1!(p)), 0) - 1

3382 I5 = B3%(p, B1!(p)): I6 = 19: CALL INCBATOFF(P, I5, I6): I5 = P1%(D)
	I6 = 16: CALL ADDPIT(D, I5, I6): GOTO 3371

3385 '
END SUB

SUB VIEWTEAM ()
	CALL SCREEN0(0)

700 CLS
	COLOR 12: PRINT TAB(20); "SELECT TEAM TO VIEW DETAILED STATS FOR:": PRINT : PRINT
	COLOR 14, 0: PRINT "(0) "; : COLOR l%(0, 11), l%(0, 12): PRINT A$(0): PRINT
	COLOR 14, 0: PRINT "(1) "; : COLOR l%(1, 11), l%(1, 12): PRINT A$(1): PRINT
	COLOR 14, 0: PRINT "(2) "; : COLOR 15: PRINT "RETURN TO PLAY"
701 i$ = GetKeyPress$
	WK = VAL(i$): IF WK < 0 OR WK > 2 THEN 701
	IF WK = 2 THEN 850
	CLS
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "VIEW PITCHERS": PRINT
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "VIEW HITTERS": PRINT
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "RETURN TO PLAY"
706 i$ = GetKeyPress$
	Q = VAL(i$): IF Q < 1 OR Q > 3 THEN 706
	IF Q = 3 THEN 850
	IF Q = 1 THEN 800

725 CLS
	COLOR 12: PRINT "VIEW BATTERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR i = 0 TO 10: COLOR 3, 0
	IF b%(WK, i, 21) = 0 THEN COLOR 11
	LOCATE i + 5, 5: PRINT USING "##"; i; : PRINT " "; b$(WK, i); TAB(40);
	COLOR 3
	IF b%(WK, i + 12, 21) = 0 THEN COLOR 11
	PRINT USING "##"; i + 12; : PRINT " "; b$(WK, i + 12): COLOR 3: NEXT
	IF b%(WK, 11, 21) = 0 THEN COLOR 11
	LOCATE , 5: PRINT USING "##"; 11; : PRINT " "; b$(WK, 11)
	COLOR 3

730 COLOR 15: LOCATE 20, 1: PRINT "# TO VIEW (0-22) (23 TO CONTINUE) "; : INPUT M
	IF M < 0 OR M > 23 THEN 730
	IF M = 23 THEN 700
	LOCATE 3, 3: COLOR l%(WK, 11), l%(WK, 12): PRINT " "; CHR$(201); STRING$(71, 205); CHR$(187); " "
	FOR X = 1 TO 7: LOCATE , 3: PRINT " "; CHR$(186); STRING$(71, 32); CHR$(186); " ": NEXT
	LOCATE , 3: PRINT " "; CHR$(200); STRING$(71, 205); CHR$(188); " "
	LOCATE 4, 6: PRINT "Stats for ";
	PRINT b$(WK, M): LOCATE , 6: PRINT "BATS: "; B1$(b%(WK, M, 0) + 2); "    "; : PRINT USING "G/A%: ##       BUNT: #       RUN: #"; b%(WK, M, 18); b%(WK, M, 30); b%(WK, M, 16)
	LOCATE , 3: PRINT " "; CHR$(204); STRING$(71, 205); CHR$(185); " "
	LOCATE , 6: PRINT "          AVG   OBP   G  AB   R   H 2B 3B HR RBI  BB  SO  SB CS HS LS"
	CALL POPUPBAT(WK, M)
790 i$ = GetKeyPress$
	COLOR 7, 0: GOTO 725

800 CLS
	COLOR 12: PRINT "VIEW PITCHERS (AVAILABLE PLAYERS HIGHLIGHTED)"
	FOR i = 0 TO 10: COLOR 3, 0
	IF p%(WK, i, 15) = 0 THEN COLOR 11
	LOCATE i + 5, 5: PRINT USING "##"; i; : PRINT " "; p$(WK, i); TAB(40);
	COLOR 3
	IF p%(WK, i + 11, 15) = 0 THEN COLOR 11
	PRINT USING "##"; i + 11; : PRINT " "; p$(WK, i + 11): COLOR 3: NEXT

801 COLOR 15: LOCATE 20, 1: PRINT "# TO VIEW (0-21) (22 TO CONTINUE) "; : INPUT M
	IF M < 0 OR M > 22 THEN 801
	IF M = 22 THEN 700
	CALL POPUPPIT(WK, M)
830 i$ = GetKeyPress$
	COLOR 7, 0: GOTO 800

850 '
END SUB

SUB WALLDOUBLE (HW%, W%)
	'*** doubles off of a wall ***
	BV = INT(RND(1) * 100) + 1: rn = INT(RND(1) * 100) + 1

3781 IF BV <= 40 AND HW% = 0 THEN F$="It's in the gap and to the wall": GOTO 3785
	IF BV <= 65 AND rn <= 25 AND (l%(1, 13) = 8 OR l%(1, 13) = 40) THEN F$="It's off the ivy!": GOTO 3785
	IF BV <= 65 AND rn <= 50 AND l%(1, 13) = 21 AND W% = 7 THEN F$="It hits off the Green Monster!": GOTO 3785
	IF BV <= 65 AND l%(1, 13) = 27 AND W% = 7 THEN F$="It hits off the screen!": GOTO 3785
	IF BV <= 65 THEN F$="It's off the wall": GOTO 3785
	IF BV <= 85 AND HW% = 0 THEN 3788
	IF BV <= 90 AND HW% = 0 THEN CALL PBP ("It's to the wall"):CALL PBP (Q$(0)+" slides into second"):F$="It's a stand-up double!!": GOTO 3785
	IF rn <= 25 AND l%(1, 13) = 21 THEN F$="It's off the Green Monster!": GOTO 3785
	IF l%(1, 13) = 27 AND W% = 7 THEN F$="It's off the screen...": GOTO 3785
	F$="It's off the top of the wall...": GOTO 3785

3788 IF W% = 8 THEN BV = 39: GOTO 3781
	CALL PBP ("It's past "+Q$(4)):F$="It goes into the corner...": GOTO 3785

3785 CALL PBP (F$)
END SUB

SUB WHICHRUNNER (i, S4%, D5)
	IF i = 4 THEN A5%(3) = 1
	IF S4% <> 4 THEN 4297
	CALL PBP ("WHICH RUNNER DO YOU WANT TO GO AFTER")
	CALL PBP ("1 - "+b$(p, B3%(p, B0%(2, 0))))
	IF B7%(p, B0%(1, 0)) <> 1 THEN CALL PBP ("2 - "+b$(p, B3%(p, B0%(1, 0))))
	IF B7%(p, B0%(1, 0)) = 1 THEN CALL PBP ("2 - "+ p$(p, P1%(p)))
	IF u6 = 2 OR u6 = 1 AND p <> u9 THEN D5 = INT(RND(1) * 2) + 1: i$ = STR$(D5): GOTO 4296
4295 i$ = GetKeyPress$

4296 D5 = VAL(i$)
	IF D5 = 2 THEN i = 2: D5 = 3: I2 = B3%(p, B0%(1, 0)): IF B7%(p, B0%(1, 0)) = 1 THEN Q$(i - 1) = p$(p, P1%(p))
	D5 = 0

4297 '
END SUB

SUB WHOSON1ST (S4%)
	S4% = 0: I1 = B0%(1, 0): I2 = B0%(2, 0): I3 = B0%(3, 0)
	IF I1 > 0 AND I2 = 0 AND I3 = 0 THEN S4% = 1
	IF I1 = 0 AND I2 > 0 AND I3 = 0 THEN S4% = 2: REM*** RUNNER ON 2ND ***
	IF I1 = 0 AND I2 = 0 AND I3 > 0 THEN S4% = 3: REM*** RUNNER ON 3RD ***
	IF I1 > 0 AND I2 > 0 AND I3 = 0 THEN S4% = 4: REM*** 1ST AND 2ND ***
	IF I1 > 0 AND I2 = 0 AND I3 > 0 THEN S4% = 5: REM*** 1ST AND 3RD ***
	IF I1 = 0 AND I2 > 0 AND I3 > 0 THEN S4% = 6: REM*** 2ND AND 3RD ***
	IF I1 > 0 AND I2 > 0 AND I3 > 0 THEN S4% = 7: REM*** BASES LOADED ***
END SUB

SUB PBP (F$)

IF G4<>0 THEN
	CALL SPA (F$,44)
	F$=LEFT$(F$,44)
	PB%=PB%+1
	IF PB%<9 THEN G$(PB%)=F$
	IF PB%>8 THEN FOR I9=1 TO 7:G$(I9)=G$(I9+1):NEXT:G$(8)=F$
	FOR I9=1 TO 8
	GPRINT0VE I9+3,20,G$(I9),15
	NEXT
	CALL DELAY
ENDIF
END SUB

SUB FOULBALL ()
	call randnum(rn)
	if rn<=50 then
		Y% = INT(RND(1) * 20) + 3130
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		CALL PBP(Y$)
		CALL PBP("foul ball!!")
	else
		Y% = INT(RND(1) * 10) + 3150
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		CALL PBP(Y$)
		Y% = INT(RND(1) * 10) + 3160
		CALL GETRPBP (Y%,Y$,D): CALL STRIPRPBP (Y$,Y%,D)
		CALL PBP(Y$)
	end if
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
