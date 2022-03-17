'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE SUB TITLE ()

DIM T(1), A$(1), L%(1, 14), B$(2, 23), B%(2, 23, 79), P$(2, 22), P%(2, 22, 88)
DIM D$(2), Q$(2056), FS%(22, 4, 4), SS%(1, 22, 6)
DIM A1$(1), W2%(1), L2%(1), B1$(2, 23), B3%(2, 23), P1$(2, 22), P3%(2, 22), B0%(2, 22, 22), P0%(2, 22, 42)
DIM D0%(1), D1%(1), TEAM%(1, 22), P4%(1, 14), T1%(2, 23)
DIM K9(1) AS LONG, S(1), YR$(2), SA$(2)
DIM TS%(1, 11), SA%(1, 24)
DIM Y AS LONG
DIM X$(4), C$(9), ITEM$(4), VV$(200), V%(200), M$(2), S$(2), NN$(2)
	X$(0) = "LP": X$(1) = "L": X$(2) = "S": X$(3) = "R": X$(4) = "RP"
	C$(0) = "DH": C$(1) = "P": C$(2) = "C": C$(3) = "1B": C$(4) = "2B": C$(5) = "3B": C$(6) = "SS": C$(7) = "LF": C$(8) = "CF": C$(9) = "RF"

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR R = 0 TO 2
		INPUT #1, D$(R)
	NEXT R
CLOSE #1

50 CLS
	CALL TITLE
	ITEM$(1) = "CHOOSE TEAMS FOR TRADING PLAYERS"
	ITEM$(2) = "RETURN TO MAIN MENU             "
	SELECTION% = 1
	Count% = 2
	Start% = 1
	SCANCODE% = 2
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 6
	Column% = 24

CALL box0(5, 23, 8, 56, 2, 31)	
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN END
	I1 = SELECTION%
	IF I1 = 1 THEN 75
	CLS

70 RUN "HELLO.EXE"

75 J = 0
	COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM 1: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 36: INPUT NY$
	IF NY$ = "" AND YR$(0) <> "" THEN GOSUB 1102: GOTO 77
	YR$(0) = NY$
	FOR I = 1 TO 200: VV$(I) = "": NEXT

76  GOSUB 1100

77  IF BO% = 1 THEN BO% = 0: GOTO 50

78  LOCATE 2, 33: COLOR 15, 0: COLOR L%(J, 11), L%(J, 12)
	PRINT A$(J): COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 75
	COLOR 15, 0
	GOTO 140

1100 NT = 0
	IF NOT _FILEEXISTS(D$(0) + "FCTEAMS." + YR$(J)) THEN BO% = 1: GOSUB 4500: RETURN

OPEN D$(0) + "FCTEAMS." + YR$(J) FOR RANDOM AS #1 LEN = 4342
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0)
	FOR I1 = 1 TO Y / 4342
	GET #1, I1
	FOR I2 = 1 TO 15
	IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
	NEXT I2
	NT = NT + 1
	VV$(NT) = Q$(0): V%(NT) = I1
1103 NEXT I1
CLOSE 1

1102 SELECTION% = 1
	Count% = NT
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 4
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1

	COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN BO% = 1: RETURN
	T(J) = V%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
	 OPEN D$(0) + "FCTEAMS." + YR$(J) FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
FOR I = 0 TO 22
FIELD #1, 45 + I * 99 AS X$, 15 AS Q$(I * 43 + 15), 2 AS Q$(I * 43 + 16), 2 AS Q$(I * 43 + 17), 2 AS Q$(I * 43 + 18), 2 AS Q$(I * 43 + 19), 2 AS Q$(I * 43 + 20), 2 AS Q$(I * 43 + 21), 2 AS Q$(I * 43 + 22), 2 AS Q$(I * 43 + 23), 2 AS Q$(I * 43 + 24) _
, 2 AS Q$(I * 43 + 25), 2 AS Q$(I * 43 + 26), 4297 - 37 - I * 99 AS X$
FIELD #1, 82 + I * 99 AS X$, 2 AS Q$(I * 43 + 27), 2 AS Q$(I * 43 + 28), 2 AS Q$(I * 43 + 29), 2 AS Q$(I * 43 + 30), 2 AS Q$(I * 43 + 31), 2 AS Q$(I * 43 + 32), 2 AS Q$(I * 43 + 33), 2 AS Q$(I * 43 + 34), 2 AS Q$(I * 43 + 35), 2 AS Q$(I * 43 + 36),  _

2 AS Q$(I * 43 + 37), 2 AS Q$(I * 43 + 38), 4297 - 61 - I * 99 AS X$
FIELD #1, 106 + I * 99 AS X$, 2 AS Q$(I * 43 + 39), 2 AS Q$(I * 43 + 40), 2 AS Q$(I * 43 + 41), 2 AS Q$(I * 43 + 42), 2 AS Q$(I * 43 + 43), 2 AS Q$(I * 43 + 44), 2 AS Q$(I * 43 + 45), 2 AS Q$(I * 43 + 46), 2 AS Q$(I * 43 + 47), 2 AS Q$(I * 43 + 48) _
, 2 AS Q$(I * 43 + 49), 2 AS Q$(I * 43 + 50), 4297 - 85 - I * 99 AS X$
FIELD #1, 130 + I * 99 AS X$, 2 AS Q$(I * 43 + 51), 2 AS Q$(I * 43 + 52), 2 AS Q$(I * 43 + 53), 2 AS Q$(I * 43 + 54), 2 AS Q$(I * 43 + 55), 2 AS Q$(I * 43 + 56), 2 AS Q$(I * 43 + 57), 4297 - 99 - I * 99 AS X$
NEXT
FOR I = 0 TO 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
FIELD #1, 2407 + I * 89 AS X$, 2 AS Q$(I * 38 + 1040), 2 AS Q$(I * 38 + 1041), 2020 - 89 - I * 89 AS X$
NEXT
FIELD #1, 4280 AS X$, 40 AS Q$(1840), 15 AS Q$(1841), 5 AS Q$(1842), 2 AS Q$(1843)
GET #1, T(J)
A$(J) = RTrim$(Q$(0))
	FOR I1 = 1 TO 13: L%(J, I1) = CVI(Q$(I1)): NEXT
	K9(J) = CVS(Q$(14))
	FOR I1 = 0 TO 22: B$(J, I1) = RTrim$(Q$(I1 * 43 + 15))
	FOR I2 = 0 TO 41: B%(J, I1, I2) = CVI(Q$(I1 * 43 + I2 + 16))
	NEXT: NEXT
	FOR I1 = 0 TO 21: P$(J, I1) = RTrim$(Q$(I1 * 38 + 1004))
	FOR I2 = 0 TO 36: P%(J, I1, I2) = CVI(Q$(I1 * 38 + I2 + 1005))
	NEXT: NEXT
	S$(J) = RTrim$(Q$(1840))
	M$(J) = RTrim$(Q$(1841))
	NN$(J) = RTrim$(Q$(1842))
	CK = CVI(Q$(1843))
CLOSE 1
	IF NOT _FILEEXISTS(D$(0) + "LR." + YR$(J)) THEN 1110
OPEN D$(0) + "LR." + YR$(J) FOR RANDOM AS #1 LEN = 4108
	FOR X = 0 TO 11
	FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 4108 - 2 - 2 * X AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 1 TO 24
	FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	GET #1, T(J)
	FOR X = 0 TO 11
	TS%(J, X) = CVI(Q$(X + 1))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	B%(J, X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(J, X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(J, X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 1 TO 24
	SA%(J, X) = CVI(Q$(X + 950))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	B%(J, X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	P%(J, X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
	NEXT: NEXT
CLOSE 1
	IF TS%(J, 0) = 999 THEN 1110
	FOR X = 0 TO 22
	FOR X1 = 42 TO 79
	B%(J, X, X1) = 0
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 37 TO 88
	P%(J, X, X1) = 0
	NEXT: NEXT
	FOR X = 1 TO 24
	SA%(J, X) = 0
	NEXT
	FOR X = 0 TO 11
	TS%(J, X) = 0
	NEXT

1110 RETURN

140 CLS
	PRINT "DOES "; : COLOR L%(J, 11), L%(J, 12)
	PRINT A$(0); : COLOR 15, 0
	PRINT " HAVE COMPILED STATS (YN)": S(0) = 0
145 I$ = GetKeyPress$
	IF UCASE$(I$) = "N" THEN S(0) = 1: YR$(1) = YR$(0): GOTO 205
	CLS

155 GOSUB 160
	IF NF = 1 THEN NF = 0: S(0) = 1
	YR$(1) = YR$(0)
	GOTO 205

160 GOSUB 1005
	F$ = MID$(SA$(J), 4, QQ)
	R$ = F$ + "." + LEFT$(SA$(J), 2)
	IF NOT _FILEXIST(D$(1) + R$) THEN GOSUB 4550: NF = 1: GOTO 204
OPEN D$(1) + R$ FOR INPUT AS #1
	INPUT #1, A1$(J): INPUT #1, W2%(J): INPUT #1, L2%(J)
	FOR I1 = 0 TO 22: INPUT #1, B1$(J, I1): INPUT #1, B3%(J, I1): NEXT I1
	FOR I1 = 0 TO 21: INPUT #1, P1$(J, I1): INPUT #1, P3%(J, I1): NEXT I1
	FOR I1 = 0 TO 22: FOR I2 = 0 TO 21: INPUT #1, B0%(J, I1, I2): NEXT I2: NEXT I1
	FOR I1 = 0 TO 21: FOR I2 = 0 TO 41: INPUT #1, P0%(J, I1, I2): NEXT I2: NEXT I1
	INPUT #1, D0%(J): INPUT #1, D1%(J)
	FOR I = 0 TO 21: INPUT #1, TEAM%(J, I): NEXT I
	FOR I = 0 TO 22: INPUT #1, T1%(J, I): NEXT I
CLOSE #1
	IF NOT _FILEXIST(D$(1) + R$ + "R") THEN 204
OPEN D$(1) + R$ + "R" FOR INPUT AS #1
	FOR I1 = 0 TO 22
	FOR I2 = 0 TO 6
	INPUT #1, SS%(J, I1, I2)
	NEXT I2: NEXT I1
CLOSE #1

204 RETURN

205 J = 1
	COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID FOR TEAM 2: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 36: INPUT NY$
	IF NY$ = "" AND YR$(1) <> "" THEN GOSUB 1102: GOTO 277
	YR$(1) = NY$
	FOR I = 1 TO 200: VV$(I) = "": NEXT

276  GOSUB 1100

277  IF BO% = 1 THEN BO% = 0: GOTO 50

278  LOCATE 2, 33: COLOR 15, 0: COLOR L%(J, 11), L%(J, 12)
	PRINT A$(J): COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)"
	I$=GetKeyPress
	IF UCASE$(I$) = "Y" THEN 205
	COLOR 15, 0

235 CLS
	PRINT "DOES "; : COLOR L%(J, 11), L%(J, 12)
	PRINT A$(1); : COLOR 15, 0
	PRINT " HAVE COMPILED STATS (YN)": S(1) = 0
240 I$ = GetKeyPress$
	IF UCASE$(I$) = "N" THEN S(1) = 1: GOTO 260
	CLS

255 GOSUB 160
	IF NF = 1 THEN NF = 0: S(1) = 1

260 CLS : COLOR 12: PRINT TAB(35); "TRADE MENU": PRINT
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "TRADE BATTERS": PRINT
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "TRADE PITCHERS": PRINT
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "END TRADING AND SAVE TRADES": PRINT
	COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "END TRADING WITHOUT SAVING TRADES"
265 I$ = GetKeyPress$
	IF I$ = "4" THEN 50
	IF I$ = "3" THEN 555
	IF I$ = "1" THEN GOSUB 290: GOTO 325
	IF I$ = "2" THEN GOSUB 290: GOTO 440
	GOTO 260

290 IF S(0) = 1 OR S(1) = 1 THEN 320
	CLS : PRINT : PRINT
	COLOR 14: PRINT "(T) "; : COLOR 15: PRINT "TRADE PLAYERS' COMPILED STATS": PRINT
	COLOR 14: PRINT "(Z) "; : COLOR 15: PRINT "ZERO PLAYERS' COMPILED STATS"
300 I$ = GetKeyPress$
	IF UCASE$(I$) <> "T" AND UCASE$(I$) <> "Z" THEN 300
	IF UCASE$(I$) = "T" THEN W1 = 1
	IF UCASE$(I$) = "Z" THEN W1 = 0

320 RETURN

325 CLS : GOSUB 6000: COLOR 15, 0: LOCATE 1: PRINT A$(0); : LOCATE , 40: PRINT A$(1)
	FOR I = 0 TO 16
	COLOR 15, 0: PRINT USING "##"; I; : PRINT " ";
	COLOR L%(0, 11), L%(0, 12): PRINT B$(0, I); : COLOR 15, 0: LOCATE , 40
	PRINT USING "##"; I; : PRINT " "; : COLOR L%(1, 11), L%(1, 12): PRINT B$(1, I)
	NEXT I
	FOR I = 17 TO 22
	COLOR 15, 0: LOCATE I - 15, 20: PRINT USING "##"; I; : PRINT " ";
	COLOR L%(0, 11), L%(0, 12): PRINT B$(0, I); : COLOR 15, 0: LOCATE , 60
	PRINT USING "##"; I; : PRINT " "; : COLOR L%(1, 11), L%(1, 12): PRINT B$(1, I)
	NEXT I

335 COLOR 15, 0: LOCATE 20, 1: GOSUB 1065: GOSUB 1065: GOSUB 1065
	LOCATE 20, 1: PRINT A$(0); " BATTER: ";

345 INPUT I$
	I = VAL(I$): IF I < 0 OR I > 22 THEN 345
	PRINT B$(0, I)
	LOCATE 21, 1: PRINT A$(1); " BATTER: ";

365 INPUT I$
	I1 = VAL(I$): IF I1 < 0 OR I1 > 22 THEN 365
	PRINT B$(1, I1)
	LOCATE 22, 1: PRINT "ANY CHANGE (YN) ";
385 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 335
	GOSUB 665
	IF TS%(0, 0) = 999 AND TS%(1, 0) = 999 THEN 386
	IF TS%(0, 0) <> 999 AND TS%(1, 0) <> 999 THEN 386
	IF TS%(0, 0) = 999 AND TS%(1, 0) <> 999 THEN FOR I2 = 0 TO 11: TS%(1, I2) = TS%(0, I2): NEXT: FOR I2 = 1 TO 24: SA%(1, I2) = SA%(0, I2): NEXT: GOTO 386
	IF TS%(1, 0) = 999 AND TS%(0, 0) <> 999 THEN FOR I2 = 0 TO 11: TS%(0, I2) = TS%(1, I2): NEXT: FOR I2 = 1 TO 24: SA%(0, I2) = SA%(1, I2): NEXT

386 SWAP B$(0, I), B$(1, I1)
	FOR I2 = 0 TO 79
	SWAP B%(0, I, I2), B%(1, I1, I2)
	NEXT I2
	FOR I2 = 0 TO 6
	SWAP SS%(0, I, I2), SS%(1, I1, I2)
	NEXT I2
	B1$(0, I) = B$(0, I): B1$(1, I1) = B$(1, I1)
	SWAP B3%(0, I), B3%(1, I1)
	IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 430
	FOR I2 = 0 TO 21: SWAP B0%(0, I, I2), B0%(1, I1, I2): NEXT I2
	GOSUB 685: GOTO 260

430 FOR I2 = 0 TO 21: B0%(0, I, I2) = 0: B0%(1, I1, I2) = 0: NEXT I2
	GOSUB 685: GOTO 260

440 CLS : GOSUB 7000: COLOR 15, 0: LOCATE 1: PRINT A$(0); : LOCATE , 40: PRINT A$(1)
	FOR I = 0 TO 11
	COLOR 15, 0: PRINT USING "##"; I; : PRINT " ";
	COLOR L%(0, 11), L%(0, 12): PRINT P$(0, I); : COLOR 15, 0
	LOCATE , 40: PRINT USING "##"; I; : PRINT " "; : COLOR L%(1, 11), L%(1, 12)
	PRINT P$(1, I)
	NEXT I
	FOR I = 12 TO 21
	COLOR 15, 0: LOCATE I - 10, 20: PRINT USING "##"; I; : PRINT "  ";
	COLOR L%(0, 11), L%(0, 12): PRINT P$(0, I); : LOCATE I - 10, 40: COLOR 15, 0
	LOCATE , 60: PRINT USING "##"; I; : PRINT "  "; : COLOR L%(1, 11), L%(1, 12)
	PRINT P$(1, I)
	NEXT I

450  COLOR 15, 0: LOCATE 20, 1: GOSUB 1065: GOSUB 1065: GOSUB 1065
	LOCATE 20, 1: PRINT A$(0); " PITCHER: ";

460  INPUT I$
	I = VAL(I$): IF I < 0 OR I > 21 THEN 460
	PRINT P$(0, I)
	LOCATE 21, 1: PRINT A$(1); " PITCHER: ";

480  INPUT I$
	I1 = VAL(I$): IF I1 < 0 OR I1 > 21 THEN 480
	PRINT P$(1, I1)
	LOCATE 22, 1: PRINT "ANY CHANGE (YN) ";

500  I$=GetKeyPress
	IF UCASE$(I$) = "Y" THEN 450
	GOSUB 705
	IF TS%(0, 0) = 999 AND TS%(1, 0) = 999 THEN 505
	IF TS%(0, 0) <> 999 AND TS%(1, 0) <> 999 THEN 505
	IF TS%(0, 0) = 999 AND TS%(1, 0) <> 999 THEN FOR I2 = 0 TO 11: TS%(1, I2) = TS%(0, I2): NEXT: FOR I2 = 1 TO 24: SA%(1, I2) = SA%(0, I2): NEXT: GOTO 505
	IF TS%(1, 0) = 999 AND TS%(0, 0) <> 999 THEN FOR I2 = 0 TO 11: TS%(0, I2) = TS%(1, I2): NEXT: FOR I2 = 1 TO 24: SA%(0, I2) = SA%(1, I2): NEXT

505  SWAP P$(0, I), P$(1, I1)
	FOR I2 = 0 TO 88: SWAP P%(0, I, I2), P%(1, I1, I2): NEXT I2
	P1$(0, I) = P$(0, I): P1$(1, I1) = P$(1, I1)
	IF S(0) = 1 OR S(1) = 1 OR W1 = 0 THEN 545
	SWAP P3%(0, I), P3%(1, I1)
	FOR I2 = 0 TO 41: SWAP P0%(0, I, I2), P0%(1, I1, I2): NEXT I2
	GOSUB 725: GOTO 260

545  FOR I2 = 0 TO 41: P0%(0, I, I2) = 0: P0%(1, I1, I2) = 0: NEXT I2
	GOSUB 725: GOTO 260

555  FOR I = 0 TO 1
	CLS : COLOR 15, 0
	IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 556

556  OPEN D$(0) + "FCTEAMS." + YR$(I) FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
FOR J = 0 TO 22
FIELD #1, 45 + J * 99 AS X$, 15 AS Q$(J * 43 + 15), 2 AS Q$(J * 43 + 16), 2 AS Q$(J * 43 + 17), 2 AS Q$(J * 43 + 18), 2 AS Q$(J * 43 + 19), 2 AS Q$(J * 43 + 20), 2 AS Q$(J * 43 + 21), 2 AS Q$(J * 43 + 22), 2 AS Q$(J * 43 + 23), 2 AS Q$(J * 43 + 24) _
, 2 AS Q$(J * 43 + 25), 2 AS Q$(J * 43 + 26), 4297 - 37 - J * 99 AS X$
FIELD #1, 82 + J * 99 AS X$, 2 AS Q$(J * 43 + 27), 2 AS Q$(J * 43 + 28), 2 AS Q$(J * 43 + 29), 2 AS Q$(J * 43 + 30), 2 AS Q$(J * 43 + 31), 2 AS Q$(J * 43 + 32), 2 AS Q$(J * 43 + 33), 2 AS Q$(J * 43 + 34), 2 AS Q$(J * 43 + 35), 2 AS Q$(J * 43 + 36),  _

2 AS Q$(J * 43 + 37), 2 AS Q$(J * 43 + 38), 4297 - 61 - J * 99 AS X$
FIELD #1, 106 + J * 99 AS X$, 2 AS Q$(J * 43 + 39), 2 AS Q$(J * 43 + 40), 2 AS Q$(J * 43 + 41), 2 AS Q$(J * 43 + 42), 2 AS Q$(J * 43 + 43), 2 AS Q$(J * 43 + 44), 2 AS Q$(J * 43 + 45), 2 AS Q$(J * 43 + 46), 2 AS Q$(J * 43 + 47), 2 AS Q$(J * 43 + 48) _
, 2 AS Q$(J * 43 + 49), 2 AS Q$(J * 43 + 50), 4297 - 85 - J * 99 AS X$
FIELD #1, 130 + J * 99 AS X$, 2 AS Q$(J * 43 + 51), 2 AS Q$(J * 43 + 52), 2 AS Q$(J * 43 + 53), 2 AS Q$(J * 43 + 54), 2 AS Q$(J * 43 + 55), 2 AS Q$(J * 43 + 56), 2 AS Q$(J * 43 + 57), 4297 - 99 - J * 99 AS X$
NEXT
FOR J = 0 TO 21
FIELD #1, 2322 + J * 89 AS X$, 15 AS Q$(J * 38 + 1004), 2 AS Q$(J * 38 + 1005), 2 AS Q$(J * 38 + 1006), 2 AS Q$(J * 38 + 1007), 2 AS Q$(J * 38 + 1008), 2 AS Q$(J * 38 + 1009), 2 AS Q$(J * 38 + 1010), 2 AS Q$(J * 38 + 1011), 2 AS Q$(J * 38 + 1012), 2 _
AS Q$(J * 38 + 1013), 2 AS Q$(J * 38 + 1014), 2 AS Q$(J * 38 + 1015), 2020 - 37 - J * 89 AS X$
FIELD #1, 2359 + J * 89 AS X$, 2 AS Q$(J * 38 + 1016), 2 AS Q$(J * 38 + 1017), 2 AS Q$(J * 38 + 1018), 2 AS Q$(J * 38 + 1019), 2 AS Q$(J * 38 + 1020), 2 AS Q$(J * 38 + 1021), 2 AS Q$(J * 38 + 1022), 2 AS Q$(J * 38 + 1023), 2 AS Q$(J * 38 + 1024), 2  _
AS Q$(J * 38 + 1025), 2 AS Q$(J * 38 + 1026), 2 AS Q$(J * 38 + 1027), 2020 - 61 - J * 89 AS X$
FIELD #1, 2383 + J * 89 AS X$, 2 AS Q$(J * 38 + 1028), 2 AS Q$(J * 38 + 1029), 2 AS Q$(J * 38 + 1030), 2 AS Q$(J * 38 + 1031), 2 AS Q$(J * 38 + 1032), 2 AS Q$(J * 38 + 1033), 2 AS Q$(J * 38 + 1034), 2 AS Q$(J * 38 + 1035), 2 AS Q$(J * 38 + 1036), 2  _
AS Q$(J * 38 + 1037), 2 AS Q$(J * 38 + 1038), 2 AS Q$(J * 38 + 1039), 2020 - 85 - J * 89 AS X$
FIELD #1, 2407 + J * 89 AS X$, 2 AS Q$(J * 38 + 1040), 2 AS Q$(J * 38 + 1041), 2020 - 89 - J * 89 AS X$
NEXT
FIELD #1, 4280 AS X$, 40 AS Q$(1840), 15 AS Q$(1841), 5 AS Q$(1842), 2 AS Q$(1843)
LSET Q$(0) = A$(I): FOR J = 1 TO 13: LSET Q$(J) = MKI$(L%(I, J)): NEXT: LSET Q$(14) = MKS$(K9(I))
FOR J = 0 TO 22: LSET Q$(J * 43 + 15) = B$(I, J)
FOR I1 = 0 TO 41: LSET Q$(J * 43 + I1 + 16) = MKI$(B%(I, J, I1))
NEXT: NEXT
FOR J = 0 TO 21: LSET Q$(J * 38 + 1004) = P$(I, J)
FOR I1 = 0 TO 36: LSET Q$(J * 38 + I1 + 1005) = MKI$(P%(I, J, I1))
NEXT: NEXT
LSET Q$(1840) = S$(I): LSET Q$(1841) = M$(I): LSET Q$(1842) = NN$(I): LSET Q$(1843) = MKI$(999)
PUT #1, T(I): CLOSE 1
	  SCK = 0
	FOR X = 1 TO 24
	SCK = SCK + SA%(I, X)
	NEXT
	IF SCK <= 0 THEN 1435
	TS%(I, 0) = 999
	FOR X = 1 TO 11
	TS%(I, X) = -1
	NEXT
OPEN D$(0) + "LR." + YR$(I) FOR RANDOM AS #1 LEN = 4108
	FOR X = 0 TO 11
	FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 4108 - 2 - 2 * X AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 1 TO 24
	FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 11: LSET Q$(X + 1) = MKI$(TS%(I, X)): NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	LSET Q$(12 + X1 + X * 14) = MKI$(B%(I, X, X1 + 41))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	LSET Q$(334 + X1 + X * 14) = MKI$(P%(I, X, X1 + 36))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	LSET Q$(642 + X1 + X * 14) = MKI$(P%(I, X, X1 + 50))
	NEXT: NEXT
	FOR X = 1 TO 24
	LSET Q$(X + 950) = MKI$(SA%(I, X))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	LSET Q$(975 + X1 + X * 24) = MKI$(B%(I, X, X1 + 55))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	LSET Q$(1528 + X1 + X * 24) = MKI$(P%(I, X, X1 + 64))
	NEXT: NEXT
	PUT #1, T(I)
CLOSE 1

1435 NEXT I
	 FOR I = 0 TO 1
	IF S(I) = 1 THEN 655
	CLS

557 GOSUB 1035
	F$ = MID$(SA$(I), 4, QQ): R$ = F$ + "." + LEFT$(SA$(I), 2)
OPEN D$(1) + R$ FOR OUTPUT AS #1
	PRINT #1, A1$(I): PRINT #1, W2%(I): PRINT #1, L2%(I)
	FOR I1 = 0 TO 22: PRINT #1, B1$(I, I1): PRINT #1, B3%(I, I1): NEXT I1
	FOR I1 = 0 TO 21: PRINT #1, P1$(I, I1): PRINT #1, P3%(I, I1): NEXT I1
	FOR I1 = 0 TO 22: FOR I2 = 0 TO 21: PRINT #1, B0%(I, I1, I2): NEXT I2: NEXT I1
	FOR I1 = 0 TO 21: FOR I2 = 0 TO 41: PRINT #1, P0%(I, I1, I2): NEXT I2: NEXT I1
	PRINT #1, D0%(I): PRINT #1, D1%(I)
	FOR I1 = 0 TO 21: PRINT #1, TEAM%(I, I1): NEXT I1
	FOR I1 = 0 TO 22: PRINT #1, T1%(I, I1): NEXT I1
CLOSE #1
OPEN D$(1) + R$ + "R" FOR OUTPUT AS #1
	FOR I1 = 0 TO 22: FOR I2 = 0 TO 6
	PRINT #1, SS%(I, I1, I2)
	NEXT I2: NEXT I1
CLOSE #1

655 NEXT I
	GOTO 50

665 GOSUB 745
	LOCATE 24, 1: PRINT "HIT ANY KEY TO TRADE";
675 I$ = GetKeyPress$
	RETURN

685 GOSUB 745
	LOCATE 24, 1: PRINT "HIT ANY KEY TO CONTINUE";
695 I$ = GetKeyPress$
	RETURN

705 GOSUB 905
	LOCATE 24, 1: PRINT "HIT ANY KEY TO TRADE";
715 I$ = GetKeyPress$
	RETURN

725 GOSUB 905
	LOCATE 24, 1: PRINT "HIT ANY KEY TO CONTINUE";
735 I$ = GetKeyPress$
	RETURN

745 CLS : COLOR L%(0, 11), L%(0, 12): PRINT A$(0); : LOCATE , 41: COLOR L%(1, 11), L%(1, 12): PRINT A$(1): COLOR 15, 0
	PRINT B$(0, I); " "; X$(B%(0, I, 0) + 2); " "; C$(B%(0, I, 22));
	IF B%(0, I, 23) >= 0 THEN PRINT ","; C$(B%(0, I, 23));
	IF B%(0, I, 24) >= 0 THEN PRINT ","; C$(B%(0, I, 24));
	IF B%(0, I, 25) >= 0 THEN PRINT ","; C$(B%(0, I, 25));
	LOCATE , 41:
	PRINT B$(1, I1); " "; X$(B%(1, I1, 0) + 2); " "; C$(B%(1, I1, 22));
	IF B%(1, I1, 23) >= 0 THEN PRINT ","; C$(B%(1, I1, 23));
	IF B%(1, I1, 24) >= 0 THEN PRINT ","; C$(B%(1, I1, 24));
	IF B%(1, I1, 25) >= 0 THEN PRINT ","; C$(B%(1, I1, 25));
	LOCATE , 1: PRINT : PRINT "REAL"; : LOCATE , 15: PRINT "COMP"; : LOCATE , 41: PRINT "REAL"; : LOCATE , 55: PRINT "COMP": LOCATE , 1
	LOCATE , 1: PRINT B%(0, I, 3); : LOCATE , 10: PRINT "G"; : LOCATE , 15: PRINT B0%(0, I, 14); : LOCATE , 41: PRINT B%(1, I1, 3); : LOCATE , 50: PRINT "G"; : LOCATE , 55: PRINT B0%(1, I1, 14)
	LOCATE , 1: PRINT B%(0, I, 4); : LOCATE , 10: PRINT "AB"; : LOCATE , 15: PRINT B0%(0, I, 0); : LOCATE , 41: PRINT B%(1, I1, 4); : LOCATE , 50: PRINT "AB"; : LOCATE , 55: PRINT B0%(1, I1, 0)
	LOCATE , 1: PRINT B%(0, I, 5); : LOCATE , 10: PRINT "R"; : LOCATE , 15: PRINT B0%(0, I, 2); : LOCATE , 41: PRINT B%(1, I1, 5); : LOCATE , 50: PRINT "R"; : LOCATE , 55: PRINT B0%(1, I1, 2)
	LOCATE , 1: PRINT B%(0, I, 6); : LOCATE , 10: PRINT "H"; : LOCATE , 15: PRINT B0%(0, I, 1); : LOCATE , 41: PRINT B%(1, I1, 6); : LOCATE , 50: PRINT "H"; : LOCATE , 55: PRINT B0%(1, I1, 1)
	LOCATE , 1: PRINT B%(0, I, 10); : LOCATE , 9: PRINT "RBI"; : LOCATE , 15: PRINT B0%(0, I, 3); : LOCATE , 41: PRINT B%(1, I1, 10); : LOCATE , 49: PRINT "RBI"; : LOCATE , 55: PRINT B0%(1, I1, 3)
	LOCATE , 1: PRINT B%(0, I, 7); : LOCATE , 10: PRINT "DB"; : LOCATE , 15: PRINT B0%(0, I, 4); : LOCATE , 41: PRINT B%(1, I1, 7); : LOCATE , 50: PRINT "DB"; : LOCATE , 55: PRINT B0%(1, I1, 4)
	LOCATE , 1: PRINT B%(0, I, 8); : LOCATE , 10: PRINT "TR"; : LOCATE , 15: PRINT B0%(0, I, 5); : LOCATE , 41: PRINT B%(1, I1, 8); : LOCATE , 50: PRINT "TR"; : LOCATE , 55: PRINT B0%(1, I1, 5)
	LOCATE , 1: PRINT B%(0, I, 9); : LOCATE , 10: PRINT "HR"; : LOCATE , 15: PRINT B0%(0, I, 6); : LOCATE , 41: PRINT B%(1, I1, 9); : LOCATE , 50: PRINT "HR"; : LOCATE , 55: PRINT B0%(1, I1, 6)
	LOCATE , 1: PRINT B%(0, I, 11); : LOCATE , 10: PRINT "BB"; : LOCATE , 15: PRINT B0%(0, I, 7); : LOCATE , 41: PRINT B%(1, I1, 11); : LOCATE , 50: PRINT "BB"; : LOCATE , 55: PRINT B0%(1, I1, 7)
	LOCATE , 1: PRINT B%(0, I, 12); : LOCATE , 10: PRINT "SO"; : LOCATE , 15: PRINT B0%(0, I, 8); : LOCATE , 41: PRINT B%(1, I1, 12); : LOCATE , 50: PRINT "S0"; : LOCATE , 55: PRINT B0%(1, I1, 8)
	LOCATE , 1: IF B%(0, I, 4) = 0 THEN PRINT "-.---"; : GOTO 845
	IF B%(0, I, 6) = B%(0, I, 4) THEN PRINT "1.000"; : GOTO 845
	PRINT USING " .###"; (B%(0, I, 6) / B%(0, I, 4));

845 LOCATE , 9: PRINT "BAVG"; : LOCATE , 15: IF B0%(0, I, 0) = 0 THEN PRINT "-.---"; : GOTO 860
	IF B0%(0, I, 0) = B0%(0, I, 1) THEN PRINT "1.000"; : GOTO 860
	PRINT USING " .###"; (B0%(0, I, 1) / B0%(0, I, 0));

860 LOCATE , 41: IF B%(1, I1, 4) = 0 THEN PRINT "-.---"; : GOTO 875
	IF B%(1, I1, 6) = B%(1, I1, 4) THEN PRINT "1.000"; : GOTO 875
	PRINT USING " .###"; (B%(1, I1, 6) / B%(1, I1, 4));

875 LOCATE , 49: PRINT "BAVG"; : LOCATE , 55: IF B0%(1, I1, 0) = 0 THEN PRINT "-.---": GOTO 890
	IF B0%(1, I1, 0) = B0%(1, I1, 1) THEN PRINT "1.000": GOTO 890
	PRINT USING " .###"; (B0%(1, I1, 1) / B0%(1, I1, 0))

890 LOCATE , 1: PRINT B%(0, I, 13); : LOCATE , 10: PRINT "SB"; : LOCATE , 15: PRINT B0%(0, I, 9); : LOCATE , 41: PRINT B%(1, I1, 13); : LOCATE , 50: PRINT "SB"; : LOCATE , 55: PRINT B0%(1, I1, 9)
	LOCATE , 1: PRINT B%(0, I, 14); : LOCATE , 10: PRINT "CS"; : LOCATE , 15: PRINT B0%(0, I, 10); : LOCATE , 41: PRINT B%(1, I1, 14); : LOCATE , 50: PRINT "CS"; : LOCATE , 55: PRINT B0%(1, I1, 10)
	RETURN

905 CLS : COLOR L%(0, 11), L%(0, 12): PRINT A$(0); : LOCATE , 41
	COLOR L%(1, 11), L%(1, 12): PRINT A$(1): COLOR 15, 0
	LOCATE , 1: PRINT P$(0, I); " "; X$(P%(0, I, 0) + 2); : LOCATE , 41: PRINT P$(1, I1); " "; X$(P%(1, I1, 0) + 2)
	LOCATE , 1: PRINT : PRINT "REAL"; : LOCATE , 15: PRINT "COMP"; : LOCATE , 41: PRINT "REAL"; : LOCATE , 55: PRINT "COMP"
	LOCATE , 1: PRINT P%(0, I, 1); "-"; P%(0, I, 2); : LOCATE , 9: PRINT "W-L"; : LOCATE , 15: PRINT P0%(0, I, 6); "-"; P0%(0, I, 7); : LOCATE , 41: PRINT P%(1, I1, 1); "-"; P%(1, I1, 2); : LOCATE , 49: PRINT "W-L"; : LOCATE , 55: PRINT P0%(1, I1, 6 _
); "-"; P0%(1, I1, 7)
	LOCATE , 1: PRINT P%(0, I, 4); : LOCATE , 10: PRINT "G"; : LOCATE , 15: PRINT P0%(0, I, 8); : LOCATE , 41: PRINT P%(1, I1, 4); : LOCATE , 50: PRINT "G"; : LOCATE , 55: PRINT P0%(1, I1, 8)
	LOCATE , 1: PRINT P%(0, I, 5); : LOCATE , 10: PRINT "GS"; : LOCATE , 15: PRINT P0%(0, I, 9); : LOCATE , 41: PRINT P%(1, I1, 5); : LOCATE , 50: PRINT "GS"; : LOCATE , 55: PRINT P0%(1, I1, 9)
	LOCATE , 1: PRINT P%(0, I, 3); : LOCATE , 10: PRINT "SV"; : LOCATE , 15: PRINT P0%(0, I, 12); : LOCATE , 41: PRINT P%(1, I1, 3); : LOCATE , 50: PRINT "SV"; : LOCATE , 55: PRINT P0%(1, I1, 12)
	LOCATE , 1: PRINT P%(0, I, 6); : LOCATE , 10: PRINT "IP"; : LOCATE , 15: PRINT INT((P0%(0, I, 0) / 3) + .5); : LOCATE , 41: PRINT P%(1, I1, 6); : LOCATE , 50: PRINT "IP"; : LOCATE , 55: PRINT INT((P0%(1, I1, 0) / 3) + .5)
	LOCATE , 1: PRINT P%(0, I, 7); : LOCATE , 10: PRINT "H"; : LOCATE , 15: PRINT P0%(0, I, 1); : LOCATE , 41: PRINT P%(1, I1, 7); : LOCATE , 50: PRINT "H"; : LOCATE , 55: PRINT P0%(1, I1, 1)
	LOCATE , 1: PRINT P%(0, I, 8); : LOCATE , 10: PRINT "BB"; : LOCATE , 15: PRINT P0%(0, I, 4); : LOCATE , 41: PRINT P%(1, I1, 8); : LOCATE , 50: PRINT "BB"; : LOCATE , 55: PRINT P0%(1, I1, 4)
	LOCATE , 1: PRINT P%(0, I, 9); : LOCATE , 10: PRINT "SO"; : LOCATE , 15: PRINT P0%(0, I, 5); : LOCATE , 41: PRINT P%(1, I1, 9); : LOCATE , 50: PRINT "SO"; : LOCATE , 55: PRINT P0%(1, I1, 5)
	LOCATE , 1: PRINT P%(0, I, 13); : LOCATE , 10: PRINT "HR"; : LOCATE , 15: PRINT P0%(0, I, 14); : LOCATE , 41: PRINT P%(1, I1, 13); : LOCATE , 50: PRINT "HR"; : LOCATE , 55: PRINT P0%(1, I1, 14)
	LOCATE , 1: PRINT USING "##.##"; P%(0, I, 10) / 100; : LOCATE , 9: PRINT "ERA"; : LOCATE , 15: IF P0%(0, I, 0) = 0 THEN PRINT " -.--"; : GOTO 965
	PRINT USING "##.##"; P0%(0, I, 3) * 27 / P0%(0, I, 0);

965 LOCATE , 41
	PRINT USING "##.##"; P%(1, I1, 10) / 100; : LOCATE , 49: PRINT "ERA"; : LOCATE , 55: IF P0%(1, I1, 0) = 0 THEN PRINT " -.--": GOTO 985
	PRINT USING "##.##"; P0%(1, I1, 3) * 27 / P0%(1, I1, 0)
	IF P0%(0, I, 0) > 0 THEN G1% = INT((((P0%(0, I, 3) * 9) / (P0%(0, I, 0) / 3)) + .0005) * 100)
	E1% = INT(G1% / 100): F1% = G1% - (E1% * 100)

985 RETURN

1005 SA$(J) = A$(J)
	FOR XX = 4 TO 11
	IF MID$(SA$(J), XX, 1) = " " OR MID$(SA$(J), XX, 1) = "." THEN MID$(SA$(J), XX, 1) = "_"
	NEXT
	QQ = 8
	FOR XX = 11 TO 4 STEP -1
	IF MID$(SA$(J), XX, 1) = "_" THEN QQ = XX - 4: GOTO 1025
	GOTO 1030

1025 NEXT XX

1030 RETURN

1035 FOR XX = 4 TO 11
	IF MID$(SA$(I), XX, 1) = " " OR MID$(SA$(I), XX, 1) = "." THEN MID$(SA$(I), XX, 1) = "_"
	NEXT
	QQ = 8
	FOR XX = 11 TO 4 STEP -1
	IF MID$(SA$(I), XX, 1) = "_" THEN QQ = XX - 4: GOTO 1055
	GOTO 1060

1055 NEXT XX

1060 RETURN

1065 PRINT "                                        "; : RETURN

6000 FOR I = 0 TO 16: LOCATE I + 2, 4: COLOR , L%(0, 12): PRINT "               ": NEXT
	FOR I = 0 TO 5: LOCATE I + 2, 24: COLOR , L%(0, 12): PRINT "               ": NEXT
	FOR I = 0 TO 16: LOCATE I + 2, 44: COLOR , L%(1, 12): PRINT "               ": NEXT
	FOR I = 0 TO 5: LOCATE I + 2, 64: COLOR , L%(1, 12): PRINT "               ": NEXT
	RETURN

7000 FOR I = 0 TO 11: LOCATE I + 2, 4: COLOR , L%(0, 12): PRINT "               ": NEXT
	FOR I = 0 TO 10: LOCATE I + 2, 24: COLOR , L%(0, 12): PRINT "               ": NEXT
	FOR I = 0 TO 11: LOCATE I + 2, 44: COLOR , L%(1, 12): PRINT "               ": NEXT
	FOR I = 0 TO 10: LOCATE I + 2, 64: COLOR , L%(1, 12): PRINT "               ": NEXT
	RETURN
	
	LOCATE 10, 8
	CALL CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	RETURN

4550 LOCATE 10, 8
	CALL CSMsgBox("NO COMPILED STATS FOR TEAM WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	RETURN

4500 LOCATE 10, 8
	CALL CSMsgBox("NO TEAMS MATCHING THIS ID WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	RETURN


SUB TITLE ()
	COLOR , 0: KEY OFF: CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 30: COLOR 12
	PRINT "FULL COUNT BASEBALL 6.0"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR 2, 0
	COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
	  PRINT "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
	COLOR 10
END SUB


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
