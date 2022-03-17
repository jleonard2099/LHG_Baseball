'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'
''$INCLUDE: 'QEditype.BI'

DEFINT A-Z
DECLARE SUB SHOWFIELD (P9%)
DECLARE SUB SHOWSPLIT (P9%)
DECLARE SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)
DECLARE SUB INFO ()
DECLARE SUB INITVARI ()
DECLARE SUB DRAWBOX ()
DECLARE SUB NEWLINES (P9)
DECLARE SUB POSITION (I%)
DECLARE SUB POSSWAP (P9)
DECLARE SUB LINSWAP (P9)
DECLARE SUB SHOWBATTERS (P9)
DECLARE SUB LETTER (I$)
DECLARE SUB COLCHK (P9, I)
'DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)

'DIM Ed AS EditInfo              'create the editor's data structure

TYPE PBP
	YY AS STRING * 30
END TYPE

DIM K9(1) AS SINGLE
DIM A$(1), L%(1, 13), T1(1), B%(1, 22, 80), P%(1, 21, 89), B$(1, 22), P$(1, 21), H$(4)
DIM PT$(1), PARK$(72), TS%(1, 11), SA%(1, 24),PIT$(22),PIT%(22)
DIM SHARED Q3$(100), MON(5, 14)
DIM SHARED ATTEN AS LONG
DIM INJ%(1, 22), DYS%(1, 21), PU$(8), WD$(9), PM$(6), BM$(6)
DIM P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
DIM S%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
DIM VV(1), VV1(1), D3%(1), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
DIM X0%(1, 2, 23), D0%(1), TP%(11, 2), L0%(1), T3%(22), AP%(2)
DIM P5%(1, 21, 41), B5%(1, 22, 21), P8%(1, 21, 41), B8%(1, 22, 21)
DIM T%(22), P4%(22), B2%(1, 22), P3%(1, 21)
DIM NG%(18), YN$(1), Q$(23), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
DIM BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2), UM%(1, 9, 4), MF%(1)
DIM PR%(24), GK%(1)
DIM CSS%(1, 22, 6)
DIM MG%(1, 999), GS%(21, 1), PS%(23, 1)
DIM U%(50)
DIM V$(200), S$(1), SP%(1), U6$(2), LGT$(2), DH$(1), UMP$(50), WE$(1), MN$(10), NB%(1, 4)
DIM UV%(200), VI$(1), DL$(1), DN$(1), F(9), M%(9), BB%(9), BP(9)

COMMON SHARED TS%(), SA%(), P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), H$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED S$, P, U6, DH%, U9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, DRIVE, AP%, INNING%, BUCK  AS PBP
COMMON SHARED MG%(), CSS%(), D, IJ%, DR%, CGERA%
REDIM Q2$(2056)
DIM E AS SINGLE
DIM SE AS SINGLE
DIM Q1 AS SINGLE
DIM QQ AS SINGLE
DIM R AS SINGLE
DIM Y AS LONG
DIM JJ AS SINGLE

'commenting out here as it is mostly irrelevant
'CALL InitMouse(There)

RANDOMIZE TIMER

CLS

BK$ = "                         "
VI$(0) = "VISITING TEAM"
VI$(1) = "HOME TEAM"
TM$(1) = "HOT": TM$(2) = "WARM": TM$(3) = "COOL": TM$(4) = "COLD"
PT$(0) = "RIGHT"
PT$(1) = "LEFT "
WD$(2) = "LEFT TO RIGHT AT"
WD$(3) = "RIGHT TO LEFT AT"
WD$(4) = "BLOWING IN FROM LEFT AT"
WD$(5) = "BLOWING IN FROM CENTER AT"
WD$(6) = "BLOWING IN FROM RIGHT AT"
WD$(7) = "BLOWING OUT TO LEFT AT"
WD$(8) = "BLOWING OUT TO CENTER AT"
WD$(9) = "BLOWING OUT TO RIGHT AT"
SC$(1) = "CLEAR": SC$(2) = "PARTLY CLOUDY": SC$(3) = "OVERCAST": SC$(4) = "DOME"
PC$(1) = "THUNDERSTORMS": PC$(2) = "RAIN": PC$(3) = "SPRINKLES": PC$(4) = "NONE"
	SP%(0) = -1: SP%(1) = -1
	U6$(0) = "HUMAN OPPONENT      "
	U6$(1) = "COMPUTER OPPONENT   "
	U6$(2) = "COMPUTER VS COMPUTER"
	PU$(0) = "1980 - PRESENT"
	PU$(1) = "1973 - 1979   "
	PU$(2) = "1955 - 1972   "
	PU$(3) = "1946 - 1954   "
	PU$(4) = "1933 - 1945   "
	PU$(5) = "1919 - 1932   "
	PU$(6) = "1909 - 1918   "
	PU$(7) = "1894 - 1908   "
	PU$(8) = "1876 - 1893   "
	LGT$(0) = "AL GAME         "
	LGT$(1) = "NL GAME         "
	LGT$(2) = "INTERLEAGUE GAME"
	DH$(0) = "NO DH "
	DH$(1) = "USE DH"
	UMP$(0) = "AL"
	UMP$(1) = "NL"
	UMP$(2) = "NL"
	WE$(0) = "YES"
	WE$(1) = "NO "
	DL$(0) = "LEAGUE GAME    "
	DL$(1) = "EXHIBITION GAME"

	IF NOT _FILEEXISTS("DEFERA") THEN 
		OPEN "DEFERA" FOR OUTPUT AS #1
			FOR I = 1 TO 14
				PRINT #1, 0
			NEXT
		CLOSE 1

OPEN "DEFERA" FOR INPUT AS #1: INPUT #1, U6: INPUT #1, LGT: INPUT #1, DH%
	INPUT #1, DL: INPUT #1, MO%: INPUT #1, DN%: INPUT #1, WE%
	INPUT #1, IJ%: INPUT #1, LR%: INPUT #1, DR%: INPUT #1, MP%
	INPUT #1, G4: INPUT #1, vg%: INPUT #1, CGERA%
CLOSE 1

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR I = 0 TO 3: INPUT #1, D$(I): NEXT I
CLOSE #1
	MN$(4) = "APRIL"
	MN$(5) = "MAY"
	MN$(6) = "JUNE"
	MN$(7) = "JULY"
	MN$(8) = "AUGUST"
	MN$(9) = "SEPTEMBER"
	MN$(10) = "OCTOBER"
	DN$(0) = "AFTERNOON"
	DN$(1) = "NIGHT"
	B1$(0) = "LP"
	B1$(1) = "L"
	B1$(2) = "S"
	B1$(3) = "R"
	B1$(4) = "RP"
	DATA  A,B,C,D,E,F,G,H,I,J,K,L,M,dh,p,"c ",1b,2b,3b,ss,lf,cf,rf,ph,pr
	CLS : FOR RA = 1 TO 60: RANDOMIZE TIMER: NEXT: P = 1
	FOR I = 0 TO 12: READ X$(I): NEXT I
	FOR I = 0 TO 11: READ C$(I): NEXT I

140 H$(1) = "1b": H$(2) = "2b": H$(3) = "3b": H$(4) = "home"
	 FOR I = 0 TO 1: FOR I1 = 0 TO 4: NB%(I, I1) = -1: NEXT: NEXT
	FOR I = 0 TO 1: P1%(I) = -1: P6%(I) = -1
	NEXT
	SP%(0) = -1: SP%(1) = -1
	 FOR I = 0 TO 1
	FOR J = 0 TO 9
	M%(J) = 0
	B7%(I, J) = -1
	B3%(I, J) = -1
	NEXT
	NEXT
	CALL INITVARI
	 IF NOT _FILEEXISTS("GAME82") THEN 6
OPEN "GAME82" FOR INPUT AS #2
INPUT #2, FF$
FOR I = 0 TO 2: INPUT #2, AP%(I): NEXT
CLOSE 2
OPEN D$(0) + FF$ FOR RANDOM AS #1 LEN = 48
	FOR X = 0 TO 18
	FIELD #1, X * 2 AS X$, 2 AS Q2$(X + 1), 48 - 2 - 2 * X AS X$
	NEXT
	FIELD #1, 38 AS X$, 2 AS Q2$(22), 2 AS Q2$(23), 3 AS Q2$(20), 3 AS Q2$(21)
	GET #1, AP%(0) + AP%(2)
	FOR X = 0 TO 18: NG%(X) = CVI(Q2$(X + 1))
	NEXT
	FOR X = 0 TO 1: YN$(X) = Q2$(20 + X)
	NEXT
	VS% = CVI(Q2$(22)): HS% = CVI(Q2$(23))
CLOSE 1
	AP% = 1

6 IF AP% <> 1 THEN 170
	U6 = NG%(8)
	DH% = NG%(3)
	G4 = NG%(4) / 10
	MO% = NG%(17)
	WE% = NG%(5)
	vg% = NG%(11)
	IJ% = NG%(6)
	LR% = NG%(7)
	DR% = NG%(9)
	MP% = NG%(10)
	CGERA% = NG%(14)
	DN% = 1
	IF NG%(15) < 1600 THEN DN% = 0
	IF U6 = 0 THEN 160
	IF U6 = 3 THEN U6 = 2: GOTO 160
	IF U6 = 1 THEN U9 = 0: GOTO 160
	U9 = 1: U6 = 1

160 GOTO 260

170 CALL INFO

171 COLOR , 0: LOCATE 3, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT U6$(U6)
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT LGT$(LGT)
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT DH$(DH%)
	COLOR 14: PRINT "(4) "; : COLOR 15: PRINT DL$(DL)
	COLOR 14: PRINT "(5) "; : COLOR 15: PRINT MN$(MO%)
	COLOR 14: PRINT "(6) "; : COLOR 15: PRINT DN$(DN%)
	COLOR 14: PRINT "(7) "; : COLOR 15: PRINT "PROCEED TO TEAM SELECTION"
	COLOR 14: PRINT "(8) "; : COLOR 15: PRINT "USE CLIMATE/WIND/TEMP DATA "; WE$(WE%)
	COLOR 14: PRINT "(9) "; : COLOR 15: PRINT "USE INJURIES "; WE$(IJ%)
	COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "SAVE SETTINGS AS DEFAULT"
	COLOR 14: PRINT "(A) "; : COLOR 15: PRINT "USE LEFT/RIGHT SPLIT STATS "; WE$(LR%)
	COLOR 14: PRINT "(B) "; : COLOR 15: PRINT "USE DAYS REST DATA FOR PITCHERS "; WE$(DR%)
	COLOR 14: PRINT "(C) "; : COLOR 15: PRINT "USE MANAGER PROFILE, IF AVAILABLE "; WE$(MP%)
	COLOR 14: PRINT "(D) "; : COLOR 15: PRINT USING "DISPLAY INTERVAL: #.# "; G4
	COLOR 14: PRINT "(E) "; : COLOR 15: PRINT "USE VGA SCREENS, IF ABLE: "; WE$(vg%)
	COLOR 14: PRINT "(F) "; : COLOR 15: PRINT "PITCHER USAGE ERA: "; PU$(CGERA%)

183 COLOR 7, 0: I$=GetKeyPress$
	IF I$ = CHR$(27) THEN RUN "HELLO.EXE"
	CALL LETTER(I$)
	I = VAL(I$): IF I < 0 OR I > 16 THEN 183
	ON I + 1 GOTO 205, 191, 193, 195, 197, 199, 201, 260, 204, 206, 207, 208, 209, 210, 211, 212

191 U6 = U6 + 1: IF U6 > 2 THEN U6 = 0
	GOTO 171

211 vg% = 1 - vg%: GOTO 171

206 IJ% = 1 - IJ%: GOTO 171

207 LR% = 1 - LR%: GOTO 171

208 DR% = 1 - DR%: GOTO 171

193 LGT = LGT + 1: IF LGT > 2 THEN LGT = 0
	GOTO 171

195 DH% = 1 - DH%: GOTO 171

197 DL = 1 - DL: GOTO 171

209 MP% = 1 - MP%: GOTO 171

199 MO% = MO% + 1: IF MO% > 10 THEN MO% = 4
	LOCATE 7, 4: PRINT BK$: GOTO 171

201 DN% = 1 - DN%
	LOCATE 8, 4
	PRINT BK$
	GOTO 171

212 CGERA% = CGERA% + 1
	IF CGERA% > 8 THEN CGERA% = 0
	GOTO 171

204 WE% = 1 - WE%: GOTO 171

210 LOCATE 16, 22: INPUT G4: GOTO 171

205 OPEN "DEFERA" FOR OUTPUT AS #1: PRINT #1, U6: PRINT #1, LGT: PRINT #1, DH%
	PRINT #1, DL: PRINT #1, MO%: PRINT #1, DN%: PRINT #1, WE%
	PRINT #1, IJ%: PRINT #1, LR%: PRINT #1, DR%: PRINT #1, MP%
	PRINT #1, G4: PRINT #1, vg%: PRINT #1, CGERA%
CLOSE 1
	GOTO 171

260 YV = 1: YM = 0
	IF NOT _FILEEXISTS(D$(0) + "UMPIRES." + UMP$(LGT)) THEN 265
	YU = 0
OPEN D$(0) + "UMPIRES." + UMP$(LGT) FOR INPUT AS #1
	FOR I1 = 1 TO 50
	INPUT #1, UMP$(I1)
	INPUT #1, U%(I1)
	NEXT
CLOSE #1
	FOR I1 = 1 TO 50
	IF UMP$(I1) <> "" THEN YU = YU + 1
	NEXT
	IF YU < 4 THEN 265

262 YU = INT(RND(1) * 50) + 1: IF UMP$(YU) = "" OR YU = YM THEN 262
	U$(YV) = UMP$(YU): YM = YU: YV = YV + 1: IF YV < 5 THEN 262
	GOTO 270

265 U$(1) = "RHODES": U$(2) = "WINBERRY": U$(3) = "TIPPETT": U$(4) = "MILLER"

270 FOR I = 0 TO 1
	IF AP% = 1 THEN T1(I) = NG%(I): YN$ = YN$(I): GOSUB 300: GOTO 2860
	 CALL INFO

2800 COLOR 15, 0: CLS
	JJ = 10
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT VI$(I)
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	PRINT
	LOCATE , 12: PRINT "MAKE SURE NUM LOCK KEY IS OFF TO USE ARROW KEYS"
	LOCATE , 12: PRINT "AND PG UP/PG DN KEYS"
	PRINT
	LOCATE , 12: PRINT "(JUST HIT ENTER TO USE PREVIOUS ID, IF SAME)"
	LOCATE 8, 27: INPUT NY$
	COLOR 15, 0
	IF NY$ = "" THEN GOSUB 8102: GOTO 2856
	YN$ = NY$
	FOR I1 = 1 TO 200: V$(I1) = "": NEXT
	GOSUB 8100

2856 IF BO% = 1 THEN BO% = 0: GOTO 2800

2858 LOCATE 2, 33
	COLOR L%(I, 11), L%(I, 12)
	PRINT A$(I)
	COLOR 15, 4
	LOCATE , 33
	PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 2800

2860 YN$(I) = YN$
	COLOR 15, 0
A$(I) = RTrim$(A$(I))
	NEXT I
	N = 1
	IF U6 = 1 AND AP% <> 1 THEN GOSUB 2430
	  '***   SELECT PITCHERS FOR EACH TEAM   ***

2861 FOR P9 = 0 TO 1
	EC% = 0
	GOSUB 4500
	IF EC% = 1 AND P9 = 0 THEN 170
	IF EC% = 1 AND P9 = 1 THEN 2861
	S8%(P9, 0) = INT(((P%(P9, P1%(P9), 8) + P%(P9, P1%(P9), 7)) / P%(P9, P1%(P9), 4)) + .5)
	S8%(P9, 1) = 3
	NEXT P9
	'***   SELECT LINEUPS FOR EACH TEAM   ***
	FOR P9 = 0 TO 1
	CALL SHOWBATTERS(P9)
	COLOR 15, 0
	LOCATE 1, 59
	PRINT "OPPOSING PITCHER:"
	LOCATE 2, 59
	PRINT P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
	LOCATE , 59
	PRINT USING "##"; P%(1 - P9, P1%(1-P9), 1); : PRINT "-"; : PRINT USING "## ##.##"; P%(1-P9, P1%(1-P9), 2); P%(1-P9, P1%(1-P9), 10) / 100
	LOCATE , 59
	IF P%(1 - P9, P1%(1 - P9), 51) + P%(1 - P9, P1%(1 - P9), 58) < > 0 AND TS%(1 - P9, 0)<>-1 THEN
		PRINT USING "L:  .###";P%(1 - P9, P1%(1 - P9), 52)/(P%(1 - P9, P1%(1 - P9), 51)+.001);
		PRINT USING "  R:  .###";P%(1 - P9, P1%(1 - P9), 59)/(P%(1 - P9, P1%(1 - P9), 58)+.001)
	ELSEIF P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7) = 0 THEN
	PRINT "OPP AVG:  .---"
	ELSEIF P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7) =  P%(1 - P9, P1%(1 - P9), 7) THEN
	PRINT "OPP AVG: 1.000"
	ELSE
	PRINT USING "OPP AVG:  .###";P%(1 - P9%, P1%(1 - P9), 7) / (P%(1 - P9, P1%(1 - P9), 6) * 3 * 0.94 + P%(1 - P9, P1%(1 - P9), 7))
	END IF
	BM$(1) = "CHOOSE STARTING PITCHER"
	BM$(2) = "COMPUTER CHOOSE STARTER"
	BM$(3) = "DISABLE/ACTIVATE PLAYER"
	BM$(4) = "GRANT DAY OF REST      "
	BM$(5) = "ACTIVATE ALL PITCHERS  "
	BM$(6) = "CONTINUE TO NEXT MENU  "
	Start% = 1

2862 'GOSUB 1521
	SELECTION% = 1
	Count% = 6
	SCANCODE% = 6
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 19
	Column% = 58
CALL box0(6, 58, 16, 80, 2, 31)
CALL box0(18, 58, 25, 80, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(BM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% <> 13 AND SCANCODE% <> 1003 THEN 2862
	END
	'2161 LOCATE 10, 59: PRINT "DISABLE BATTER (0-M)": LOCATE , 59: PRINT "Z-CONTINUE"
	LOCATE , 59: PRINT "X-REACTIVATE ALL"
	LOCATE , 59: PRINT "<TAB>-BAT/FIELD/SPLIT"
	NEXT
	 '***   SELECT LINEUPS FOR EACH TEAM   ***
	FOR P9 = 0 TO 1

480 CLS : CALL INFO
	LOCATE 3, 1
	COLOR L%(P9, 11), L%(P9, 12): PRINT A$(P9): COLOR 7, 0: PRINT
	COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "INPUT A LINEUP MANUALLY": PRINT
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "SELECT A LINEUP FROM MANAGER PROFILE": PRINT
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "COMPUTER DETERMINE LINEUP RANDOMLY": PRINT
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "COMPUTER DETERMINE LINEUP FROM MANAGER PROFILE"
	 IF AP% = 1 AND NLF% = 1 THEN NLF% = 0: I$ = "2": GOTO 491
	IF AP% = 1 AND MP% = 0 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN I$ = "1": GOTO 491
	IF AP% = 1 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN I$ = "2": GOTO 491
490 I$ = GetKeyPress$
	IF I$=CHR$(27) THEN 170

491 I1 = VAL(I$): IF I1 < 0 OR I1 > 3 THEN 490
	IF I$ = "2" THEN GOSUB 5000: IF LE = 0 THEN 520
	IF LE = 1 THEN LE = 0: GOTO 480
	IF I$ <> "1" AND I$ <> "3" THEN GOSUB 2160: GOSUB 2310: GOSUB 540
	IF I$ = "1" OR I$ = "3" THEN GOSUB 1830

520 COLOR 15, 0: LOCATE 5, 59: PRINT "OPPOSING PITCHER"
	LOCATE , 59: PRINT P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
	IF RP = 1 THEN RP = 0: GOTO 480
	NEXT P9: GOSUB 2390
	W5 = 0: W6 = 1: VV(0) = 1: VV1(0) = 9: VV(1) = 1: VV1(1) = 9: GOTO 2330

540 FOR I = 0 TO 9: M%(I) = 0: NEXT I: GOSUB 1370
	FOR I = 1 TO 9
	COLOR 15, 0: LOCATE 5, 59: PRINT "OPPOSING PITCHER": LOCATE , 59: PRINT P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

560 CALL POSITION(I): PRINT "                  "; : CALL POSITION(I)
561 I$ = GetKeyPress$
	CALL LETTER(I$)
	IF I$ = CHR$(27) THEN
	  FOR J = 1 TO 9
	  M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1
	  NEXT
	  C1 = 0
	  FOR J = 0 TO 22
	  B%(P9, J, 21) = 0
	  NEXT
	  RP = 1
	  FOR J = 0 TO 22
	  IF B%(P9, J, 31) <> 0 THEN B%(P9, J, 21) = 99
	  NEXT
	  GOTO 480
	END IF
	I1 = VAL(I$): IF UCASE$(I$) = "X" THEN 540
	FOR J = 1 TO 9: IF J = I OR B3%(P9, J) = I1 AND B7%(P9, J) = 1 THEN 610
	IF B3%(P9, J) = I1 AND UCASE$(I$) <> "P" THEN 560

610 NEXT J
	IF DH% = 1 AND UCASE$(I$) = "P" THEN 560
	IF B%(P9, I1, 21) <> 0 AND UCASE$(I$) <> "P" THEN 560
	IF UCASE$(I$) = "P" THEN 760
	B3%(P9, I) = I1: PRINT USING "# "; I; : PRINT B$(P9, B3%(P9, I));
	LOCATE , 74
680 I$ = GetKeyPress$
	IF VAL(I$) = 1 THEN 680
	FOR J = 1 TO 9: IF J = I THEN 720
	IF VAL(I$) = B7%(P9, J) THEN 680

720 NEXT J
	IF DH% = 0 AND VAL(I$) = 0 OR UCASE$(I$) = "X" THEN 560
	PRINT C$(VAL(I$)); : B7%(P9, I) = VAL(I$)
	B9%(P9, B7%(P9, I)) = B3%(P9, I): M%(VAL(I$)) = 1
	LOCATE I1 + 2, 2: PRINT "*"; : COLOR L%(P9, 11), L%(P9, 12): PRINT B$(P9, I1): COLOR 15, 0: GOSUB 1370: GOTO 830

760 FOR J = 1 TO 9: IF J = I THEN 780
	IF B3%(P9, J) = P1%(P9) AND B7%(P9, J) = 1 THEN 560

780 NEXT J
	B3%(P9, I) = P1%(P9): PRINT P$(P9, P1%(P9)); : M%(VAL(I$)) = 1
	LOCATE , 72
	PRINT "P "; : B7%(P9, I) = 1: B9%(P9, 1) = P1%(P9): M%(1) = 1: GOSUB 1370

830 IF C1 = 1 THEN 850
	NEXT I

850 C1 = 1: LOCATE 25, 1: COLOR 14: PRINT "(1-9) CHANGE 0-NONE (R)ESELECT LINEUP (P)OSITION SWAP (L)INEUP SWAP";
	COLOR 15, 0
	IF AP% = 1 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN I$ = "0": GOTO 861
860 I$ = GetKeyPress$

861 IF UCASE$(I$) = "R" OR I$=CHR$(27) THEN FOR J = 1 TO 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: NEXT: C1 = 0: FOR J = 0 TO 22: B%(P9, J, 21) = 0: NEXT: RP = 1: GOTO 1001
	IF UCASE$(I$) = "L" THEN CALL LINSWAP(P9): GOTO 850
	IF UCASE$(I$) = "P" THEN CALL POSSWAP(P9): GOTO 850
	I = VAL(I$): IF I < 1 OR I > 9 THEN 890
	LOCATE B3%(P9, I) + 2 - B4%, 2: PRINT " "; B$(P9, B3%(P9, I)): GOTO 560

890 IF I% = 0 THEN GOSUB 1710
	GOSUB 1200: C1 = 0

910 RETURN

1001 FOR J = 0 TO 22: IF B%(P9, J, 31) <> 0 THEN B%(P9, J, 21) = 99
	NEXT
	GOTO 910

1200 FOR I = 1 TO 9
	IF B7%(P9, I) = 1 THEN 1230
	B%(P9, B3%(P9, I), 21) = 1

1230 M%(I) = 0: NEXT I
	M%(0) = 0: RETURN

1370 LOCATE 25, 1: FOR I1 = 0 TO 9: IF DH% = 0 AND I1 = 0 THEN 1410
	IF DH% = 1 AND I1 = 1 THEN 1410
	IF M%(I1) = 0 THEN PRINT I1; " ";
	IF M%(I1) = 1 THEN PRINT "  ";

1410 NEXT I1
	RETURN

1520 CLS

1521 LOCATE 1,1
	COLOR L%(P9, 11), L%(P9, 12): PRINT A$(P9);
	COLOR 14, 0: LOCATE , 19: PRINT "T  IP   H   G GS DR    W- L   ERA"
	FOR I = 0 TO 21
	IF P$(P9, I) = "XXX" OR LEFT$(P$(P9, I), 1) = " " OR P$(P9, I) = "" THEN
		P%(P9, I, 15) = 98
	END IF
	IF P$(P9, I) = "XXX" OR LEFT$(P$(P9, I), 1) = " " OR P$(P9, I) = "" THEN 1580
	COLOR 2, 0
	  IF P%(P9, I, 15) = 0 THEN
		PRINT "A";
	ELSEIF P%(P9, I, 15) = 1 THEN
		PRINT "L";
	ELSE
		PRINT "X";
	END IF
	COLOR 15,0
	LOCATE , 3: PRINT P$(P9, I);
	LOCATE , 19: PRINT B1$(P%(P9, I, 0) + 2);
	LOCATE , 20: PRINT USING " ### ### ### ## ## "; P%(P9, I, 6); P%(P9, I, 7); P%(P9, I, 4); P%(P9, I, 5); P%(P9, I, 35);
	LOCATE , 39: PRINT USING "  ##"; P%(P9, I, 1); : PRINT "-"; : PRINT USING "## ##.##"; P%(P9, I, 2); P%(P9, I, 10) / 100

1580 NEXT I
	RETURN

1710 LOCATE 25, 1: PRINT "                                                                          ";
	LOCATE 25, 1: PRINT "SAVE THIS LINE UP (YN) ";
	IF AP% = 1 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN I$ = "N": GOTO 1730
1720 I$ = GetKeyPress$

1730 IF UCASE$(I$) <> "Y" THEN 1820

1790 CLS
	IF P%(1 - P9, P1%(1 - P9), 0) = -1 THEN Q2% = 1
	IF P%(1 - P9, P1%(1 - P9), 0) = 0 THEN Q2% = 0
	FOR I = 0 TO 9
	PRINT I; " ";
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 THEN PRINT "AVAILABLE"
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 THEN PRINT "LINEUP ALREADY SAVED "; MG%(P9, 100 + (I + Q2% * 10) * 21); " % USAGE"
	NEXT
	INPUT "LINEUP # TO SAVE "; LN%
	INPUT "% OF TIME TO USE "; LU%
	IF MG%(P9, 120 + (LN% + Q2% * 10) * 21) <> 999 THEN MG%(P9, 120 + (LN% + Q2% * 10) * 21) = 999
	IF Q2% = 1 THEN LN% = LN% + 10
	MG%(P9, 101 + LN% * 21) = DH%
	MG%(P9, 100 + LN% * 21) = LU%
	FOR I = 1 TO 9
	MG%(P9, 110 + I + LN% * 21) = B3%(P9, I)
	MG%(P9, 101 + I + LN% * 21) = B7%(P9, I)
	NEXT
	GOSUB 4522

1820 RETURN

2500 CLS
	IF P%(1 - P9, P1%(1 - P9), 0) = -1 THEN Q2% = 1
	IF P%(1 - P9, P1%(1 - P9), 0) = 1 THEN Q2% = 0
	PRINT TAB(30); "LINEUPS FOUND VS. "; PT$(Q2%)
	PRINT " #0    "; MG%(P9, 100 + (0 + Q2% * 10) * 21); "%";
	LOCATE , 17: PRINT " #1    "; MG%(P9, 100 + (1 + Q2% * 10) * 21); "%";
	LOCATE , 33: PRINT " #2    "; MG%(P9, 100 + (2 + Q2% * 10) * 21); "%";
	LOCATE , 49: PRINT " #3    "; MG%(P9, 100 + (3 + Q2% * 10) * 21); "%";
	LOCATE , 65: PRINT " #4    "; MG%(P9, 100 + (4 + Q2% * 10) * 21); "%"
	FOR I = 0 TO 4
	FOR I1 = 1 TO 9
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 AND I1 = 1 THEN LOCATE 3, 16 * I + 1: PRINT "NONE FOUND": GOTO 2510
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 AND I1 <> 1 THEN LOCATE 2 + I1, 16 * I + 1: PRINT "         ": GOTO 2510
	LOCATE 2 + I1, 16 * I + 1
	IF MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 THEN PRINT "PITCHER"; : GOTO 2512
	PRINT LEFT$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

2512 LOCATE 2 + I1, 16 * I + 12: PRINT C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

2510 NEXT
	NEXT
	PRINT
	PRINT " #5    "; MG%(P9, 100 + (5 + Q2% * 10) * 21); "%";
	LOCATE , 17: PRINT " #6    "; MG%(P9, 100 + (6 + Q2% * 10) * 21); "%";
	LOCATE , 33: PRINT " #7    "; MG%(P9, 100 + (7 + Q2% * 10) * 21); "%";
	LOCATE , 49: PRINT " #8    "; MG%(P9, 100 + (8 + Q2% * 10) * 21); "%";
	LOCATE , 65: PRINT " #9    "; MG%(P9, 100 + (9 + Q2% * 10) * 21); "%"
	FOR I = 5 TO 9
	FOR I1 = 1 TO 9
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 AND I1 = 1 THEN LOCATE 14, 16 * (I - 5) + 1: PRINT "NONE FOUND": GOTO 2505
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 AND I1 <> 1 THEN LOCATE 13 + I1, 16 * (I - 5) + 1: PRINT "         ": GOTO 2505
	LOCATE 13 + I1, 16 * (I - 5) + 1
	IF MG%(P9, 101 + I1 + (I + Q2% * 10) * 21) = 1 THEN PRINT "PITCHER"; : GOTO 2507
	PRINT LEFT$(B$(P9, MG%(P9, 110 + I1 + (I + Q2% * 10) * 21)), 10);

2507 LOCATE 13 + I1, 16 * (I - 5) + 12: PRINT C$(MG%(P9, 101 + I1 + (I + Q2% * 10) * 21))

2505 NEXT
	NEXT
	RETURN

2530 RN = INT(RND(1) * PCT%) + 1
	PCT% = 0
	FOR I = 0 TO 9
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) <> 999 THEN 2535
	PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
	IF RN <= PCT% THEN LN% = I: GOTO 2536

2535 NEXT

2536 RETURN

1830 GOSUB 2500
	LF% = 0: PCT% = 0
	FOR I = 0 TO 9
	IF MG%(P9, 120 + (I + Q2% * 10) * 21) = 999 THEN LF% = LF% + 1: PCT% = PCT% + MG%(P9, 100 + (I + Q2% * 10) * 21)
	NEXT
	IF LF% = 0 THEN 3110
	IF AP% = 1 AND MP% = 0 AND (U6 = 2 OR U6 = 1 AND U9 = P9) OR I$ = "3" THEN GOSUB 2530: GOTO 2516

2515 LOCATE 23, 1: INPUT "ENTER LINEUP NUMBER (0-9) "; LN%

2516 IF LN% < 0 OR LN% > 9 THEN 2515
	IF MG%(P9, 120 + (LN% + Q2% * 10) * 21) <> 999 THEN 480
	IF Q2% = 1 THEN LN% = LN% + 10
	I1 = MG%(P9, 101 + LN% * 21)
	IF DH% <> I1 THEN 480
	FOR I = 1 TO 9
	B3%(P9, I) = MG%(P9, 110 + I + LN% * 21)
	B7%(P9, I) = MG%(P9, 101 + I + LN% * 21)
	NEXT
	LU% = 1

1910 FOR I = 1 TO 9: B9%(P9, B7%(P9, I)) = B3%(P9, I)
	IF B7%(P9, I) = 1 THEN B3%(P9, I) = P1%(P9): B9%(P9, 1) = P1%(P9): GOTO 2525
	B%(P9, B3%(P9, I), 21) = 0

2525 NEXT I
	IF LU% = 1 THEN LU% = 0: GOSUB 2160
	GOSUB 2310: CALL NEWLINES(P9): GOSUB 1940: GOTO 850

1940 FOR I2 = 0 TO 22: LOCATE I2 + 2, 2: FOR I3 = 1 TO 9
	IF B3%(P9, I3) = I2 AND B7%(P9, I3) <> 1 THEN PRINT "*"; : COLOR L%(P9, 11), L%(P9, 12): PRINT B$(P9, I2): COLOR 15, 0
	NEXT I3: NEXT I2
	FOR I3 = 0 TO 9: M%(I3) = 1: NEXT I3
	RETURN

2000 IF P%(P9, I, 15) >= 98 THEN COLOR 0, 7
	RETURN

2160 I% = 99: CALL SHOWBATTERS(P9)
	COLOR 15, 0: LOCATE 5, 59: PRINT "OPPOSING PITCHER"
	LOCATE 6, 59: PRINT P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)

2161 LOCATE 10, 59: PRINT "DISABLE BATTER (0-M)": LOCATE , 59: PRINT "Z-CONTINUE"
	LOCATE , 59: PRINT "X-REACTIVATE ALL"
	LOCATE , 59: PRINT "<TAB>-BAT/FIELD/SPLIT"
	IF AP% = 1 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN I$ = "Z": GOTO 2175
2170 I$ = GetKeyPress$
	if i$ = chr$(27) then
	  FOR J = 1 TO 9
	  M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1
	  NEXT
	  C1 = 0
	  FOR J = 0 TO 22
	  B%(P9, J, 21) = 0
	  NEXT
	  RP = 1
	  FOR J = 0 TO 22
	  IF B%(P9, J, 31) <> 0 THEN B%(P9, J, 21) = 99
	  NEXT
	  GOTO 480
	END IF

2175 IF ASC(I$) = 9 THEN TG = TG + 1
	  IF TG > 2 OR TG = 2 AND TS%(P9, 0) <> 999 THEN TG = 0
	IF ASC(I$) = 9 AND TG = 1 THEN CALL SHOWFIELD(P9): GOTO 2170
	IF ASC(I$) = 9 AND TG = 0 THEN CALL SHOWBATTERS(P9): GOTO 2161
	IF ASC(I$) = 9 AND TG = 2 THEN CALL SHOWSPLIT(P9): GOTO 2170
	CALL LETTER(I$)
	IF UCASE$(I$) = "Z" THEN TG = 0: CALL SHOWBATTERS(P9): GOTO 2230
	IF UCASE$(I$) = "X" THEN GOSUB 2240: GOTO 2170
	I1 = VAL(I$): IF I1 > 22 THEN 2170
	B%(P9, I1, 21) = 99: LOCATE I1 + 2, 3: COLOR 0, 7: PRINT B$(P9, I1); : COLOR 15, 0: GOTO 2170

2230 COLOR 15, 0: LOCATE 5, 59: PRINT "OPPOSING PITCHER"
	LOCATE 6, 59: PRINT P$(1 - P9, P1%(1 - P9)); " "; B1$(P%(1 - P9, P1%(1 - P9), 0) + 2)
	I% = 0: RETURN

2240 FOR I3 = 0 TO 22: IF B%(P9, I3, 21) = 99 AND B%(P9, I3, 31) = 0 THEN B%(P9, I3, 21) = 0: LOCATE I3 + 2, 3: PRINT B$(P9, I3)
	NEXT I3: RETURN

2310 FOR I = 10 TO 22
	LOCATE I, 59: PRINT "                     ";
	NEXT
	RETURN

2330 GOSUB 2470

2342 LOCATE 6: PRINT "LOADING GAME..."

2345 I1 = VAL(LEFT$(A$(0), 2)): I2 = VAL(LEFT$(A$(1), 2))
	  IF CGERA% > 2 THEN S8%(1, 1) = 4: S8%(0, 1) = 4
	IF CGERA% > 3 THEN S8%(1, 1) = 5: S8%(0, 1) = 5
	IF CGERA% = 7 THEN S8%(1, 1) = 7: S8%(0, 1) = 7
	IF CGERA% >= 8 THEN S8%(1, 1) = 99: S8%(0, 1) = 99
	IF (MON = 6 OR MON = 7) AND vg% = 0 THEN CHAIN "sourceg.exe"
	CHAIN "SOURCE.EXE"

2390 FOR I = 0 TO 1: FOR I1 = 1 TO 9
	X0%(I, 0, I1) = B3%(I, I1)
	X0%(I, 1, I1) = B7%(I, I1)
	X0%(I, 2, I1) = I1 * 10
	NEXT I1
	D0%(I) = 9
	NEXT I
	RETURN

2430 CALL INFO: COLOR 14: PRINT "WHICH TEAM DOES COMPUTER MANAGE ?": PRINT
	COLOR 15: PRINT "(0) "; : COLOR L%(0, 11), L%(0, 12): PRINT A$(0): COLOR 7, 0: PRINT
	COLOR 15: PRINT "(1) "; : COLOR L%(1, 11), L%(1, 12): PRINT A$(1): COLOR 7, 0
2440 I$ = GetKeyPress$
	U9 = VAL(I$): IF U9 <> 1 THEN U9 = 0
	RETURN

2470 RD = 0
	FOR I = 0 TO 1
	FOR I1 = 1 TO 9
	IF B7%(I, I1) < 2 OR B7%(I, I1) > 9 THEN 2475
	IF B7%(I, I1) = B%(I, B3%(I, I1), 22) THEN B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 33): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 34): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 26): GOTO 2475
	IF B7%(I, I1) = B%(I, B3%(I, I1), 23) THEN B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 35): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 36): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 27): GOTO 2475
	IF B7%(I, I1) = B%(I, B3%(I, I1), 24) THEN B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 37): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 38): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 28): GOTO 2475
	IF B7%(I, I1) = B%(I, B3%(I, I1), 25) THEN B%(I, B3%(I, I1), 15) = B%(I, B3%(I, I1), 39): B%(I, B3%(I, I1), 17) = B%(I, B3%(I, I1), 40): B%(I, B3%(I, I1), 19) = B%(I, B3%(I, I1), 29): GOTO 2475
	IF B7%(I, I1) = 2 THEN B%(I, B3%(I, I1), 15) = 1: B%(I, B3%(I, I1), 17) = 0: B%(I, B3%(I, I1), 19) = 910 + INT(RND(1) * 10) + 1
	IF B7%(I, I1) = 3 THEN B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 960 + INT(RND(1) * 10) + 1
	IF B7%(I, I1) = 4 THEN B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 912 + INT(RND(1) * 10) + 1
	IF B7%(I, I1) = 5 OR B7%(I, I1) = 6 THEN B%(I, B3%(I, I1), 15) = 0: B%(I, B3%(I, I1), 17) = 3: B%(I, B3%(I, I1), 19) = 902 + INT(RND(1) * 10) + 1
	IF B7%(I, I1) >= 7 THEN B%(I, B3%(I, I1), 15) = 2: B%(I, B3%(I, I1), 17) = 2: B%(I, B3%(I, I1), 19) = 972 + INT(RND(1) * 10) + 1

2475 NEXT: NEXT

2476 GOSUB 3500
	IF BO% = 1 THEN BO% = 0: GOTO 2476
	CALL GETWEATHER(WD%, WS%, MO%, WF!, SC%, TP%, RD): CLS
	CALL DRAWBOX
	LOCATE , 11: PRINT "CONDITIONS AT "; S$(1)
	IF L%(1, 9) = 1 THEN LOCATE , 11: PRINT "TEMPERATURE: 70": LOCATE , 11: PRINT "WEATHER: DOME": LOCATE , 11: PRINT "WIND: NONE": GOTO 2479
	LOCATE , 11: PRINT "TEMPERATURE: "; TP%
	LOCATE , 11: PRINT "SKY CONDITIONS: "; SC$(SC%)
	LOCATE , 11: PRINT "WIND: "; WD$(WD%);
	PRINT USING " ## MPH"; WS%
	IF VG% = 1 THEN
		WD$(2) = "L->R"
		WD$(3) = "R->L"
		WD$(4) = "<-LF"
		WD$(5) = "<-CF"
		WD$(6) = "<-RF"
		WD$(7) = "->LF"
		WD$(8) = "->CF"
		WD$(9) = "->RF"
	END IF
	WI$ = WD$(WD%)+STR$(WS%)+ " MPH"
	LOCATE , 11: PRINT "PRECIPITATION: "; PC$(PC%)

2479 S$ = PARK$
	RETURN

3110 CLS : PRINT "ERROR:: NO LINEUPS  FOUND...BE SURE CORRECT LINEUP DISK IS IN DRIVE"
	IF AP% = 1 THEN NLF% = 1
	GOTO 480

3500 'SELECT A DIFFERENT STADIUM
PARK$(1) = "ANAHEIM STADIUM             "
PARK$(2) = "THE ASTRODOME               "
PARK$(3) = "OAKLAND-ALAMEADA COLISEUM   "
PARK$(4) = "SKYDOME                     "
PARK$(5) = "FULTON COUNTY STADIUM       "
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
PARK$(72) = "GENERIC                     "
	CALL DRAWBOX
	PARK$ = RIGHT$(STR$(L%(1, 13)), LEN(STR$(L%(1, 13))) - 1) + "."
	IF DN% = 0 THEN PARK$ = PARK$ + "D" ELSE PARK$ = PARK$ + "N"
	LOCATE , 11: PRINT "CURRENT PARK IS:"
	LOCATE , 11: PRINT PARK$(L%(1, 13))
	LOCATE , 11
	IF _FILEEXISTS(PARK$) THEN PRINT "GRAPHIC EXISTS" ELSE PRINT "GRAPHIC NOT AVAILABLE": PARK$ = "99.N"
	LOCATE , 11: PRINT "SELECT A DIFFERENT STADIUM (YN)"
	COLOR 15, 0
	IF AP% = 1 THEN RETURN
3505 I$ = GetKeyPress$
	IF UCASE$(I$) <> "Y" THEN RETURN
	CLS
	LOCATE 23, 10: PRINT "* - STADIUM GRAPHIC FILE NOT AVAILABLE"
	Start% = 1

3510 SELECTION% = 1
	Count% = 15
	SCANCODE% = 72
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 5
	Column% = 10
CALL box0(4, 9, 20, 38, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(PARK$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN BO% = 1: RETURN
	PARK$ = RIGHT$(STR$(SELECTION%), LEN(STR$(SELECTION%)) - 1) + "."
	IF DN% = 0 THEN PARK$ = PARK$ + "D" ELSE PARK$ = PARK$ + "N"
	COLOR 15, 0
	IF _FILEEXISTS(PARK$) THEN L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%): RETURN
	LOCATE 10, 8
	PRINT "STADIUM GRAPHIC NOT AVAILABLE"
	LOCATE , 8: PRINT "USE SELECTED PARK RATINGS WITH"
	LOCATE , 8: PRINT "GENERIC DISPLAY (YN)"
3515 I$ = GetKeyPress$
	IF I$ <> "Y" THEN 3510
	L%(1, 13) = SELECTION%: S$(1) = PARK$(SELECTION%)
	IF DN% = 0 THEN PARK$ = "99.D" ELSE PARK$ = "99.N"
	RETURN

4500 '*** PITCHING ROTATIONS ***
	GOSUB 1520
	IF AP% = 1 AND (U6 = 2 OR U6 = 1 AND U9 = P9) THEN RETURN
	PM$(1) = "CHOOSE STARTING PITCHER"
	PM$(2) = "COMPUTER CHOOSE STARTER"
	PM$(3) = "DISABLE/ACTIVATE PLAYER"
	PM$(4) = "GRANT DAY OF REST      "
	PM$(5) = "ACTIVATE ALL PITCHERS  "
	PM$(6) = "CONTINUE TO NEXT MENU  "
	Start% = 1

4501 GOSUB 1521
	SELECTION% = 1
	LOCATE 7, 57
	COLOR 15,0
	PRINT "STARTING PITCHER IS:"
	IF SP%(P9) <> -1 THEN
		LOCATE , 57
		PRINT P$(P9, P1%(P9)); TAB(79)
	ELSE
		LOCATE , 57
		PRINT "NONE SELECTED"; TAB(79)
	END IF
	Count% = 6
	SCANCODE% = 6
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 12
	Column% = 57
CALL box0(11, 56, 18, 80, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(PM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN
		EC% = 1
		RETURN
	END IF
	IF SCANCODE% <> 13 AND SCANCODE% <> 1003 THEN 4501
	  IF SELECTION% = 1 THEN
		CALL SELECTPITCHER (P$(),P9,P%(),PIT$(),PIT%(),NP,I1)
		IF I1 = -1 THEN 4501
		IF P%(P9, I1, 15) > 0 THEN

	LOCATE 9
	Wdth = 54
	Message$ = "This pitcher has either been disabled or is the current starter. Press any key to continue"
	CALL CSMsgBox(Message$, Wdth, Cnf)
	I$=GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)

	GOTO 4501
		END IF
		P1%(P9) = I1
		P6%(P9) = 1
		FOR I1 = 0 TO 21
	IF P%(P9, I1, 15) = 1 THEN
	   P%(P9, I1, 15) = 0
	END IF
		NEXT
		P%(P9, P1%(P9), 15) = 1
		SP%(P9) = P1%(P9)
		GOTO 4501
	END IF
	  IF SELECTION% = 2 THEN
		GOSUB 1520
		GOTO 4501
	END IF
	  IF SELECTION% = 3 THEN
		CALL SELECTPITCHER (P$(),P9,P%(),PIT$(),PIT%(),NP,I1)
		IF I1 = -1 THEN 4501
		IF P%(P9, I1, 15) = 0 THEN
	P%(P9, I1, 15) = 99
	GOTO 4501
		END IF
		IF P%(P9, I1, 15) = 99 THEN
	P%(P9, I1, 15) = 0
	GOTO 4501
		END IF
		GOTO 4501
	END IF
	IF SELECTION% = 4 THEN
		CALL DAYOFF (DR%,P%(),P9)
		
		'--> include this as part of the routine???
		LOCATE 9
		Message$ = "All pitchers have been granted a day's rest. Press any key to continue"	
		Wdth = 54
		CALL CSMsgBox(Message$, Wdth, Cnf)
		I$=GetKeyPress$
		CALL CSMsgBox("", 0, Cnf)
		
		GOTO 4501
	end if
	  IF SELECTION% = 5 THEN
		CALL ACTPITCHERS (P%(),P9)
		
		'--> include this as part of the routine???
		LOCATE 9
		Message$ = "All pitchers are now activated for duty. Press any key to continue"
		Wdth = 54		
		CALL CSMsgBox(Message$, Wdth, Cnf)
		I$=GetKeyPress$
		CALL CSMsgBox("", 0, Cnf)
		
		GOTO 4501
	end if
	  IF SELECTION% = 6 AND SP%(P9) = -1 THEN
		LOCATE 9
		Message$ = "A starting pitcher must be selected before proceeding. Press any key to continue"
		Wdth = 54		
		CALL CSMsgBox(Message$, Wdth, Cnf)
		I$=GetKeyPress$
		CALL CSMsgBox("", 0, Cnf)
		
		GOTO 4501
	ELSE
		RETURN
	END IF

4515 IF MF%(P9) = -1 THEN 4600
	IF MG%(P9, 99) <> 999 THEN 4600

4516 'AUTOMATIC DISABLE OF PITCHERS IN THE ROTATION

4517 RN = INT(RND(1) * 100) + 1
	P%(P9, MG%(P9, MG%(P9, 2) + 3), 15) = 0
	IF RN <= MG%(P9, MG%(P9, 2) + 10 + 3) OR MG%(P9, MG%(P9, 2) + 10 + 3) = 0 THEN
		P1%(P9) = MG%(P9, MG%(P9, 2) + 3)
		P6%(P9) = 1
		FOR I1 = 0 TO 21
	IF P%(P9, I1, 15) = 1 THEN
	   P%(P9, I1, 15) = 0
	END IF
		NEXT
		P%(P9, P1%(P9), 15) = 1
		SP%(P9) = P1%(P9)
		GOTO 4520
	END IF
	RN = INT(RND(1) * 100) + 1
	SP% = 0
	FOR I = 21 TO 36
	IF MG%(P9, I) = -1 OR MG%(P9, I) < 0 OR MG%(P9, I) > 21 THEN 4518
	SP% = SP% + MG%(P9, I + 20)
	IF RN <= SP% AND MG%(P9, I) <> -1 THEN
		P1%(P9) = MG%(P9, I)
		P6%(P9) = 1
		FOR I1 = 0 TO 21
	IF P%(P9, I1, 15) = 1 THEN
	   P%(P9, I1, 15) = 0
	END IF
		NEXT
		P%(P9, P1%(P9), 15) = 1
		SP%(P9) = P1%(P9)
		GOTO 4520
	END IF

4518 NEXT
	GOTO 4517

4520 MG%(P9, 2) = MG%(P9, 2) + 1: IF MG%(P9, 2) = MG%(P9, 1) THEN MG%(P9, 2) = 0

4522 MG%(P9, 99) = 999
	MG%(P9, 0) = 999
	MF%(P9) = 0
OPEN D$(3) + "MGR." + YN$(P9) FOR RANDOM AS #2 LEN = 2000
	FOR I = 0 TO 999
	FIELD #2, I * 2 AS Q$, 2 AS Q2$(I), 2000 - I * 2 - 2 AS Q$
	NEXT
	FOR I = 0 TO 999: LSET Q2$(I) = MKI$(MG%(P9, I)): NEXT
	PUT #2, T1(P9): CLOSE 2

4535 RETURN

4600 CR% = 1
		GS = 0
		FOR I = 0 TO 21
		IF P%(P9, I, 5) > GS THEN GS = P%(P9, I, 5)
		NEXT
		I = 1
		IF GS <= 75 THEN I = 2
		IF GS <= 54 THEN I = 3
		IF GS <= 41 THEN I = 4
		IF GS <= 32 THEN I = 5
	MG%(P9, 1) = I: MG%(P9, 2) = 0
	FOR I = 3 TO 36
	MG%(P9, I) = -1
	NEXT
	FOR I = 41 TO 56
	MG%(P9, I) = 0
	NEXT
	MG%(P9, 0) = 999
	  'COMPUTER SET ROTATION
	CR% = 0
	FOR I = 0 TO 21
	IF P$(P9, I) = "XXX" THEN GS%(I, 0) = 0: GS%(I, 1) = -1: GOTO 4691
	GS%(I, 0) = P%(P9, I, 5): GS%(I, 1) = I

4691 NEXT
	FOR Z = 0 TO 20: FOR Z1 = Z + 1 TO 21: IF GS%(Z, 0) >= GS%(Z1, 0) THEN 4695
	SWAP GS%(Z, 0), GS%(Z1, 0): SWAP GS%(Z, 1), GS%(Z1, 1)

4695 NEXT Z1: NEXT Z
	FOR I = 0 TO MG%(P9, 1) - 1
	MG%(P9, I + 3) = GS%(I, 1)
	NEXT
	I1 = 0
	FOR I = MG%(P9, 1) TO 21
	I1 = I1 + 1
	IF GS%(I, 0) = 0 THEN 4700
	MG%(P9, I1 + 20) = GS%(I, 1)
	NEXT

4700 I1 = I1 - 1: IF MG%(P9, 1) = 1 THEN MG%(P9, 13) = 100: GOTO 4516
	IF MG%(P9, 1) = 2 THEN GS% = 75
	IF MG%(P9, 1) = 3 THEN GS% = 54
	IF MG%(P9, 1) = 4 THEN GS% = 41
	IF MG%(P9, 1) = 5 THEN GS% = 32
	FOR I = 1 TO MG%(P9, 1)
	MG%(P9, I + 12) = INT(P%(P9, MG%(P9, I + 2), 5) * 100 / GS% + .5)
	NEXT
	SS% = 0
	FOR I = 21 TO 36
	IF MG%(P9, I) < 0 OR MG%(P9, I) > 21 THEN 4705
	SS% = SS% + P%(P9, MG%(P9, I), 5)

4705 NEXT
	FOR I = 21 TO 36
	IF MG%(P9, I) < 0 OR MG%(P9, I) > 21 THEN 4710
	MG%(P9, I + 20) = INT(P%(P9, MG%(P9, I), 5) * 100 / SS% + .5)

4710 NEXT
	GOTO 4516

5000 '*** COMPUTER DETERMINE LINEUPS ***
	FOR I = 0 TO 22: IF B$(P9, I) = "XXX" THEN B%(P9, I, 21) = 98
	NEXT
	GOSUB 2160: GOSUB 2310
	FOR I = 1 TO 9: F(I) = 0: BP(I) = 0: BB%(I) = -1: NEXT
	M = 0: FOR I = 9 TO 1 STEP -1: J = I: FOR I2 = 0 TO 22: PS%(I2, 0) = 0: PS%(I2, 1) = -1: NEXT: I2 = 0
	IF I = 1 THEN 5120
	FOR I1 = 0 TO 22: IF B%(P9, I1, 21) > 0 OR B$(P9, I1) = "XXX" THEN 5117
	IF B%(P9, I1, 22) = I AND B%(P9, I1, 23) = -1 THEN PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 22) = I AND B%(P9, I1, 24) = -1 THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .9 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 22) = I AND B%(P9, I1, 25) = -1 THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .8 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 22) = I THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .7 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 23) = I AND B%(P9, I1, 24) = -1 THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 23) = I AND B%(P9, I1, 25) = -1 THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 23) = I THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 24) = I AND B%(P9, I1, 25) = -1 THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 24) = I THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117
	IF B%(P9, I1, 25) = I THEN PS%(I2, 0) = INT(B%(P9, I1, 4) * .1 + .5): PS%(I2, 1) = I1: I2 = I2 + 1: GOTO 5117

5117 NEXT I1

5118 I2 = I2 - 1
	AB% = 0
	FOR I1 = 0 TO I2
	AB% = AB% + PS%(I1, 0)
	NEXT
	RN = INT(RND(1) * AB%) + 1
	FOR I1 = 0 TO I2
	IF RN <= PS%(I1, 0) THEN F(J) = PS%(I1, 1): B%(P9, PS%(I1, 1), 21) = 1: GOTO 5119
	PS%(I1 + 1, 0) = PS%(I1 + 1, 0) + PS%(I1, 0)
	NEXT I1
	LOCATE 25, 1: PRINT "CANNOT MAKE A LINEUP!!..."; : LE = 1
	FOR J = 1 TO 9: M%(J) = 0: B7%(P9, J) = -1: B3%(P9, J) = -1: NEXT: C1 = 0
	FOR J = 0 TO 22
	IF B%(P9, J, 21) <> 99 THEN B%(P9, J, 21) = 0
	NEXT
	IF AP% = 1 THEN 5163
5162 I$ = GetKeyPress$

5163 RETURN

5119 NEXT I: GOTO 5230

5120 IF DH% = 0 THEN BP(9) = 1: GOTO 5230
	J = J - 1
	I2 = 0: FOR I1 = 0 TO 22: IF B%(P9, I1, 21) > 0 OR B$(P9, I1) = "XXX" THEN 5122
	IF B%(P9, I1, 22) = 0 THEN PS%(I2, 0) = B%(P9, I1, 4): PS%(I2, 1) = I1: GOTO 5121
	IF B%(P9, I1, 23) = 0 THEN PS%(I2, 0) = B%(P9, I1, 4) * .5: PS%(I2, 1) = I1: GOTO 5121
	IF B%(P9, I1, 24) = 0 THEN PS%(I2, 0) = B%(P9, I1, 4) * .3: PS%(I2, 1) = I1: GOTO 5121
	IF B%(P9, I1, 25) = 0 THEN PS%(I2, 0) = B%(P9, I1, 4) * .2: PS%(I2, 1) = I1: GOTO 5121
	PS%(I2, 0) = INT(B%(P9, I1, 4) * .05 + .5): PS%(I2, 1) = I1

5121 I2 = I2 + 1

5122 NEXT I1
	GOTO 5118

5230 ' SORT HITTERS
	C = 1
	B = 3: GOSUB 5300
	B = 4: GOSUB 5300
	C = 2
	B = 1: GOSUB 5300
	B = 2: GOSUB 5300
	C = 1
	B = 5: GOSUB 5300
	B = 6: GOSUB 5300
	B = 7: GOSUB 5300
	B = 8: GOSUB 5300
	IF DH% = 1 THEN B = 9: GOSUB 5300
	FOR I = 1 TO 9
	B3%(P9, I) = BB%(I)
	B7%(P9, I) = BP(I)
	NEXT
	GOTO 1910

5300 E = -100: U = -1: T = -1
	FOR L = 0 TO 9
	IF DH% = 0 AND L = 0 THEN L = L + 2
	IF DH% = 1 AND L = 1 THEN L = L + 1
	J = F(L): X = 1
	FOR Z = 1 TO 9
	IF BB%(Z) = J THEN X = 0: Z = 9
	NEXT Z
	IF X = 0 THEN 5350
	IF C = 1 THEN 5345
	JJ = J: GOSUB 5355: T1% = 100 * B%(P9, J, 6) / B%(P9, J, 4) - 27
	IF T1% < 0 THEN T1% = 3 * T1%
	JJ = 3 * R + T1%: IF JJ > E THEN E = JJ: T = J: U = L
	GOTO 5350

5345 SE = (B%(P9, J, 6) + B%(P9, J, 7) + B%(P9, J, 8) * 2 + B%(P9, J, 9) * 3) / B%(P9, J, 4)
	IF SE >= E THEN E = SE: T = J: U = L

5350 NEXT L
	BB%(B) = T
	BP(B) = U
	RETURN

5355 'IF A PITCHER THEN 268
	Q1 = (B%(P9, JJ, 6) - B%(P9, JJ, 8) - B%(P9, JJ, 9) + B%(P9, JJ, 11)) * .7
	IF Q1 = 0 THEN Q1 = 1
	Q1 = B%(P9, JJ, 13) / Q1: IF Q1 < .02 THEN 5375
	QQ = Q1 + .18: R = INT(QQ * (18 + 2 * ABS((QQ < .34)))): GOTO 5380

5375 R = 2 + INT(Q1 * 100): IF B%(P9, JJ, 1) = 2 THEN R = R - 1

5380 IF (B%(P9, JJ, 1) = 6 OR B%(P9, JJ, 1) = 8) AND R < 7 THEN R = R + 1
	IF R > 9 THEN R = 9
	IF R < 0 THEN R = 1
	RETURN

8100 NT = 0
OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
	Y = LOF(1)
	FIELD #1, 15 AS Q2$(0)
	FOR I1 = 1 TO Y / 4342
	GET #1, I1
	FOR I2 = 1 TO 15
	IF LEFT$(Q2$(0),1) = " " OR ASC(MID$(Q2$(0), I2, 1)) < 32 OR ASC(MID$(Q2$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q2$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q2$(0), 3)) = "UN" THEN 8105
	NEXT I2
	NT = NT + 1
	V$(NT) = Q2$(0): UV%(NT) = I1

8105 NEXT I1
CLOSE 1

8102 SELECTION% = 1
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
MAMenu V$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN BO% = 1: RETURN
	T1(I) = UV%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."

300  OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q2$(0), 2 AS Q2$(1), 2 AS Q2$(2), 2 AS Q2$(3), 2 AS Q2$(4), 2 AS Q2$(5), 2 AS Q2$(6), 2 AS Q2$(7), 2 AS Q2$(8), 2 AS Q2$(9), 2 AS Q2$(10), 2 AS Q2$(11), 2 AS Q2$(12), 2 AS Q2$(13), 4 AS Q2$(14), 4297 AS X$
FOR A = 0 TO 22
FIELD #1, 45 + A * 99 AS X$, 15 AS Q2$(A * 43 + 15), 2 AS Q2$(A * 43 + 16), 2 AS Q2$(A * 43 + 17), 2 AS Q2$(A * 43 + 18), 2 AS Q2$(A * 43 + 19), 2 AS Q2$(A * 43 + 20), 2 AS Q2$(A * 43 + 21), 2 AS Q2$(A * 43 + 22), 2 AS Q2$(A * 43 + 23), 2 AS Q2$(A * _
43 + 24), 2 AS Q2$(A * 43 + 25), 2 AS Q2$(A * 43 + 26), 4297 - 37 - A * 99 AS X$
FIELD #1, 82 + A * 99 AS X$, 2 AS Q2$(A * 43 + 27), 2 AS Q2$(A * 43 + 28), 2 AS Q2$(A * 43 + 29), 2 AS Q2$(A * 43 + 30), 2 AS Q2$(A * 43 + 31), 2 AS Q2$(A * 43 + 32), 2 AS Q2$(A * 43 + 33), 2 AS Q2$(A * 43 + 34), 2 AS Q2$(A * 43 + 35), 2 AS Q2$(A *  _

43 + 36), 2 AS Q2$(A * 43 + 37), 2 AS Q2$(A * 43 + 38), 4297 - 61 - A * 99 AS X$
FIELD #1, 106 + A * 99 AS X$, 2 AS Q2$(A * 43 + 39), 2 AS Q2$(A * 43 + 40), 2 AS Q2$(A * 43 + 41), 2 AS Q2$(A * 43 + 42), 2 AS Q2$(A * 43 + 43), 2 AS Q2$(A * 43 + 44), 2 AS Q2$(A * 43 + 45), 2 AS Q2$(A * 43 + 46), 2 AS Q2$(A * 43 + 47), 2 AS Q2$(A * _
43 + 48), 2 AS Q2$(A * 43 + 49), 2 AS Q2$(A * 43 + 50), 4297 - 85 - A * 99 AS X$
FIELD #1, 130 + A * 99 AS X$, 2 AS Q2$(A * 43 + 51), 2 AS Q2$(A * 43 + 52), 2 AS Q2$(A * 43 + 53), 2 AS Q2$(A * 43 + 54), 2 AS Q2$(A * 43 + 55), 2 AS Q2$(A * 43 + 56), 2 AS Q2$(A * 43 + 57), 4297 - 99 - A * 99 AS X$
NEXT
FOR A = 0 TO 21
FIELD #1, 2322 + A * 89 AS X$, 15 AS Q2$(A * 38 + 1004), 2 AS Q2$(A * 38 + 1005), 2 AS Q2$(A * 38 + 1006), 2 AS Q2$(A * 38 + 1007), 2 AS Q2$(A * 38 + 1008), 2 AS Q2$(A * 38 + 1009), 2 AS Q2$(A * 38 + 1010), 2 AS Q2$(A * 38 + 1011), 2 AS Q2$(A * 38 + _
1012), 2 AS Q2$(A * 38 + 1013), 2 AS Q2$(A * 38 + 1014), 2 AS Q2$(A * 38 + 1015), 2020 - 37 - A * 89 AS X$
FIELD #1, 2359 + A * 89 AS X$, 2 AS Q2$(A * 38 + 1016), 2 AS Q2$(A * 38 + 1017), 2 AS Q2$(A * 38 + 1018), 2 AS Q2$(A * 38 + 1019), 2 AS Q2$(A * 38 + 1020), 2 AS Q2$(A * 38 + 1021), 2 AS Q2$(A * 38 + 1022), 2 AS Q2$(A * 38 + 1023), 2 AS Q2$(A * 38 +  _

1024), 2 AS Q2$(A * 38 + 1025), 2 AS Q2$(A * 38 + 1026), 2 AS Q2$(A * 38 + 1027), 2020 - 61 - A * 89 AS X$
FIELD #1, 2383 + A * 89 AS X$, 2 AS Q2$(A * 38 + 1028), 2 AS Q2$(A * 38 + 1029), 2 AS Q2$(A * 38 + 1030), 2 AS Q2$(A * 38 + 1031), 2 AS Q2$(A * 38 + 1032), 2 AS Q2$(A * 38 + 1033), 2 AS Q2$(A * 38 + 1034), 2 AS Q2$(A * 38 + 1035), 2 AS Q2$(A * 38 +  _

1036), 2 AS Q2$(A * 38 + 1037), 2 AS Q2$(A * 38 + 1038), 2 AS Q2$(A * 38 + 1039), 2020 - 85 - A * 89 AS X$
FIELD #1, 2407 + A * 89 AS X$, 2 AS Q2$(A * 38 + 1040), 2 AS Q2$(A * 38 + 1041), 2020 - 89 - A * 89 AS X$
NEXT
FIELD #1, 4280 AS X$, 40 AS Q2$(1840), 15 AS Q2$(1841), 5 AS Q2$(1842), 2 AS Q2$(1843)
GET #1, T1(I)
A$(I) = Q2$(0)
A$(I) = RTrim$(A$(I))
FOR I1 = 1 TO 13
L%(I, I1) = CVI(Q2$(I1))
NEXT
IF (L%(I, 11) = 7 OR L%(I, 11) = 15) AND L%(I, 12) = 0 THEN L%(I, 11) = 0: L%(I, 12) = 7
K9(I) = CVS(Q2$(14))
FOR I1 = 0 TO 22: B$(I, I1) = Q2$(I1 * 43 + 15)
B$(I, I1) = RTrim$(B$(I, I1))
FOR I2 = 0 TO 41
	B%(I, I1, I2) = CVI(Q2$(I1 * 43 + I2 + 16))
NEXT
IF B%(I, I1, 31) <> 0 THEN B%(I, I1, 21) = 99
NEXT
FOR I1 = 0 TO 21: P$(I, I1) = Q2$(I1 * 38 + 1004)
P$(I, I1) = RTrim$(P$(I, I1))
FOR I2 = 0 TO 36: P%(I, I1, I2) = CVI(Q2$(I1 * 38 + I2 + 1005))
NEXT
IF P%(I, I1, 34) <> 0 THEN P%(I, I1, 15) = 99
IF P%(I, I1, 0) = 0 THEN P%(I, I1, 0) = 1
NEXT
S$(I) = Q2$(1840): M$(I) = Q2$(1841): NN$(I) = Q2$(1842): CK = CVI(Q2$(1843))
CLOSE 1
FOR I1 = 0 TO 22

'check for 1934 season

IF VAL(LEFT$(A$(I), 2)) < 54 THEN B%(I, I1, 30) = B%(I, I1, 30) * .75
FOR I2 = 22 TO 25
IF B%(I, I1, I2) <> -1 AND B%(I, I1, I2) <> 0 AND B%(I, I1, I2 + 4) = 0 THEN B%(I, I1, I2 + 4) = 900
NEXT
NEXT
M$(I) = RTrim$(M$(I))
S$(I) = RTrim$(S$(I))
NN$(I) = RTrim$(NN$(I))
FOR X = 0 TO 22
FOR X1 = 1 TO 14
B%(I, X, X1 + 41) = 0
NEXT: NEXT
FOR X = 0 TO 21
FOR X1 = 1 TO 28
P%(I, X, X1 + 36) = 0
NEXT: NEXT
	IF LR% = 1 THEN TS%(I, 0) = -1: GOTO 316
	IF NOT _FILEEXISTS(D$(0) + "LR." + YN$) THEN TS%(I, 0) = -1: GOTO 316
OPEN D$(0) + "LR." + YN$ FOR RANDOM AS #1 LEN = 4108
	FOR X = 0 TO 11
	FIELD #1, X * 2 AS X$, 2 AS Q2$(X + 1), 4108 - 2 - 2 * X AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q2$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q2$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q2$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
	NEXT
	NEXT
	FOR X = 1 TO 24
	FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q2$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q2$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q2$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
	NEXT
	NEXT
	GET #1, T1(I)
	FOR X = 0 TO 11
	TS%(I, X) = CVI(Q2$(X + 1))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	B%(I, X, X1 + 41) = CVI(Q2$(12 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(I, X, X1 + 36) = CVI(Q2$(334 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(I, X, X1 + 50) = CVI(Q2$(642 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 1 TO 24: SA%(I, X) = CVI(Q2$(X + 950)): NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	B%(I, X, X1 + 55) = CVI(Q2$(975 + X1 + X * 24))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	P%(I, X, X1 + 64) = CVI(Q2$(1528 + X1 + X * 24))
	NEXT: NEXT
CLOSE 1
	IF TS%(I, 0) <> 999 THEN TS%(I, 0) = -1

316  FOR X = 0 TO 22
	FOR X1 = 0 TO 14
	B%(I, X, 80) = B%(I, X, 80) + B%(I, X, X1 + 41)
	NEXT: NEXT
	  FOR X = 0 TO 21
	FOR X1 = 0 TO 14
	P%(I, X, 89) = P%(I, X, 89) + P%(I, X, X1 + 50)
	NEXT: NEXT
	  IF NOT _FILEEXISTS(D$(3) + "MGR." + YN$) THEN MF%(I) = -1: GOTO 317
OPEN D$(3) + "MGR." + YN$ FOR RANDOM AS #2 LEN = 2000
	FOR X = 0 TO 999
	FIELD #2, X * 2 AS Q2$, 2 AS Q2$(X), 2000 - X * 2 - 2 AS Q2$
	NEXT
	GET #2, T1(I)
	FOR X = 0 TO 999: MG%(I, X) = CVI(Q2$(X)): NEXT
CLOSE 2

317  IF MG%(I, 0) <> 999 OR MF%(I) = -1 THEN MF%(I) = -1: FOR X = 0 TO 999: MG%(I, X) = -1: NEXT
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
	  F$ = MID$(N$(I), 4, QQ)
	F$ = F$ + "." + LEFT$(N$(I), 2)
	IF NOT _FILEEXISTS(D$(1) + F$ + "D") THEN 318
	  OPEN D$(1) + F$ + "D" FOR INPUT AS #1
	FOR I1 = 0 TO 22
	INPUT #1, INJ%(I, I1)
	NEXT
	FOR I1 = 0 TO 21
	INPUT #1, DYS%(I, I1)
	NEXT
CLOSE 1
	IF IJ% = 1 THEN 318
	FOR I1 = 0 TO 22
	IF INJ%(I, I1) > 0 THEN B%(I, I1, 21) = 99
	NEXT

318 IF DR% = 1 THEN 319
	FOR I1 = 0 TO 21
	IF DYS%(I, I1) = 0 OR DYS%(I, I1) >= 90 THEN 320
	P%(I, I1, 35) = DYS%(I, I1)

320 NEXT

319  RETURN

SUB COLCHK (P9, I)
	IF B%(P9, I, 21) >= 98 THEN COLOR 0, 7
END SUB

SUB DRAWBOX ()
	JJ = 5
	LOCATE 6, 8: COLOR L%(1, 11), L%(1, 12)
	PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
	FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
END SUB

SUB GETAKEY ()
	LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
4001 I$ = GetKeyPress$
END SUB

SUB GETWEATHER (WD%, WS%, MO%, WF!, SC%, TP%, RD)

IF L%(1, 13) > 71 THEN
	ID = INT(RND(1) * 71) + 1
ELSE
	ID = L%(1, 13)
END IF

'LAYOUT OF THE PARKS FILE IS
'NAME, CAPACITY, LF,LCF,CF,RCF,RF,GRASS,DOME,WINFAC
'6 MONTHS OF DATA, TEMP, DELTA,RL,LR,INL,INC,INR,OUTL,OUTC,OUTR,WS09,WS1014,WS1519,WS20,PRECIP

OPEN "PARKS.DBF" FOR RANDOM AS #1 LEN = 240
FIELD #1, 40 AS Q3$(0), 4 AS Q3$(1), 2 AS Q3$(2), 2 AS Q3$(3), 2 AS Q3$(4), 2 AS Q3$(5), 2 AS Q3$(6), 2 AS Q3$(7), 2 AS Q3$(8), 2 AS Q3$(9)
FOR I = 0 TO 5
FOR I1 = 0 TO 14
FIELD #1, 60 + I * 30 + I1 * 2 AS X$, 2 AS Q3$(10 + I1 + I * 15)
NEXT: NEXT

GET #1, ID
ATTEN = CVL(Q3$(1))
LF = CVI(Q3$(2)): LCF = CVI(Q3$(3))
CF = CVI(Q3$(4)): RCF = CVI(Q3$(5)): RF = CVI(Q3$(6))
TG = CVI(Q3$(7)): DOME = CVI(Q3$(8)): WINFAC = CVI(Q3$(9))
FOR I = 0 TO 5: FOR I1 = 0 TO 14
MON(I, I1) = CVI(Q3$(10 + I1 + I * 15))
NEXT: NEXT
CLOSE 1

IF MO% < 4 THEN MO% = 4
IF MO% > 9 THEN MO% = 9

SM% = MO% - 4

IF INT(RND(1) * 100) + 1 <= 50 THEN
	TP% = MON(SM%, 0) + INT(RND(1) * MON(SM%, 1) + 1)
ELSE
	TP% = MON(SM%, 0) - INT(RND(1) * MON(SM%, 1) + 1)
END IF

SUM = 0
FOR I = 2 TO 9
SUM = SUM + MON(SM%, I)
NEXT

RN = INT(RND(1) * SUM) + 1

SUM = 0
FOR I = 2 TO 9
	SUM = SUM + MON(SM%, I)
	IF RN <= SUM THEN 6105
NEXT

6105 WD% = I

SUM = 0
FOR I = 10 TO 13
SUM = SUM + MON(SM%, I)
NEXT

RN = INT(RND(1) * SUM) + 1

SUM = 0
FOR I = 10 TO 13
	SUM = SUM + MON(SM%, I)
	IF RN <= SUM THEN 6106
NEXT

6106 IF I = 10 THEN WS% = INT(RND(1) * 10)
	IF I = 11 THEN WS% = INT(RND(1) * 5) + 10
	IF I = 12 THEN WS% = INT(RND(1) * 5) + 15
	IF I = 13 THEN WS% = INT(RND(1) * 10) + 20

WF! = 1

IF WD% > 3 AND WD% < 7 AND WS% >= 15 THEN WF! = 1 - WINFAC / 100
IF WD% > 6 AND WD% < 10 AND WS% >= 15 THEN WF! = 1 + WINFAC / 100

PC% = 4
SC% = INT(RND(1) * 2) + 1

IF INT(RND(1) * 100) + 1 <= MON(SM%, 14) THEN
	PC% = INT(RND(1) * 3) + 1
	SC% = 3
	RN = INT(RND(1) * 100) + 1
	IF RN <= 5 THEN RD = 1: RE = INT(RND(1) * 3) + 6
END IF

IF L%(1, 13) = 2 OR L%(1, 13) = 4 OR L%(1, 13) = 10 OR L%(1, 13) = 13 OR L%(1, 13) = 24 THEN PC% = 4: SC% = 4
	  END SUB

SUB INFO ()
	CLS
	COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 33: COLOR 14
	PRINT "GAME OPTIONS MENU"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1991"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 3, 1: COLOR , 0
END SUB

SUB INITVARI ()
	FOR P9 = 0 TO 1: FOR I3 = 0 TO 21: IF P%(P9, I3, 15) <> 99 THEN P%(P9, I3, 15) = 0
	NEXT: NEXT
	FOR I = 0 TO 1: FOR I1 = 0 TO 23: X0%(I, 0, I1) = -1: X0%(I, 2, I1) = 100: NEXT I1: NEXT I
	FOR I = 0 TO 1: FOR I1 = 0 TO 22
	IF B%(I, I1, 21) <> 99 THEN B%(I, I1, 21) = 0
	NEXT: NEXT
END SUB

SUB LETTER (I$)
	IF UCASE$(I$) = "A" THEN I$ = "10"
	IF UCASE$(I$) = "B" THEN I$ = "11"
	IF UCASE$(I$) = "C" THEN I$ = "12"
	IF UCASE$(I$) = "D" THEN I$ = "13"
	IF UCASE$(I$) = "E" THEN I$ = "14"
	IF UCASE$(I$) = "F" THEN I$ = "15"
	IF UCASE$(I$) = "G" THEN I$ = "16"
	IF UCASE$(I$) = "H" THEN I$ = "17"
	IF UCASE$(I$) = "I" THEN I$ = "18"
	IF UCASE$(I$) = "J" THEN I$ = "19"
	IF UCASE$(I$) = "K" THEN I$ = "20"
	IF UCASE$(I$) = "L" THEN I$ = "21"
	IF UCASE$(I$) = "M" THEN I$ = "22"
END SUB

SUB LINSWAP (P9)
	LOCATE 18, 59: PRINT "B.O. PLYR 1 (1-9)";
1441 I$ = GetKeyPress$
	K1 = VAL(I$): IF K1 < 1 OR K1 > 9 THEN 1441
	IF B7%(P9, K1) = 1 THEN 1441
	LOCATE 19, 59: PRINT "B.O. PLYR 2 (1-9)";
1442 I$ = GetKeyPress$
	K2 = VAL(I$): IF K2 < 1 OR K2 > 9 THEN 1442
	IF B7%(P9, K2) = 1 THEN 1442
	SWAP B7%(P9, K2), B7%(P9, K1)
	SWAP B3%(P9, K1), B3%(P9, K2)
	B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
	B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
	CALL NEWLINES(P9)
	LOCATE 18, 59: PRINT "                 "
	LOCATE 19, 59: PRINT "                 "
END SUB

SUB NEWLINES (P9)
	FOR I = 1 TO 9
	CALL POSITION(I)
	PRINT USING "# "; I;
	IF B7%(P9, I) = 1 AND I5 <> 66 THEN PRINT P$(P9, B3%(P9, I));
	IF B7%(P9, I) <> 1 THEN PRINT B$(P9, B3%(P9, I));
	PRINT TAB(74);
	PRINT C$(B7%(P9, I));
	NEXT I
	I5 = 0
END SUB

SUB POSITION (I)
	LOCATE I + 7, 59
END SUB

SUB POSSWAP (P9)
	LOCATE 18, 59: PRINT "B.O. PLYR 1 (1-9)";
1431 I$ = GetKeyPress$
	K1 = VAL(I$): IF K1 < 1 OR K1 > 9 THEN 1431
	IF B7%(P9, K1) = 1 THEN 1431
	LOCATE 19, 59: PRINT "B.O. PLYR 2 (1-9)";
1432 I$ = GetKeyPress$
	K2 = VAL(I$): IF K2 < 1 OR K2 > 9 THEN 1432
	IF B7%(P9, K2) = 1 THEN 1432
	SWAP B7%(P9, K2), B7%(P9, K1)
	B9%(P9, B7%(P9, K1)) = B3%(P9, K1)
	B9%(P9, B7%(P9, K2)) = B3%(P9, K2)
	CALL NEWLINES(P9)
	LOCATE 18, 59: PRINT "                 "
	LOCATE 19, 59: PRINT "                 "
END SUB

SUB SHOWBATTERS (P9)
	CLS : COLOR 14: PRINT "#"; : COLOR L%(P9, 11), L%(P9, 12): PRINT A$(P9); : COLOR 14, 0
	LOCATE , 24: PRINT "B  POS   AB 2B 3B HR RBI  SB   AVG"
	LOCATE 2
	FOR I = 0 TO 22: LOCATE , 1
	IF B$(P9, I) = "XXX" OR LEFT$(B$(P9, I), 1) = " " OR B$(P9, I) = "" THEN B%(P9, I, 21) = 98: GOTO 1191
	COLOR 2, 0
	IF I < 10 THEN PRINT USING "#"; I;
	IF I > 9 THEN PRINT X$(I - 10);
	COLOR 15, 0
	CALL COLCHK(P9, I)
	LOCATE , 3: PRINT B$(P9, I);
	COLOR 15, 0
	LOCATE , 24: PRINT B1$(B%(P9, I, 0) + 2);
	LOCATE , 27: IF B%(P9, I, 22) <> -1 THEN PRINT USING "#"; B%(P9, I, 22);
	LOCATE , 28: IF B%(P9, I, 23) <> -1 THEN PRINT USING "#"; B%(P9, I, 23);
	LOCATE , 29: IF B%(P9, I, 24) <> -1 THEN PRINT USING "#"; B%(P9, I, 24);
	LOCATE , 30: IF B%(P9, I, 25) <> -1 THEN PRINT USING "#"; B%(P9, I, 25);
	LOCATE , 32
	PRINT USING "### ## ## ## ### ###"; B%(P9, I, 4); B%(P9, I, 7); B%(P9, I, 8); B%(P9, I, 9); B%(P9, I, 10); B%(P9, I, 13);
	LOCATE , 53
	IF B%(P9, I, 6) = B%(P9, I, 4) AND B%(P9, I, 6) <> 0 THEN PRINT "1.000"; : GOTO 1190
	IF B%(P9, I, 4) = 0 THEN PRINT " .---"; : GOTO 1190
	PRINT USING " .###"; B%(P9, I, 6) / B%(P9, I, 4);

1190 IF I <> 22 THEN PRINT

1191 NEXT I
END SUB

SUB SHOWFIELD (P9)
	'DISPLAY FIELDING AVERAGES
	LOCATE 1, 15
	COLOR 14, 0
	PRINT "P1 FAVG A R P2 FAVG A R P3 FAVG A R P4 FAVG A R                 "
	COLOR 15, 0
	LOCATE 2
	FOR I = 0 TO 22
	LOCATE , 15: PRINT "                                                                ";
	IF B$(P9, I) = "XXX" OR LEFT$(B$(P9, I), 1) = " " OR B$(P9, I) = "" THEN 2807
	LOCATE , 16: IF B%(P9, I, 22) <> -1 THEN PRINT USING "# #### # #"; B%(P9, I, 22); B%(P9, I, 26); B%(P9, I, 33); B%(P9, I, 34);
	LOCATE , 28: IF B%(P9, I, 23) <> -1 THEN PRINT USING "# #### # #"; B%(P9, I, 23); B%(P9, I, 27); B%(P9, I, 35); B%(P9, I, 36);
	LOCATE , 40: IF B%(P9, I, 24) <> -1 THEN PRINT USING "# #### # #"; B%(P9, I, 24); B%(P9, I, 28); B%(P9, I, 37); B%(P9, I, 38);
	LOCATE , 52: IF B%(P9, I, 25) <> -1 THEN PRINT USING "# #### # #"; B%(P9, I, 25); B%(P9, I, 29); B%(P9, I, 39); B%(P9, I, 40);
	IF I <> 22 THEN PRINT

2807 NEXT I
END SUB

SUB SHOWSPLIT (P9)
	'DISPLAY SPLIT STATS
	LOCATE 1, 15
	COLOR 14, 0
	PRINT "VS L  AB 2B 3B HR  BB   K   AVG  VS R  AB 2B 3B HR  BB   K   AVG"
	COLOR 15, 0
	LOCATE 2
	FOR I = 0 TO 22
	LOCATE , 15: PRINT "                                                                ";
	IF B$(P9, I) = "XXX" OR LEFT$(B$(P9, I), 1) = " " OR B$(P9, I) = "" THEN 2806
	LOCATE , 20: PRINT USING "### ## ## ## ### ### "; B%(P9, I, 42); B%(P9, I, 44); B%(P9, I, 45); B%(P9, I, 46); B%(P9, I, 47); B%(P9, I, 48);
	IF B%(P9, I, 42) = 0 OR B%(P9, I, 43) = 0 THEN PRINT " .000"; : GOTO 2802
	IF B%(P9, I, 42) = B%(P9, I, 43) THEN PRINT "1.000"; : GOTO 2802
	PRINT USING " .###"; B%(P9, I, 43) / B%(P9, I, 42);

2802 LOCATE , 53: PRINT USING "### ## ## ## ### ### "; B%(P9, I, 49); B%(P9, I, 51); B%(P9, I, 52); B%(P9, I, 53); B%(P9, I, 54); B%(P9, I, 55);
	IF B%(P9, I, 49) = 0 OR B%(P9, I, 50) = 0 THEN PRINT " .000"; : GOTO 2805
	IF B%(P9, I, 49) = B%(P9, I, 50) THEN PRINT "1.000"; : GOTO 2805
	PRINT USING " .###"; B%(P9, I, 50) / B%(P9, I, 49);

2805 IF I <> 22 THEN PRINT

2806 NEXT I
	
END SUB

SUB DAYOFF (DR%,P%(),P9)
	IF DR% <> 1 THEN
		FOR I1 = 0 TO 21
		IF P%(P9, I1, 35) > 0 THEN P%(P9, I1, 35) = P%(P9, I1, 35) - 1
		NEXT
	END IF
END SUB

SUB ACTPITCHERS (P%(),P9)
	FOR I3 = 0 TO 21
	IF P%(P9, I3, 15) = 99 AND P%(P9, I3, 34) = 0 THEN
		P%(P9, I3, 15) = 0
	END IF
	NEXT I3
END SUB

SUB SELECTPITCHER (P$(),P9,P%(),PIT$(),PIT%(),NP,SELECTION%)
	NP = 0
	FOR I = 0 TO 21
	IF LEFT$(P$(P9,I),1) = " " OR P$(P9, I) = "XXX" THEN 4504
	NP = NP + 1
	PIT$(NP) = P$(P9, I)
	PIT%(NP) = I

4504 NEXT
	Start% = 1

4503 SELECTION% = 1
	Count% = NP
	SCANCODE% = NP
	FileColr% = 15
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 2
	Column% = 3
	
_MouseShow "DEFAULT"
MMenuVert VARPTR(PIT$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN SELECTION% = -1:GOTO 4506
	IF SCANCODE% <> 13 AND SCANCODE% <> 1003 THEN 4503
	SELECTION% = PIT%(SELECTION%)

4506 END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
