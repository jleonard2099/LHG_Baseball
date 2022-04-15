'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z

DECLARE SUB AddInt (SEG AElement, Value, NumEls)
DECLARE SUB InitInt (SEG AElement, StartValue, NumEls)
DECLARE SUB ISortI (SEG Element%, SEG IndexElement%, NumElements%, Direction%)

DIM H(4), DC(3), SC%(3400, 1), SC$(3400, 1), SCE(3400), X%(40, 1), GAME(3400)
DIM DX(10000)
DIM X$(40, 1), Z1%(40), Z1$(40), D$(2), Z2$(40)
RANDOMIZE TIMER

OPEN "DEV.BAS" FOR INPUT AS #2: FOR I = 0 TO 2: INPUT #2, D$(I): NEXT: CLOSE 2
	'DISPLAY LEAGUE ON THE SCREEN

4000 Spec$ = D$(2) + "*.LIB"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
	REDIM leagueFiles$(0 TO Count%)
	FOR X = 1 TO Count%
	leagueFiles$(X) = SPACE$(12)
	NEXT
	leagueFiles$(0) = Spec$
	CALL ReadFile(leagueFiles$())
	'CALL SortStr(VARPTR(leagueFiles$(1)), Count%, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO SCHEDULE": COLOR 15
	SELECTION% = 1
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	LOCATE 25, 1
	COLOR 14
	PRINT "ENTER"; : COLOR 15
	PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu leagueFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN END
	targetFile$ = leagueFiles$(SELECTION%)
	LOCATE 2, 33: PRINT targetFile$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 targetFile$ = RTrim$(targetFile$)
	LG$ = LEFT$(targetFile$, LEN(targetFile$) - 4)
OPEN D$(2) + LG$ + ".LIB" FOR INPUT AS #2
	INPUT #2, LG$: INPUT #2, D1$: INPUT #2, D2$: INPUT #2, D3$: INPUT #2, D4$
	FOR X = 1 TO 40
	INPUT #2, Z1$(X): INPUT #2, Z1%(X): INPUT #2, Z2$(X)
	NEXT
CLOSE 2
	COLOR 15, 1
	GOTO 25

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)

	END
	'GOTO 50

25      FOR I = 1 TO 40
		IF I <= 10 AND Z1$(I) <> "XXX" THEN DC(0) = DC(0) + 1
		IF I > 10 AND I <= 20 AND Z1$(I) <> "XXX" THEN DC(1) = DC(1) + 1
		IF I > 20 AND I <= 30 AND Z1$(I) <> "XXX" THEN DC(2) = DC(2) + 1
		IF I > 30 AND I <= 40 AND Z1$(I) <> "XXX" THEN DC(3) = DC(3) + 1
		NEXT
	A = 4
		IF DC(3) = 0 THEN A = 3
		IF DC(2) = 0 THEN A = 2
		IF DC(1) = 0 THEN A = 1
		IF A = 1 THEN B = DC(0)
		IF A = 2 AND DC(0) = DC(1) THEN B = DC(0)
		IF A = 3 AND DC(0) = DC(1) AND DC(1) = DC(2) THEN B = DC(0)
		IF A = 4 AND DC(0) = DC(1) AND DC(1) = DC(2) AND DC(2) = DC(3) THEN B = DC(0)
		IF B < 2 THEN PRINT "NOT ENOUGH TMS IN EACH DIV. MUST BE AT LEAST 2": END
		IF A = 1 OR A = 2 AND DC(0) = DC(1) OR A = 3 AND DC(0) = DC(1) AND DC(1) = DC(2) OR A = 4 AND DC(0) = DC(1) AND DC(1) = DC(2) AND DC(2) = DC(3) THEN 100
		PRINT "ALL FOUR DIV NOT EQUAL SIZE": END

100 CLS

	CALL CSMsgBox("NOW YOU WILL DETERMINE THE NUMBER OF GAMES FOR EACH TEAM IN EACH DIVISION. THE MAXIMUM NUMBER OF GAMES YOU CAN SCHEDULE IS 170 GAMES", 26, Pass)
	I$=GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	
	LOCATE 12, 1: PRINT "# OF TIMES PLAY TEAMS IN OWN DIV : "; : INPUT C
		LOCATE 13, 1: PRINT "# OF TIMES PLAY TEAMS IN OTHER DIV : "; : INPUT D
		LOCATE 15, 1: PRINT "THE # OF GAMES FOR EACH TEAM IS : "; :
		PRINT C * (B - 1) + (A - 1) * B * D
		IF C * (B - 1) + (A - 1) * B * D > 170 THEN 100
		CLS : PRINT "NOW YOU WILL DETERMINE THE NUMBER OF DIVISION HOME "
		PRINT "GAMES AGAINST EACH TEAM IN THE DIVISION"
		LOCATE 5, 1: PRINT "0 - # OF 1 GAME SERIES              : "; : INPUT E
		LOCATE 6, 1: PRINT "1 - # IF 2 GAME SERIES              : "; : INPUT F
		LOCATE 7, 1: PRINT "2 - # OF 3 GAME SERIES              : "; : INPUT G
		LOCATE 8, 1: PRINT "3 - # OF 4 GAME SERIES (NO DBL HDR) : "; : INPUT H
		LOCATE 9, 1: PRINT "4 - # OF 4 GAME SERIES (DBL HDR)    : "; : INPUT I
		PRINT "TOTAL # OF DIVISION HOME GAMES "; ((C * (B - 1)) / 2)
		PRINT "TOTAL YOU HAVE SCHEULED "; (E + (F * 2) + (G * 3) + ((H + I) * 4)) * (B - 1)
		PRINT "YOU STILL NEED TO SCHEDULE ";
		PRINT "ANY CHANGE ROUTINE"
110 I$ = GetKeyPress$
		'  "NON - DIV HOME GAMES"
	CLS : PRINT "NOW YOU WILL DETERMINE THE NUMBER OF NON-DIV HOME GAMES"
		PRINT "AGAINST EACH TEAM IN EACH DIVISION"
		LOCATE 5, 1: PRINT "0 - # OF 1 GAME SERIES              : "; : INPUT J
		LOCATE 5, 1: PRINT "1 - # OF 2 GAME SERIES              : "; : INPUT K
		LOCATE 7, 1: PRINT "2 - # OF 3 GAME SERIES              : "; : INPUT L
		LOCATE 8, 1: PRINT "3 - # OF 4 GAME SERIES (NO DBL HDR) : "; : INPUT M
		LOCATE 9, 1: PRINT "4 - # OF 4 GAME SERIES (DBL HDR)    : "; : INPUT N
		LOCATE 12, 1: PRINT "TOTAL # OF NON-DIV GAMES "; : PRINT ((A - 1) * B * D) / 2
		LOCATE 13, 1: PRINT "TOTAL YOU HAVE SCHEDULES "; (J + (K * 2) + (L * 3) + ((M + N) * 4)) * B
		LOCATE 14, 1: PRINT "YOU STILL NEED TO SCHEDULE ";
		PRINT ";ANY CHANGE ROUTINE"
	CLS : PRINT "ENTER DATE SEASON TO BEGIN ON :"
		INPUT "XX/XX/XX"; targetFile$
		'SELECT C VS C , DH NO DH , RULES ETC.
		';CALC # OF DAYS IN SN.
		IF (B / 2) - INT(B / 2) = 0 THEN SX = C * (B - 1) - ((N + I) * 2): E0 = 0: 'EVEN # OF TMS/DIV
		IF (B / 2) - INT(B / 2) > 0 THEN SX = C * (B - 1) + C - ((N + I) * 2): E0 = 1: REM;ODD # TMS/DIV
		GD = INT(B / 2): ' ; GAMES/DAY
		SD = 10000
		IF E0 = 0 THEN XX = B - 1: ' ; EVEN # OF TMS FOR ROTATING
		IF E0 = 1 THEN XX = B: '   ; ODD # OF TMS FOR ROTATING

DODIV:
		DV = 0: GOSUB DIV
		IF A > 1 THEN DV = 10: GOSUB DIV
		IF A > 2 THEN DV = 20: GOSUB DIV
		IF A > 3 THEN DV = 30: GOSUB DIV
		GOTO NONDIV
DIV:
		GOSUB 9000
		PRINT E, F, G, H, I, YY, NN, GM
22 I$ = GetKeyPress$
		YY = E: NN = 1: IF YY > 0 THEN GOSUB 9005
		PRINT E, F, G, H, I, YY, NN, GM
23 I$ = GetKeyPress$
		YY = F: NN = 2: IF YY > 0 THEN GOSUB 9005
		PRINT E, F, G, H, I, YY, NN, GM
24 I$ = GetKeyPress$
		YY = G: NN = 3: IF YY > 0 THEN GOSUB 9005
		PRINT E, F, G, H, I, YY, NN, GM
28 I$ = GetKeyPress$
		YY = H: NN = 4: IF YY > 0 THEN GOSUB 9005
		PRINT E, F, G, H, I, YY, NN, GM
26 I$ = GetKeyPress$
		YY = I: NN = 4: DH = 1: IF YY > 0 THEN GOSUB 9005
		PRINT E, F, G, H, I, YY, NN, GM
27 I$ = GetKeyPress$
		DH = 0
		RETURN

9005 :
		'T1 = 1
		FOR QQ = 1 TO YY
		T1 = 1
		PRINT "QQ,A="; QQ, A
		GOSUB 9000
		GOSUB 9010
		NEXT QQ
		RETURN

9010 :
		MM = INT(RND(1) * B) + 1
		IF X%(MM, 0) <> -1 THEN 9010
		X%(MM, 0) = Z1%(T1 + DV): ' ; RANDOMLY LOAD TM# FROM LL FILE
		X$(MM, 0) = Z2$(T1 + DV): ' ; RANDOMLY LOAD ID# FROM LL FILE
		T1 = T1 + 1: IF T1 > B THEN 500
		GOTO 9010

500  
		PRINT "@500"
		FOR RR = 1 TO B - 1
		IF E0 = 0 THEN X%(B - 1, 0) = X%(1, 0): X$(B - 1, 0) = X$(1, 0)
		IF E0 = 1 THEN X%(B, 0) = X%(1, 0): X$(B, 0) = X$(1, 0): ' ; ODD ROTATE
		FOR DD = 2 TO XX
		X%(DD - 1, 0) = X%(DD, 0): X$(DD - 1, 0) = X$(DD, 0)
		NEXT DD

505     PRINT "@505"
		DY = INT(RND(1) * SD) + 1
		PRINT "DY="; DY
		IF DX(DY) > 0 THEN 505
		DX(DY) = 1
		'GET HOME DAY #

510     PRINT "@510"
		DY1 = INT(RND(1) * SD) + 1
		PRINT DY1
		IF DX(DY1) > 0 THEN 510
		DX(DY1) = 1
	FOR PP = 1 TO NN
		IF N = 4 AND DH = 1 THEN 520
		DY = DY + 1
		DY1 = DY1 + 1

520
		GM = GM + 1
		PRINT "GM="; GM, B
		SC%(GM, 0) = X%(1, 0): SC$(GM, 0) = X$(1, 0): SC%(GM, 1) = X%(2, 0): SC$(GM, 1) = X$(2, 0)
		SCE(GM) = DY
		GM = GM + 1
		SC%(GM, 0) = X%(2, 0): SC$(GM, 0) = X$(2, 0): SC%(GM, 1) = X%(1, 0): SC$(GM, 1) = X$(1, 0)
		SCE(GM) = DY1
		IF B >= 4 THEN GM = GM + 1: SC%(GM, 0) = X%(3, 0): SC$(GM, 0) = X$(3, 0): SC%(GM, 1) = X%(4, 0): SC$(GM, 1) = X$(4, 0): SCE(GM) = DY
		IF B >= 4 THEN GM = GM + 1: SC%(GM, 0) = X%(4, 0): SC$(GM, 0) = X$(4, 0): SC%(GM, 1) = X%(3, 0): SC$(GM, 1) = X$(3, 0): SCE(GM) = DY1
		IF B >= 6 THEN GM = GM + 1: SC%(GM, 0) = X%(5, 0): SC$(GM, 0) = X$(5, 0): SC%(GM, 1) = X%(6, 0): SC$(GM, 1) = X$(6, 0): SCE(GM) = DY
		IF B >= 6 THEN GM = GM + 1: SC%(GM, 0) = X%(6, 0): SC$(GM, 0) = X$(6, 0): SC%(GM, 1) = X%(5, 0): SC$(GM, 1) = X$(5, 0): SCE(GM) = DY1
		IF B >= 8 THEN GM = GM + 1: SC%(GM, 0) = X%(7, 0): SC$(GM, 0) = X$(7, 0): SC%(GM, 1) = X%(8, 0): SC$(GM, 1) = X$(8, 0): SCE(GM) = DY
		IF B >= 8 THEN GM = GM + 1: SC%(GM, 0) = X%(8, 0): SC$(GM, 0) = X$(8, 0): SC%(GM, 1) = X%(7, 0): SC$(GM, 1) = X$(7, 0): SCE(GM) = DY1
		IF B = 10 THEN GM = GM + 1: SC%(GM, 0) = X%(9, 0): SC$(GM, 0) = X$(9, 0): SC%(GM, 1) = X%(10, 0): SC$(GM, 1) = X$(10, 0): SCE(GM) = DY
		IF B = 10 THEN GM = GM + 1: SC%(GM, 0) = X%(10, 0): SC$(GM, 0) = X$(10, 0): SC%(GM, 1) = X%(9, 0): SC$(GM, 1) = X$(9, 0): SCE(GM) = DY1
		NEXT PP
		NEXT RR
		RETURN
NONDIV:
		IF A = 1 THEN 7000
		GOSUB 9000
		DV = 0: DV1 = 10: GOSUB DOYY
		GOTO DIV3
DO1:
		T1 = 1
		FOR QQ = 1 TO YY
		GOSUB 7010
		NEXT QQ
		RETURN

7010 :
		MM = INT(RND(1) * B) + 1
		IF X%(MM + DV, 0) <> -1 THEN 7010
		X%(MM + DV, 0) = Z1%(T1 + DV)
		X$(MM + DV, 0) = Z2$(T1 + DV)
		T1 = T1 + 1: IF T1 > B THEN T1 = 1: GOTO 9015
		GOTO 7010

9015 :
		MM = INT(RND(1) * B) + 1
		IF X%(MM + DV1, 1) <> -1 THEN 9015
		X%(MM + DV1, 1) = Z1%(T1 + DV1)
		X$(MM + DV1, 1) = Z2$(T1 + DV1)
		T1 = T1 + 1: IF T1 > B THEN T1 = 1: GOTO ROTAT1
		GOTO 7010
ROTAT1:
		XX = B - 1
		FOR RR = 1 TO B
		X%(B, 1) = X%(1, 1): X$(B, 1) = X$(1, 1)
		FOR DD = 2 TO XX
		X%(DD - 1, 1) = X%(DD, 1): X$(DD - 1, 1) = X$(DD, 1): NEXT DD

7020
		DY = INT(RND(1) * SD) + 1
		IF DX(DY) > 0 THEN 7020
		DX(DY) = 1

7030
		DY1 = INT(RND(1) * SD) + 1
		IF DX(DY1) > 0 THEN 7030
		DX(DY1) = 1
		FOR PP = 1 TO NN
		IF NN = 4 AND DH = 1 THEN 7040
		DY = DY + .1
		DY1 = DY1 + .1

7040
		FOR TT = 1 TO B: GM = GM + 1
		SC%(GM, 0) = X%(TT + DV, 0): SC$(GM, 0) = X$(TT + DV, 0)
		SC%(GM, 1) = X%(TT + DV1, 1): SC$(GM, 1) = X$(TT + DV1, 1)
		SCE(GM) = DY
		GM = GM + 1
		SC%(GM, 0) = X%(TT + DV1, 1): SC$(GM, 0) = X$(TT + DV1, 1)
		SC%(GM, 1) = X%(TT + DV, 0): SC$(GM, 1) = X$(TT + DV, 0)
		SCE(GM) = DY1
		NEXT TT
		NEXT PP
		NEXT RR
		RETURN
DIV3:
		IF A <= 2 THEN 7000
		GOSUB 9000
		DV = 0: DV1 = 20: GOSUB DOYY
		GOSUB 9000
		DV = 10: DV1 = 20: GOSUB DOYY
DIV4:
		IF A <= 3 THEN 7000
		GOSUB 9000
		DV = 0: DV1 = 30: GOSUB DOYY
		GOSUB 9000
		DV = 10: DV1 = 30: GOSUB DOYY
		GOSUB 9000
		DV = 20: DV1 = 30: GOSUB DOYY

7000 :
		CLS : PRINT "SORTING SCHEDULE..."
		M = GM: L = GM - 1
		PRINT M, L
SORT1:
		S6 = 0: S7 = 0
SORT2:
		InitInt GAME(1), 0, GM           'initialize the array (base zero)
		ISortI SCE(1), GAME(1), GM, Direction
		AddInt GAME(1), 1, GM            'add one to every element (needed
	'  unless starting at element zero)
	OPEN "SCHED" FOR OUTPUT AS #2
		FOR I = 1 TO GM
'        PRINT #2, SCE(I), SC%(I, 0), SC%(I, 1), SC$(I, 0), SC$(I, 1)
		PRINT #2, GAME(I), SCE(GAME(I)), SCE(I)
		NEXT
	CLOSE 2
	END

8020
		FOR I = 1 TO GM
		D = D + 100: X = INT(D / 100)
		IF X = 431 THEN D = D + 7000
		IF X = 532 THEN D = D + 6900
		IF X = 631 THEN D = D + 7000
		IF X = 732 THEN D = D + 6900
		IF X = 832 THEN D = D + 6900
		IF X = 931 THEN D = D + 7000
		IF X = 1032 THEN D = D + 6900
		IF X = 1131 THEN D = D + 7000
		IF X = 1232 THEN D = D - 113099
		IF X = 132 THEN D = D + 6900
		IF X = 229 THEN D = D + 7200
		IF X = 332 THEN D = D + 6900
		'PUT IN C VS C
		'DAY NIGHT ETC
		'WRITE IT OUT
	NEXT I
		STOP
		END
	
DOYY:
		YY = J: NN = 1: IF YY > 0 THEN GOSUB DO1
		YY = K: NN = 2: IF YY > 0 THEN GOSUB DO1
		YY = L: NN = 3: IF YY > 0 THEN GOSUB DO1
		YY = M: NN = 4: IF YY > 0 THEN GOSUB DO1
		YY = N: NN = 4: DH = 1: IF YY > 0 THEN GOSUB DO1
		DH = 0
		RETURN

9000 :
		FOR Q2 = 1 TO 40: FOR Q1 = 0 TO 1: X%(Q2, Q1) = -1: X$(Q2, Q1) = "": NEXT: NEXT
		RETURN

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
