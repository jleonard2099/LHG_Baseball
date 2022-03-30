'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - View Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon	
_AllowFullScreen

DECLARE SUB TITLE ()

DIM O%(100), O$(100), T%(100), P%(100), Q$(500), teamItems$(500)
DIM Z2(14, 14), Z(15), Z1(15), VV%(500)
DIM A$(14), D$(1), L%(4)
DIM A(14, 24)
DIM H$(100), CP$(25)
DIM W0(14), W1(14), AP%(100, 1), CRD(100), SITE$(100)
	CP% = 1
	'DATA NAME,...
	'DATA NAME,...,TO
	FOR I = 0 TO 25: READ CP$(I): NEXT

OPEN "DEV.BAS" FOR INPUT AS #1: INPUT #1, D$(0): INPUT #1, D$(1): CLOSE 1

45 COLOR 15, 0: CLS

750 CALL TITLE

' This may be in the wrong spot
	menuItems$(1) = "VIEW / PRINT COMPILED STATS FOR ONE TEAM"
	menuItems$(2) = "PRINT REPORTS FOR ALL TEAMS IN A LEAGUE "
	menuItems$(3) = "RETURN TO MAIN MENU    "
	SELECTION% = 1
	Count% = 5
	Start% = 1
	SCANCODE% = 5
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 25

CALL box0(7, 24, 13, 58, 2, 31)
	
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN GOSUB 3056; GOSUB 6585
	IF SELECTION% = 2 THEN CALL PRINTREPORTS()
	IF SELECTION% = 3 THEN 1
		Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)

90 IF UCASE$(Z$) = "END" THEN RUN "HELLO.EXE 4"
	'*** READ IN TEAM STATS ***
	IF NOT _FILEEXISTS(D$(1) + Z$) THEN 5025

3056 COLOR 15, 0
	JJ = 6
	SN = 1: LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	PRINT
	LOCATE , 12: PRINT "ESC TO RETURN TO MAIN MENU"
3057 I$ = GetKeyPress$
	IF I$ = CHR$(27) THEN Z$ = "END": GOTO 90
	IF I$ = CHR$(13) THEN GOSUB 1102: GOTO 3060
	LOCATE 7, 27: PRINT I$
	LOCATE 7, 28: LINE INPUT NY$
	NY$ = I$ + NY$
	COLOR 15, 0

	IF NY$ = "" THEN GOSUB 1102: GOTO 3060
	YN$ = NY$
	FOR I = 1 TO 500: teamItems$(I) = "": NEXT
	GOSUB 1100

3060 IF BO% = 1 THEN BO% = 0: GOTO 3056

3058 LOCATE 2, 33: COLOR T%(0), T%(1): PRINT A$
	COLOR 15, 4: LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 3056
	COLOR 15, 0
	RETURN

1100 OPEN D$(0) + "BASK." + YN$ FOR RANDOM AS #1 LEN = 4342
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0), 640 AS X$
	FOR I1 = 1 TO Y / 655
	GET #1, I1
	teamItems$(I1) = Q$(0): VV%(I1) = I1
	NEXT
CLOSE 1
	NTMS = Y / 655

1102 SELECTION% = 1
	Count% = NTMS
	Start% = 1
	FileFGColr% = 15:FileBGColr% = 4
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	COLOR , 4: CLS : LOCATE , 33: COLOR 14
	PRINT "TEAM SELECTION": COLOR 15
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
	PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
	PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
	
_MouseShow "DEFAULT"
20   MAMenu teamItems$(), SELECTION%, Start%, Count%, EXITCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF EXITCODE% = 27 THEN BO% = 1: RETURN
	IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 20
	IF EXITCODE% = -79 THEN Start% = NTMS: EXITCODE% = 3: SELECTION% = NTMS: GOTO 20
	IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
	IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NTMS THEN Start% = NTMS
	IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 20
	T1 = VV%(SELECTION%)

9000 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342

' read data in to fields

GET #1, T1
	A$ = Q$(0)

9005 IF RIGHT$(A$, 1) = " " THEN A$ = LEFT$(A$, LEN(A$) - 1): GOTO 9005
	B$ = A$
	SP = CVI(Q$(1)): DA = CVI(Q$(2)): V1% = CVI(Q$(3)): V2% = CVI(Q$(4))
	FOR I = 0 TO 4: L%(I) = CVI(Q$(I + 5)): NEXT
	FOR I = 0 TO 9: T%(I) = CVI(Q$(I + 10)): NEXT
CLOSE 1
	B$ = A$
	RETURN

6585 FOR XX = 4 TO 11: IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
	NEXT XX
	FOR XX = 12 TO 14: IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
	NEXT XX
	QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
	GOTO 6620

6615 NEXT XX

6620 RETURN

4500 COLOR 15, 0: LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
4501 I$ = GetKeyPress$
	RETURN

5025 CLS : PRINT "ERROR::: UNABLE TO LOCATE COMPILED STATS FILE"
	RETURN
	GOTO 45

895 COLOR 7: LOCATE 24, 1: PRINT "ANY KEY TO CONTINUE";
896 I$ = GetKeyPress$
	RETURN


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
SUB TITLE ()
	COLOR , 2
	LOCATE 1, 1: PRINT TAB(80); : LOCATE , 26: COLOR 0
	PRINT "FULL COUNT BASEBALL STATISTICS"
	LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
	PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; TAB(70); LEFT$(TIME$, 5);
	LOCATE 4, 1: COLOR 2, 0
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
