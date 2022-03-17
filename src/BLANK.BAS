'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z

DIM O1(40), TM$(40), JS(46, 42) AS SINGLE, ZM%(46), NM$(46)
DIM H1$(40), PS$(40), D$(2)
DIM BA$(40), DFT(40, 20) AS SINGLE, ER$(40), OFT(40, 20) AS SINGLE
DIM O2(40), Z2$(255), Z3$(255), Z1(255, 2) AS SINGLE, BAT(24), PIT(23)
DIM J AS SINGLE

4 OPEN "DEV.BAS" FOR INPUT AS #2
	FOR I = 0 TO 2
		INPUT #2, D$(I)
	NEXT
CLOSE 2

5 CLS

APP = 0

4000 Spec$ = D$(2) + "*.LIB"
	null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
	Count% = DirCount%
	IF Count% = 0 THEN 4500
REDIM F$(0 TO Count%)
	FOR X = 1 TO Count%
	  F$(X) = SPACE$(12)
	NEXT
	F$(0) = Spec$
	CALL ReadFile(F$())
	'CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 CLS
	COLOR , 1
	CLS
	LOCATE , 28
	COLOR 14
	PRINT "SELECT LEAGUE TO ERASE": COLOR 15
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
	PRINT "-SELECTS LEAGUE   ";
	COLOR 14
	PRINT "HIT ESCAPE KEY TO EXIT";
	COLOR 15

_MouseShow "DEFAULT"
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 99
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 F$ = RTrim$(F$)
	WE$ = LEFT$(F$, LEN(F$) - 4)
	LH$ = LEFT$(WE$, 8)

15 CLS
	LOCATE 10, 8
	CALL CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	IF I$ <> "Y" THEN 1310

30 OPEN D$(2) + LH$ + ".TMS" FOR INPUT AS #2
	INPUT #2, LG$, D1$, D2$, D3$, D4$
	FOR XW = 1 TO 40

35 INPUT #2, H1$(XW)
	NEXT
CLOSE 2

71 FOR XW = 1 TO 40
	IF H1$(XW) = CHR$(42) THEN 74

801 PRINT "DELETING "; H1$(XW); " STATS"
	KILL (D$(1) + H1$(XW))
	KILL (D$(1) + H1$(XW) + "R")
	KILL (D$(1) + H1$(XW) + "S")
	KILL (D$(1) + H1$(XW) + "D")

74 NEXT
	CLS
	GOTO 4000

CLS 

103 RUN "HELLO.EXE"

4500 LOCATE 10, 8
	CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Cnf)
	GOTO 99


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
