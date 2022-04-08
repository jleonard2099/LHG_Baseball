'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Create League"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon	
_AllowFullScreen

DEFINT A-Z
DECLARE FUNCTION PadRight$ (X$, Length)

DIM Z1$(40), Z2$(40), Z1%(40), D$(2)
DIM Q$(15), teamItems$(200), V%(200), T%(34), L%(13)
DIM Y AS LONG

'determine if the monitor is mono or color
FG = 0
BG = 7
Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte

OPEN "DEV.BAS" FOR INPUT AS #2
	FOR I = 0 TO 2
	INPUT #2, D$(I)
	NEXT
CLOSE 2
	CLS

50 FOR X = 1 TO 40
	Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
	NEXT

60 CLS
	COLOR 12: PRINT TAB(31); "LEAGUE EDITOR MENU": PRINT

70 COLOR 14: PRINT "(1)"; : COLOR 15: PRINT " EDIT AN EXISTING LEAGUE": PRINT

80 COLOR 14: PRINT "(2)"; : COLOR 15: PRINT " CREATE A NEW LEAGUE": PRINT

90 COLOR 14: PRINT "(3)"; : COLOR 15: PRINT " RETURN TO STANDINGS/LEADERS MENU"
100 I$ = GetKeyPress$

110 I1 = VAL(I$)
	IF I1 < 1 OR I1 > 3 THEN 100
	IF I1 = 1 THEN 4000

120 IF I1 = 3 OR I$ = CHR$(27) THEN 1195

130 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 1
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER NAME FOR LEAGUE: ": PRINT
	LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
	LOCATE , 12: PRINT "SPACES."
	LOCATE , 12: PRINT "SAMPLE NAMES ARE NL1990 AL56 1956AL NATIONAL"
	LOCATE 7, 37: INPUT LG$
	IF LG$ = "" THEN 130
	LG$ = LEFT$(LG$, 8)
	LOCATE 13, 12: PRINT "ANY CHANGE (YN) "

22  I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 130

25  CLS
	COLOR 15, 1
	LOCATE 5, 1: QPrint "Name for Division 1 ", 30, -1
	LOCATE , 21: QPrint PadRight$(D1$, 8), FG, BG, -1
	LOCATE 7, 1: QPrint "Name for Division 2 ", 30, -1
	LOCATE , 21: QPrint PadRight$(D2$, 8), FG, BG, -1
	LOCATE 9, 1: QPrint "Name for Division 3 ", 30, -1
	LOCATE , 21: QPrint PadRight$(D3$, 8), FG, BG, -1
	LOCATE 11, 1: QPrint "Name for Division 4 ", 30, -1
	LOCATE , 21: QPrint PadRight$(D4$, 8), FG, BG, -1

GetDiv1:
	LOCATE 5, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 8     'specify field parameters
	CALL TextIn(D1$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv2:
	LOCATE 7, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 8     'specify field parameters
	CALL TextIn(D2$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv3:
	LOCATE 9, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 8     'specify field parameters
	CALL TextIn(D3$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv2:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv4:
	LOCATE 11, 21                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 8     'specify field parameters
	CALL TextIn(D4$, Max, NumOnly, CapsOn, XCode, Colr)        'get input
	IF XCode = 1 GOTO GetDiv3:           'can't back up any more, stay here

Done:
	LOCATE 23, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, FG, BG)
	LOCATE , 1
	ClearEOL 14, 1
	IF XCode = 1 GOTO GetDiv4
	IF X$ = "N" GOTO GetDiv1
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow

220 COLOR 14, 0
	CLS
	PRINT
	PRINT "  "; LG$; " LEAGUE ": PRINT : COLOR 12
	LOCATE , 4: PRINT D1$; TAB(24); D2$; TAB(44); D3$; TAB(64); D4$

230 FOR I = 1 TO 10
	COLOR 2: PRINT USING "## "; I; : COLOR 15: PRINT Z1$(I);
	LOCATE , 21: COLOR 2: PRINT USING "## "; I + 10; : COLOR 15: PRINT Z1$(I + 10);
	LOCATE , 41: COLOR 2: PRINT USING "## "; I + 20; : COLOR 15: PRINT Z1$(I + 20);
	LOCATE , 61: COLOR 2: PRINT USING "## "; I + 30; : COLOR 15: PRINT Z1$(I + 30)
	NEXT I

240 LOCATE 19, 1
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "ADD A TEAM"
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "REMOVE A TEAM"
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "END AND SAVE SETTINGS"
250 I$ = GetKeyPress$

260 IF I$ = "3" THEN 950

270 LOCATE 20, 30: PRINT "SLOT # (1-40) ";

280 N = 1: INPUT B

290 IF B < 1 OR B > 40 THEN 270

300 IF I$ = "2" THEN Z1$(B) = "XXX": GOTO 220

310 GOSUB 1440

390 Z1$(B) = T1$: Z2$(B) = YN$: Z1%(B) = D

400 GOTO 220

950 CLS

970  OPEN D$(2) + LG$ + ".LIB" FOR OUTPUT AS #2
	PRINT #2, LG$: PRINT #2, D1$: PRINT #2, D2$: PRINT #2, D3$: PRINT #2, D4$
	FOR X = 1 TO 40
	PRINT #2, Z1$(X): PRINT #2, Z1%(X): PRINT #2, Z2$(X)
	NEXT
CLOSE 2
	FOR X = 1 TO 40

980 IF Z1$(X) = "XXX" THEN Z1$(X) = CHR$(42): GOTO 1040

990 FOR D = 4 TO 11
	IF MID$(Z1$(X), D, 1) = CHR$(32) OR MID$(Z1$(X), D, 1) = "." THEN MID$(Z1$(X), D, 1) = "_"

1000 NEXT D
	QQ = 8
	FOR D = 11 TO 4 STEP -1
	IF MID$(Z1$(X), D, 1) = "_" THEN QQ = D - 4: GOTO 1020

1010 GOTO 1030

1020 NEXT D

1030 L = LEN(Z1$(X))
	YR$ = LEFT$(Z1$(X), 2)
	TM$ = MID$(Z1$(X), 4, QQ)
	Z1$(X) = TM$ + "." + YR$

1040 NEXT X

1050 Q$ = LG$ + ".TMS"

1060 OPEN D$(2) + Q$ FOR OUTPUT AS #2

1070 WRITE #2, LG$: WRITE #2, D1$: WRITE #2, D2$: WRITE #2, D3$: WRITE #2, D4$

1080 FOR X = 1 TO 40
	WRITE #2, Z1$(X)
	NEXT

1090 CLOSE 2

1195 CLS
	RUN "HELLO.EXE"
	END

1100 NT = 0
	IF NOT _FILEEXISTS(D$(0) + "FCTEAMS." + YN$) THEN 2060
OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0)
	FOR I1 = 1 TO Y / 4342
	GET #1, I1
	FOR I2 = 1 TO 15
	IF LEFT$(Q$(0),2) = "XX"OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
	NEXT I2
	NT = NT + 1
	teamItems$(NT) = Q$(0): V%(NT) = I1

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
MAMenu teamItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN BO% = 1: RETURN
	D = V%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."

1300 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
GET #1, D
T1$ = Q$(0)
T1$ = RTrim$(T1$)
FOR I1 = 1 TO 13: L%(I1) = CVI(Q$(I1)): NEXT
CLOSE 1
	RETURN

1440 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	PRINT " º"; STRING$(61, 32); "º "; : NEXT
	LOCATE 7 + JJ, 8
	PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 27: INPUT NY$
	IF NY$ = "" THEN GOSUB 1102: GOTO 182
	YN$ = NY$
	ERASE teamItems$

182 IF BO% = 1 THEN BO% = 0: GOTO 1440

183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T1$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1440
	RETURN

2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT

2720 BO% = 1: RETURN

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
	'CALL SortStr(VARPTR(F$(1)), Count, 0)

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
MAMenu F$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN 60
	F$ = F$(SELECTION%)
	LOCATE 2, 33: PRINT F$
	
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	COLOR 15, 0
3910 I$ = GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1310

3911 F$ = RTrim$(F$)
	LG$ = LEFT$(F$, LEN(F$) - 4)
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
	GOTO 50


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
