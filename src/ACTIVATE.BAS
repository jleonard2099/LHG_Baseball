'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Activate Players"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon	
_AllowFullScreen

DEFINT A-Z
DECLARE SUB TITLE ()

DIM K9(1) AS LONG, D$(2), Q$(1844), FS%(22, 4, 4)
DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21)
DIM B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21)
DIM teamItems$(200), V%(200), Y AS LONG

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR I = 0 TO 2: INPUT #1, D$(I): NEXT I
CLOSE #1

55 CLS
	PRINT TAB(27); : COLOR 14, 4: PRINT "TEAM MANAGEMENT AND INPUT": PRINT : PRINT : COLOR , 0
	COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "SELECT TEAM TO ACTIVATE/DEACTIVATE PLAYERS FOR": PRINT
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "RETURN TO MAIN MENU": PRINT
65 I$ = GetKeyPress$
	Q = VAL(I$): IF Q < 0 OR Q > 1 THEN 65
	IF Q = 1 THEN 86

77 GOTO 1445

CLS

88 RUN "HELLO.EXE"

835 CLS
	PRINT "CHANGE BATTERS"
	X% = 2

840 FOR I = 0 TO 10: COLOR 15, 0
	LOCATE I + 5, 5: PRINT USING "## "; I + 1;
	IF B%(I, 31) <> 0 THEN COLOR L%(11), L%(12)
	PRINT B$(I); : COLOR 15, 0
	PRINT TAB(40); : PRINT USING "## "; I + 13;
	IF B%(I + 12, 31) <> 0 THEN COLOR L%(11), L%(12)
	PRINT B$(I + 12): COLOR 15, 0
	NEXT
	LOCATE , 5: PRINT USING "## "; 12;
	IF B%(11, 31) <> 0 THEN COLOR L%(11), L%(12)
	PRINT B$(11): COLOR 15, 0
	PRINT

855 PRINT "# TO CHANGE (1-23) (0 TO CONTINUE) "; : INPUT I

865 I = I - 1

875 IF I < 0 OR I > 22 THEN X% = 0: GOTO 1155
	B%(I, 31) = 1 - B%(I, 31): GOTO 840

1155 CLS
	PRINT "CHANGE PITCHERS": X% = 2

1160 FOR I = 0 TO 10
	LOCATE I + 5, 5: PRINT USING "## "; I + 1;
	IF P%(I, 34) <> 0 THEN COLOR L%(11), L%(12)
	PRINT P$(I); : COLOR 15, 0
	PRINT TAB(40); : PRINT USING "## "; I + 12;
	IF P%(I + 11, 34) <> 0 THEN COLOR L%(11), L%(12)
	PRINT P$(I + 11): COLOR 15, 0
	NEXT
	PRINT

1175 PRINT
	PRINT "# TO CHANGE (1-22) (0 TO CONTINUE) "; : INPUT I

1185 I = I - 1

1195 IF I < 0 OR I > 21 THEN X% = 0: GOTO 1385
	P%(I, 34) = 1 - P%(I, 34)
	GOTO 1160

1385 CLS
OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
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
LSET Q$(0) = T$: FOR I = 1 TO 13: LSET Q$(I) = MKI$(L%(I)): NEXT: LSET Q$(14) = MKS$(K9(0))
FOR I = 0 TO 22: LSET Q$(I * 43 + 15) = B$(I)
FOR I1 = 0 TO 41: LSET Q$(I * 43 + I1 + 16) = MKI$(B%(I, I1))
NEXT: NEXT
FOR I = 0 TO 21: LSET Q$(I * 38 + 1004) = P$(I)
FOR I1 = 0 TO 36: LSET Q$(I * 38 + I1 + 1005) = MKI$(P%(I, I1))
NEXT: NEXT
LSET Q$(1840) = S$: LSET Q$(1841) = M$: LSET Q$(1842) = NN$: LSET Q$(1843) = MKI$(999)
PUT #1, T: CLOSE 1

1435 GOTO 55

1445 COLOR 15, 0: CLS
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
	FOR I = 1 TO 200: teamItems$(I) = "": NEXT

1441 GOSUB 1100

182 IF BO% = 1 THEN BO% = 0: GOTO 1445

183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1445
	COLOR 15, 0
	GOTO 835

1100 NT = 0
	IF NOT _FILEEXISTS(D$(0) + "FCTEAMS." + YN$) THEN GOSUB 4500: BO% = 1: RETURN
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
	T = V%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
	 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
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
GET #1, T
T$ = RTrim$(Q$(0))
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = RTrim$(Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = RTrim$(Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = RTrim$(Q$(1840)): M$ = RTrim$(Q$(1841)): NN$ = RTrim$(Q$(1842)): CK = CVI(Q$(1843))
CLOSE 1

RETURN

4500 LOCATE 10, 8
	CALL CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	RETURN

4550 LOCATE 10, 8
	CALL CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	RETURN


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
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
