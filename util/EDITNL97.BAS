'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB BATTEREDIT ()
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE SUB PITCHEREDIT ()
DECLARE SUB TITLE ()
DECLARE SUB EDITTEAM (YN$, T$, NN$, S$, M$, T)

DIM D$(3), menuItems$(10)
DIM K9(1) AS LONG
DIM Q$(2100), FS%(22, 4, 4), BP$(3)
DIM B%(22, 79), P%(21, 88), B$(22), P$(21), L%(13)
DIM BS$(22), PS$(21)
DIM B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22)
DIM teamNames$(200), teamIndex%(500), SS%(22, 6)
DIM TB%(15), TP%(33),INJ%(22),DYS%(21)
DIM SA%(24), TS%(11), BRS$(79)
DIM SLG2 AS SINGLE, OB AS SINGLE, R1 AS SINGLE, R2 AS SINGLE
DIM BUNT AS SINGLE, B1 AS SINGLE, RR AS SINGLE
DIM I2 AS SINGLE, ERX AS SINGLE
DIM Y AS LONG

COMMON SHARED B%(), P%(), B$(), P$(), L%(), BRS$(), K9() AS LONG, SA%(), TS%()
COMMON SHARED YN$, T$, NN$, S$, M$, T, N$, INJ%(), DYS%()

FOR I = 0 TO 3
	READ BP$(I)
NEXT
DATA "OUTDOOR","INDOOR ","GRASS","TURF"

OPEN "DEV.BAS" FOR INPUT AS #1
	FOR I = 0 TO 2
	INPUT #1, D$(I)
	NEXT I
CLOSE #1

55 CALL TITLE
	menuItems$(1) = "INPUT A NEW TEAM           "
	menuItems$(2) = "EDIT AN EXISTING TEAM      "
	menuItems$(3) = "PRINT ACTUAL TEAM STATS    "
	menuItems$(4) = "ENTER/EDIT UMPIRES         "
	menuItems$(5) = "SWAP SPOTS ON A TEAM ROSTER"
	menuItems$(6) = "RETURN TO MAIN MENU        "
	menuItems$(7) = "CALCULATE BUNT RATINGS     "
	menuItems$(8) = "CALCULATE RUN RATINGS      "
	menuItems$(9) = "VIEW TEAM ROSTERS/RATINGS  "
	SELECTION% = 1
	Count% = 9
	Start% = 1
	SCANCODE% = 9
	FileFGColor% = 15:FileBGColor% = 1
	HiLiteFGColor% = 0:HiLiteBGColor% = 7
	Spacing% = 1
	Row% = 6
	Column% = 25

CALL box0(5, 24, 15, 52, 2,  15, 1)	
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN SELECTION% = 6
	Q = SELECTION% - 1
	IF Q = 0 THEN 105
	IF Q = 2 OR Q = 10 THEN 77
	IF Q = 1 THEN 77
	IF Q = 3 THEN RUN "UMPMGR.EXE"
	IF Q = 4 THEN 77
	IF Q = 5 THEN 88
	IF Q = 6 THEN 77

77 IF Q = 1 OR Q = 2 OR Q = 4 OR Q = 8 OR Q = 6 OR Q = 7 THEN 1445

88 RUN "HELLO.EXE"

105  IF Q <> 0 THEN 106
	T$ = ""
	T = 0
	YN$=""
	FOR X = 1 TO 13
	L%(X) = 0
	NEXT
	K9(0) = 0
	FOR X = 0 TO 22
	B$(X) = ""
	FOR X1 = 0 TO 41
	B%(X, X1) = 0
	NEXT: NEXT
	FOR X = 0 TO 21
	P$(X) = ""
	FOR X1 = 0 TO 36
	P%(X, X1) = 0
	NEXT: NEXT
	S$ = ""
	M$ = ""
	NN$ = ""
	CK = 0
	FOR X = 0 TO 22
	FOR X1 = 42 TO 79
	B%(X, X1) = 0
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 37 TO 88
	P%(X, X1) = 0
	NEXT: NEXT
	FOR X = 1 TO 24
	SA%(X) = 0
	NEXT
	FOR X = 0 TO 11
	TS%(X) = 0
	NEXT

106 CALL EDITTEAM(YN$, T$, NN$, S$, M$, T)
	CALL BATTEREDIT
	CALL PITCHEREDIT

1385
	L%(1) = L%(1) + 4
	L%(2) = L%(2) - 7
	L%(3) = L%(3) - 1
	L%(4) = L%(4) + 5
	FOR I = 0 TO 3
	SA%(1+I*6) = SA%(1+I*6)+4
	SA%(2+I*6) = SA%(2+I*6)-7
	SA%(3+I*6) = SA%(3+I*6)-1
	SA%(4+I*6) = SA%(4+I*6)+5
	SA%(5+I*6) = SA%(5+I*6)+4
	SA%(6+I*6) = SA%(6+I*6)+4
	NEXT
	FOR I = 0 TO 22
	B%(I, 20) = B%(I, 20) + 4
	FOR I1 = 0 TO 3
	B%(I,56+I1*6) = B%(I,56+I1*6)+4
	B%(I,57+I1*6) = B%(I,57+I1*6)-7
	B%(I,58+I1*6) = B%(I,58+I1*6)-1
	B%(I,59+I1*6) = B%(I,59+I1*6)+5
	B%(I,60+I1*6) = B%(I,60+I1*6)+4
	B%(I,61+I1*6) = B%(I,61+I1*6)+4
	NEXT
	NEXT
	 FOR I = 0 TO 21
	P%(I, 14) = P%(I, 14) + 4
	FOR I1 = 0 TO 3
	P%(I,65+I1*6) = P%(I,65+I1*6)+4
	P%(I,66+I1*6) = P%(I,66+I1*6)-7
	P%(I,67+I1*6) = P%(I,67+I1*6)-1
	P%(I,68+I1*6) = P%(I,68+I1*6)+5
	P%(I,69+I1*6) = P%(I,69+I1*6)+4
	P%(I,70+I1*6) = P%(I,70+I1*6)+4
	NEXT
	NEXT
	LOCATE 10, 8
	
	CALL CSMsgBox("SAVING TEAM FILES", 20, Pass)

OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14)
FOR I = 0 TO 22
FIELD #1, 45 + I * 99 AS X$, 15 AS Q$(I * 43 + 15), 2 AS Q$(I * 43 + 16), 2 AS Q$(I * 43 + 17), 2 AS Q$(I * 43 + 18), 2 AS Q$(I * 43 + 19), 2 AS Q$(I * 43 + 20), 2 AS Q$(I * 43 + 21), 2 AS Q$(I * 43 + 22), 2 AS Q$(I * 43 + 23), 2 AS Q$(I * 43 + 24) _
, 2 AS Q$(I * 43 + 25), 2 AS Q$(I * 43 + 26)
FIELD #1, 82 + I * 99 AS X$, 2 AS Q$(I * 43 + 27), 2 AS Q$(I * 43 + 28), 2 AS Q$(I * 43 + 29), 2 AS Q$(I * 43 + 30), 2 AS Q$(I * 43 + 31), 2 AS Q$(I * 43 + 32), 2 AS Q$(I * 43 + 33), 2 AS Q$(I * 43 + 34), 2 AS Q$(I * 43 + 35), 2 AS Q$(I * 43 + 36),  _

2 AS Q$(I * 43 + 37), 2 AS Q$(I * 43 + 38)
FIELD #1, 106 + I * 99 AS X$, 2 AS Q$(I * 43 + 39), 2 AS Q$(I * 43 + 40), 2 AS Q$(I * 43 + 41), 2 AS Q$(I * 43 + 42), 2 AS Q$(I * 43 + 43), 2 AS Q$(I * 43 + 44), 2 AS Q$(I * 43 + 45), 2 AS Q$(I * 43 + 46), 2 AS Q$(I * 43 + 47), 2 AS Q$(I * 43 + 48) _
, 2 AS Q$(I * 43 + 49), 2 AS Q$(I * 43 + 50)
FIELD #1, 130 + I * 99 AS X$, 2 AS Q$(I * 43 + 51), 2 AS Q$(I * 43 + 52), 2 AS Q$(I * 43 + 53), 2 AS Q$(I * 43 + 54), 2 AS Q$(I * 43 + 55), 2 AS Q$(I * 43 + 56), 2 AS Q$(I * 43 + 57)
NEXT
FOR I = 0 TO 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
FIELD #1, 2407 + I * 89 AS X$, 2 AS Q$(I * 38 + 1040), 2 AS Q$(I * 38 + 1041)
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

1386 SCK = 0
	FOR I = 1 TO 24
	SCK = SCK + SA%(I)
	NEXT
	IF SCK <= 0 THEN 1435
	TS%(0) = 999
	FOR I = 1 TO 11
	TS%(I) = -1
	NEXT
OPEN D$(0) + "LR." + YN$ FOR RANDOM AS #1 LEN = 4108
	FOR X = 0 TO 11
	FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1)
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 1 TO 24
	FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X)
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24)
	NEXT
	NEXT
	FOR X = 0 TO 11: LSET Q$(X + 1) = MKI$(TS%(X)): NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	LSET Q$(12 + X1 + X * 14) = MKI$(B%(X, X1 + 41))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	LSET Q$(334 + X1 + X * 14) = MKI$(P%(X, X1 + 36))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	LSET Q$(642 + X1 + X * 14) = MKI$(P%(X, X1 + 50))
	NEXT: NEXT
	FOR X = 1 TO 24
	LSET Q$(X + 950) = MKI$(SA%(X))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	LSET Q$(975 + X1 + X * 24) = MKI$(B%(X, X1 + 55))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	LSET Q$(1528 + X1 + X * 24) = MKI$(P%(X, X1 + 64))
	NEXT: NEXT
	PUT #1, T: CLOSE 1

1435 OPEN D$(1) + F$ + "D" FOR OUTPUT AS #1
	FOR I1 = 0 TO 22
	PRINT #1, INJ%(I1)
	NEXT
	FOR I1 = 0 TO 21
	PRINT #1, DYS%(I1)
	NEXT

1388 CLOSE 1
	CALL CSMsgBox("", 0, Pass)
	GOTO 55

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
	teamNames$(NT) = Q$(0): V%(NT) = I1

1103 NEXT I1
CLOSE 1

1102 SELECTION% = 1
	Count% = NT
	Start% = 1
	FileFGColor% = 15:FileBGColor% = 4
	HiLiteFGColor% = 0:HiLiteBGColor% = 7
	Rows% = 20
	Columns% = 4
	Spacing% = 6
	Row% = 4
	Column% = 1
	COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
	LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu teamNames$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColor%, FileBGColor%, HiLiteFGColor%, HiLiteBGColor%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

	IF ExitCode% = 27 THEN BO% = 1: RETURN
	T = teamIndex%(SELECTION%)
	LOCATE 2, 33: COLOR 31: PRINT "LOADING..."

OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14)
FOR I = 0 TO 22
FIELD #1, 45 + I * 99 AS X$, 15 AS Q$(I * 43 + 15), 2 AS Q$(I * 43 + 16), 2 AS Q$(I * 43 + 17), 2 AS Q$(I * 43 + 18), 2 AS Q$(I * 43 + 19), 2 AS Q$(I * 43 + 20), 2 AS Q$(I * 43 + 21), 2 AS Q$(I * 43 + 22), 2 AS Q$(I * 43 + 23), 2 AS Q$(I * 43 + 24) _
, 2 AS Q$(I * 43 + 25), 2 AS Q$(I * 43 + 26)
FIELD #1, 82 + I * 99 AS X$, 2 AS Q$(I * 43 + 27), 2 AS Q$(I * 43 + 28), 2 AS Q$(I * 43 + 29), 2 AS Q$(I * 43 + 30), 2 AS Q$(I * 43 + 31), 2 AS Q$(I * 43 + 32), 2 AS Q$(I * 43 + 33), 2 AS Q$(I * 43 + 34), 2 AS Q$(I * 43 + 35), 2 AS Q$(I * 43 + 36),  _

2 AS Q$(I * 43 + 37), 2 AS Q$(I * 43 + 38)
FIELD #1, 106 + I * 99 AS X$, 2 AS Q$(I * 43 + 39), 2 AS Q$(I * 43 + 40), 2 AS Q$(I * 43 + 41), 2 AS Q$(I * 43 + 42), 2 AS Q$(I * 43 + 43), 2 AS Q$(I * 43 + 44), 2 AS Q$(I * 43 + 45), 2 AS Q$(I * 43 + 46), 2 AS Q$(I * 43 + 47), 2 AS Q$(I * 43 + 48) _
, 2 AS Q$(I * 43 + 49), 2 AS Q$(I * 43 + 50)
FIELD #1, 130 + I * 99 AS X$, 2 AS Q$(I * 43 + 51), 2 AS Q$(I * 43 + 52), 2 AS Q$(I * 43 + 53), 2 AS Q$(I * 43 + 54), 2 AS Q$(I * 43 + 55), 2 AS Q$(I * 43 + 56), 2 AS Q$(I * 43 + 57)
NEXT
FOR I = 0 TO 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015)
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027)
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039)
FIELD #1, 2407 + I * 89 AS X$, 2 AS Q$(I * 38 + 1040), 2 AS Q$(I * 38 + 1041)
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
S$ = RTrim$(Q$(1840)): M$ = RTrim$(Q$(1841)): NN$ = RTrim$(Q$(1842))
CK = CVI(Q$(1843))
CLOSE 1

IF NOT _FILEEXISTS(D$(0) + "LR." + YN$) THEN 1200

OPEN D$(0) + "LR." + YN$ FOR RANDOM AS #1 LEN = 4108
	FOR X = 0 TO 11
	FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1)
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14)
	NEXT
	NEXT
	FOR X = 1 TO 24
	FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X)
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24)
	NEXT
	NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24)
	NEXT
	NEXT
	GET #1, T
	FOR X = 0 TO 11
	TS%(X) = CVI(Q$(X + 1))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 14
	B%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 14
	P%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
	NEXT: NEXT
	FOR X = 1 TO 24
	SA%(X) = CVI(Q$(X + 950))
	NEXT
	FOR X = 0 TO 22
	FOR X1 = 1 TO 24
	B%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 1 TO 24
	P%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
	NEXT: NEXT
CLOSE 1

	IF TS%(0) = 999 THEN 1200
	FOR X = 0 TO 22
	FOR X1 = 42 TO 79
	B%(X, X1) = 0
	NEXT: NEXT
	FOR X = 0 TO 21
	FOR X1 = 37 TO 88
	P%(X, X1) = 0
	NEXT: NEXT
	FOR X = 1 TO 24
	SA%(X) = 0
	NEXT
	FOR X = 0 TO 11
	TS%(X) = 0
	NEXT

1200 N$ = T$
	FOR xx = 4 TO 11
	IF MID$(N$, xx, 1) = " " OR MID$(N$, xx, 1) = "." THEN MID$(N$, xx, 1) = "_"
	NEXT xx
	QQ = 8
	FOR xx = 11 TO 4 STEP -1
	IF MID$(N$, xx, 1) = "_" THEN QQ = xx - 4: GOTO 118
	GOTO 119
118 NEXT xx
119 '
	  F$ = MID$(N$, 4, QQ)
	F$ = F$ + "." + LEFT$(N$, 2)
	IF NOT _FILEEXISTS(D$(1) + F$ + "D") THEN 318
	  OPEN D$(1) + F$ + "D" FOR INPUT AS #1
	FOR I1 = 0 TO 22
	INPUT #1, INJ%(I1)
	NEXT
	FOR I1 = 0 TO 21
	INPUT #1, DYS%(I1)
	NEXT

318  CLOSE 1
	RETURN
	
RETURN

1445 COLOR 15, 0: CLS
	JJ = 7
	LOCATE 6, 8: COLOR 15, 4
	Print " " + Chr$(214); String$(61, 196); Chr$(183) + " "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
	Print " " + Chr$(186); String$(61, 32); Chr$(186) + " "; : NEXT
	LOCATE 7 + JJ, 8
	Print " " + Chr$(211); String$(61, 196); Chr$(189) + " "; : LOCATE 7, 32
	LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
	LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
	LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
	LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
	LOCATE 7, 27: INPUT NY$
	IF NY$ = "" THEN GOSUB 1102: GOTO 182
	YN$ = NY$
	ERASE teamNames$

1441 GOSUB 1100

182 IF BO% = 1 THEN BO% = 0: GOTO 1445

183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE? (Y/N)";
	I$GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 1445
	COLOR 15, 0
	IF Q = 2 THEN 1675

1540 IF Q = 4 THEN 3000
	IF Q = 7 THEN 1971
	IF Q = 6 THEN 1920
	IF Q = 8 THEN 80
	GOTO 105

1675 LPRINT CHR$(15): WIDTH "LPT1:", 137: LPRINT "TEAM # "; T

1685 LPRINT "TEAM NAME "; T$; TAB(30); "MANAGER: "; M$; TAB(50); "STADIUM: "; S$

1695 LPRINT : LPRINT "LBA LSO LBB LHR DPG DBL TR    HR   FOUL PARK INFO"

1705 LPRINT USING "### ### ### ### ### ### ### ###### ###  "; L%(1); L%(2); L%(3); L%(4); L%(5); L%(6); L%(7); K9(0); L%(8); : LPRINT BP$(L%(9)); " "; BP$(L%(10) + 2)

1725 LPRINT

1735 LPRINT "BATTER      B    G  AB   R   H DB TR HR RBI  BB  SO  SB CS RN B GA P1 FAV1 A R P2 FAV2 A R P3 FAV3 A R P4 FAV4 A R EBA BAVG"

1755 FOR I = 0 TO 22: IF B$(I) = "" OR B$(I) = "XXX" THEN 1855

1756 LPRINT LEFT$(B$(I), 12); TAB(13); : IF B%(I, 0) = 0 THEN LPRINT "S  ";

1765 IF B%(I, 0) = -1 THEN LPRINT "L  ";

1775 IF B%(I, 0) = 1 THEN LPRINT "R  ";

1785 IF B%(I, 0) = -2 THEN LPRINT "LP ";

1795 IF B%(I, 0) = 2 THEN LPRINT "RP ";

1796 LPRINT USING "### ### ### ### ## ## ## ### ### ### ### ##  "; B%(I, 3); B%(I, 4); B%(I, 5); B%(I, 6); B%(I, 7); B%(I, 8); B%(I, 9); B%(I, 10); B%(I, 11); B%(I, 12); B%(I, 13); B%(I, 14);

1797 LPRINT USING "# "; B%(I, 16); : LPRINT USING "# "; B%(I, 30);

1798 LPRINT USING "## ## #### # # "; B%(I, 18); B%(I, 22); B%(I, 26); B%(I, 33); B%(I, 34);
	IF B%(I, 23) < 0 THEN 1845
	LPRINT USING "## #### # # "; B%(I, 23); B%(I, 27); B%(I, 35); B%(I, 36);
	IF B%(I, 24) < 0 THEN 1845
	LPRINT USING "## #### # # "; B%(I, 24); B%(I, 28); B%(I, 37); B%(I, 38);
	IF B%(I, 25) < 0 THEN 1845
	LPRINT USING "## #### # # "; B%(I, 25); B%(I, 29); B%(I, 39); B%(I, 40);

1845 LPRINT TAB(116); USING "### "; B%(I, 20);

1846 IF B%(I, 4) = 0 THEN LPRINT " 000": GOTO 1855

1847 Z% = B%(I, 6) / B%(I, 4) * 1000: LPRINT USING " ###"; B%(I, 6) / B%(I, 4) * 1000

1855 NEXT I
	FOR I = 0 TO 21: IF P$(I) = "" OR P$(I) = "XXX" THEN 1862

1856 LPRINT LEFT$(P$(I), 12); TAB(13); : IF P%(I, 0) = 1 THEN LPRINT "R  ";

1857 IF P%(I, 0) = -1 THEN LPRINT "L  ";

1858 LPRINT USING "### ### ### ### ## ## ## ### ### ### ### ## "; P%(I, 19); P%(I, 20); P%(I, 21); P%(I, 22); P%(I, 23); P%(I, 24); P%(I, 25); P%(I, 26); P%(I, 27); P%(I, 28); P%(I, 29); P%(I, 30);

1859 LPRINT USING " # # 52  1 ####                                         ### "; P%(I, 31); P%(I, 32); P%(I, 17); P%(I, 14);

1860 IF P%(I, 20) = 0 THEN LPRINT " 000": GOTO 1862

1861 Z% = P%(I, 22) / P%(I, 20) * 1000: LPRINT USING " ###"; P%(I, 22) / P%(I, 20) * 1000

1862 NEXT I: LPRINT

1865 LPRINT "PITCHER     T  W  L  SV G  GS CG IP  H   BB  SO  ERA  GA HRA HOLD EBA"

1885 FOR I = 0 TO 21: IF P$(I) = "" OR P$(I) = "XXX" THEN 1915

1895 LPRINT MID$(P$(I), 1, 12); TAB(13); : IF P%(I, 0) = 1 THEN LPRINT "R  ";

1905 IF P%(I, 0) = -1 THEN LPRINT "L  ";

1910 LPRINT USING "## ## ## ## ## ## ### ### ### ### #.## ## ##   #   ###"; P%(I, 1); P%(I, 2); P%(I, 3); P%(I, 4); P%(I, 5); P%(I, 16); P%(I, 6); P%(I, 7); P%(I, 8); P%(I, 9); P%(I, 10) / 100; P%(I, 12); P%(I, 13); P%(I, 18); P%(I, 14)

1915 NEXT I: LPRINT CHR$(18): LPRINT CHR$(12): GOTO 55

1920 '*** BUNT RATING CALCS ***
	CLS
	FOR I = 0 TO 22
	IF B$(I) = "" OR B$(I) = "XXX" THEN 1950
	PA = B%(I, 11) + B%(I, 4)
	IF PA = 0 THEN PA = 1
	OB = (B%(I, 6) * 100 + PA / 2) / PA
	SLG = (B%(I, 6) + B%(I, 7) + B%(I, 8) * 2 + B%(I, 9) * 3) * 100
	SLG2 = (SLG + PA / 2) / PA
	R1 = (B%(I, 12) * 100 + PA / 2) / PA
	R1 = 11 - R1 ^ 2 / 25
	R2 = 11 - (SLG2 - OB) / 25
	BUNT = (R1 + R1 + R2) / 3
	B1 = B%(I, 15) * 1000 / PA
	IF B1 <= 0 THEN BR = 1: GOTO 1921
	IF B1 <= 1 THEN BR = 2: GOTO 1921
	IF B1 <= 3 THEN BR = 3: GOTO 1921
	IF B1 <= 6 THEN BR = 4: GOTO 1921
	IF B1 <= 10 THEN BR = 5: GOTO 1921
	IF B1 <= 15 THEN BR = 6: GOTO 1921
	IF B1 <= 20 THEN BR = 7: GOTO 1921
	IF B1 <= 25 THEN BR = 8: GOTO 1921
	IF B1 <= 30 THEN BR = 9: GOTO 1921
	IF B1 <= 35 THEN BR = 10: GOTO 1921
	BR = 10

1921 BUNT = (BUNT + BR) / 2
	IF BUNT < 1 THEN BUNT = 1
	IF BUNT > 9 THEN BUNT = 9
	B%(I, 30) = INT(BUNT + .5)
	PRINT B$(I); TAB(20); B%(I, 30)

1950 NEXT

1955 CALL GETKEY
	CLS
	FOR I = 0 TO 21
	IF P$(I) = "" OR P$(I) = "XXX" THEN 1970
	PA = P%(I, 20) + P%(I, 27)
	IF PA = 0 THEN PA = 1
	OB = (P%(I, 22) * 100 + PA / 2) / PA
	SLG = (P%(I, 22) + P%(I, 23) + P%(I, 24) * 2 + P%(I, 25) * 3) * 100
	SLG2 = (SLG + PA / 2) / PA
	R1 = (P%(I, 28) * 100 + PA / 2) / PA
	R1 = 11 - R1 ^ 2 / 25
	IF R1 < 1 THEN R1 = 1
	IF R1 > 10 THEN R1 = 10
	R2 = 11 - (SLG2 - OB) / 25
	IF R2 < 1 THEN R2 = 1
	IF R2 > 10 THEN R2 = 10
	BUNT = (R1 + R1 + R2) / 3
	B1 = P%(I, 36) * 1000 / PA
	IF B1 <= 0 THEN BR = 1: GOTO 1961
	IF B1 <= 1 THEN BR = 2: GOTO 1961
	IF B1 <= 3 THEN BR = 3: GOTO 1961
	IF B1 <= 6 THEN BR = 4: GOTO 1961
	IF B1 <= 10 THEN BR = 5: GOTO 1961
	IF B1 <= 15 THEN BR = 6: GOTO 1961
	IF B1 <= 20 THEN BR = 7: GOTO 1961
	IF B1 <= 25 THEN BR = 8: GOTO 1961
	IF B1 <= 30 THEN BR = 9: GOTO 1961
	IF B1 <= 35 THEN BR = 10: GOTO 1961
	BR = 10

1961 BUNT = (BUNT + BR) / 2
	IF BUNT < 1 THEN BUNT = 1
	IF BUNT > 9 THEN BUNT = 9
	P%(I, 32) = INT(BUNT + .5)
	PRINT P$(I); TAB(20); P%(I, 32)

1970 NEXT
	CALL GETKEY
	GOTO 1385

1971 'RUN RATING
	CLS
	FOR I = 0 TO 22
	IF B$(I) = "" OR B$(I) = "XXX" THEN 1975
	SPD = 200 * B%(I, 8) + B%(I, 13) * 100
	IF B%(I, 6) + B%(I, 11) - B%(I, 7) - B%(I, 8) - B%(I, 9) = 0 THEN RR = INT(RND(1) * 15) + 1: GOTO 1972
	RR = INT(SPD / (B%(I, 6) + B%(I, 11) - B%(I, 7) - B%(I, 8) - B%(I, 9)) + .5)
	RR = RR * 1.4

1972 IF RR <= 3 THEN B%(I, 16) = 1
	IF RR > 3 AND RR <= 5 THEN B%(I, 16) = 2
	IF RR > 5 AND RR <= 10 THEN B%(I, 16) = 3
	IF RR > 10 AND RR <= 15 THEN B%(I, 16) = 4
	IF RR > 15 AND RR <= 20 THEN B%(I, 16) = 5
	IF RR > 20 AND RR <= 25 THEN B%(I, 16) = 6
	IF RR > 25 AND RR <= 30 THEN B%(I, 16) = 7
	IF RR > 30 AND RR <= 40 THEN B%(I, 16) = 8
	IF RR > 40 THEN B%(I, 16) = 9
	PRINT B$(I); TAB(20); B%(I, 16)

1975 NEXT
	CALL GETKEY
	FOR I = 0 TO 21
	CLS
	IF P$(I) = "" OR P$(I) = "XXX" THEN 1985
	RR = INT(RND(1) * 20) + 1
	RR = RR * 1.4

1982 IF RR <= 4 THEN P%(I, 31) = 1
	IF RR > 4 AND RR <= 8 THEN P%(I, 31) = 2
	IF RR > 8 AND RR <= 12 THEN P%(I, 31) = 3
	IF RR > 12 AND RR <= 16 THEN P%(I, 31) = 4
	IF RR > 16 THEN P%(I, 31) = 5
	PRINT P$(I); TAB(20); P%(I, 31)

1985 NEXT
	CALL GETKEY
	GOTO 1385

3000 TS = 0
	CLS
	PRINT "DOES "; T$; " HAVE COMPILED STATS (YN)";
3001 I$ = GetKeyPress$

3005 IF UCASE$(I$) = "Y" THEN TS = 1

3010 IF TS = 1 THEN GOSUB 4160

3055 CLS
	PRINT "TEAM ROSTER FOR "; T$

3060 FOR I = 0 TO 22
	LOCATE I + 3, 1
	PRINT USING "## "; I + 1; : PRINT B$(I);
	NEXT

3061 FOR I = 0 TO 21
	LOCATE I + 3, 25
	PRINT USING "## "; I + 1;
	PRINT P$(I);
	NEXT

3065 LOCATE 10, 50: PRINT "1 - SWITCH BATTERS"

3066 LOCATE 11, 50: PRINT "2 - SWITCH PITCHERS"

3067 LOCATE 12, 50: PRINT "3 - SAVE CHANGES"

3068 LOCATE 13, 50: PRINT "4 - RETURN TO MENU (NO CHANGES)"
3070 I$ = GetKeyPress$

3071 I = VAL(I$): IF I < 1 OR I > 4 THEN 3070

3075 IF I = 4 THEN 55

3080 IF I = 3 THEN GOSUB 4260: GOTO 1385

3090 IF I = 1 THEN GOSUB 3100: GOTO 3055

3095 GOSUB 3200: GOTO 3055

3100 GOSUB 3300

3105 LOCATE 16, 50: INPUT "BATTER #1 "; I1

3106 IF I1 < 1 OR I1 > 23 THEN 3105

3107 LOCATE 16, 60: PRINT B$(I1 - 1)

3108 LOCATE 17, 50: INPUT "BATTER #2 "; I2

3109 IF I2 < 1 OR I2 > 23 THEN 3108

3110 LOCATE 17, 60: PRINT B$(I2 - 1)

3115 LOCATE 18, 50: PRINT "ANY CHANGE? (Y/N)";
3116 I$ = GetKeyPress$

3117 IF UCASE$(I$) = "Y" THEN 3100

3120 SWAP B$(I1 - 1), B$(I2 - 1): SWAP BS$(I1 - 1), BS$(I2 - 1)

3121 FOR I = 0 TO 79: SWAP B%(I1 - 1, I), B%(I2 - 1, I): NEXT

3122 FOR I = 0 TO 21: SWAP B0%(I1 - 1, I), B0%(I2 - 1, I): NEXT
	FOR I = 0 TO 6: SWAP SS%(I1 - 1, I), SS%(I2 - 1, I): NEXT

3124 SWAP BS%(I1 - 1), BS%(I2 - 1)

3125 RETURN

3200 GOSUB 3300

3205 LOCATE 16, 50: INPUT "PITCHER #1 "; I1

3206 IF I1 < 1 OR I1 > 22 THEN 3205

3207 LOCATE 16, 60: PRINT P$(I1 - 1)

3208 LOCATE 17, 50: INPUT "PITCHER #2 "; I2

3209 IF I2 < 1 OR I2 > 22 THEN 3208

3210 LOCATE 17, 60: PRINT P$(I2 - 1)

3215 LOCATE 18, 50: PRINT "ANY CHANGE? (Y/N)";
3216 I$ = GetKeyPress$

3217 IF UCASE$(I$) = "Y" THEN 3200

3220 SWAP P$(I1 - 1), P$(I2 - 1): SWAP PS$(I1 - 1), PS$(I2 - 1)

3221 FOR I = 0 TO 88: SWAP P%(I1 - 1, I), P%(I2 - 1, I): NEXT

3222 FOR I = 0 TO 41: SWAP P0%(I1 - 1, I), P0%(I2 - 1, I): NEXT

3224 SWAP PS%(I1 - 1), PS%(I2 - 1)

3225 RETURN

3300 FOR I = 16 TO 19: LOCATE I, 50: PRINT "                           ": NEXT: RETURN

4005 FOR XX = 4 TO 11: IF MID$(ST$, XX, 1) = " " OR MID$(ST$, XX, 1) = "." THEN MID$(ST$, XX, 1) = "_"

4010 NEXT

4015 QQ = 8: FOR XX = 11 TO 4 STEP -1: IF MID$(ST$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 4025

4020 GOTO 4030

4025 NEXT XX

4030 RETURN

4065 PRINT "                                        "; : RETURN

4160 ST$ = T$
	CLS

4162 GOSUB 4005

4165 F$ = MID$(ST$, 4, QQ)
	R$ = F$ + "." + LEFT$(ST$, 2)

4170 IF NOT _FILEEXISTS(D$(1) + R$) THEN GOSUB 4550: TS = 0: GOTO 4210
OPEN D$(1) + R$ FOR INPUT AS #1

4171 INPUT #1, A$: INPUT #1, W2%: INPUT #1, L2%

4175 FOR I = 0 TO 22: INPUT #1, BS$(I): INPUT #1, BS%(I): NEXT I

4180 FOR I = 0 TO 21: INPUT #1, PS$(I): INPUT #1, PS%(I): NEXT I

4185 FOR I = 0 TO 22: FOR I1 = 0 TO 21: INPUT #1, B0%(I, I1): NEXT I1: NEXT I

4190 FOR I = 0 TO 21: FOR I1 = 0 TO 41: INPUT #1, P0%(I, I1): NEXT I1: NEXT I

4195 INPUT #1, D0%: INPUT #1, D1%
	FOR I = 0 TO 21: INPUT #1, T(I): NEXT I
	FOR I = 0 TO 22: INPUT #1, T1(I): NEXT I
CLOSE #1
	IF NOT _FILEEXISTS(D$(1) + R$ + "R") THEN SS = 0: GOTO 4210
	SS = 1
OPEN D$(1) + R$ + "R" FOR INPUT AS #1
	FOR I = 0 TO 22
	FOR I1 = 0 TO 6
	INPUT #1, SS%(I, I1)
	NEXT I1: NEXT I
CLOSE #1

4210 RETURN

4260 IF TS = 0 THEN 4297

4265 CLS

4270 OPEN D$(1) + R$ FOR OUTPUT AS #1

4271 PRINT #1, A$: PRINT #1, W2%: PRINT #1, L2%

4275 FOR I = 0 TO 22: PRINT #1, BS$(I): PRINT #1, BS%(I): NEXT I

4280 FOR I = 0 TO 21: PRINT #1, PS$(I): PRINT #1, PS%(I): NEXT I

4285 FOR I = 0 TO 22: FOR I1 = 0 TO 21: PRINT #1, B0%(I, I1): NEXT I1: NEXT I

4290 FOR I = 0 TO 21: FOR I1 = 0 TO 41: PRINT #1, P0%(I, I1): NEXT I1: NEXT I

4295 PRINT #1, D0%: PRINT #1, D1%
	FOR I = 0 TO 21: PRINT #1, T(I): NEXT I
	FOR I = 0 TO 22: PRINT #1, T1(I): NEXT I

4296 CLOSE 1
	  IF SS = 0 THEN 4297
OPEN D$(1) + R$ + "R" FOR OUTPUT AS #1
	FOR I = 0 TO 22
	FOR I1 = 0 TO 6
	PRINT #1, SS%(I, I1)
	NEXT I1: NEXT I
CLOSE #1

4297 RETURN

80 GOSUB 5000: GOTO 500

81 CLS : COLOR 12: PRINT "TEAM OPTIONS": PRINT
	COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "VIEW HITTERS": PRINT
	COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "VIEW PITCHERS": PRINT
	COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "VIEW PITCHERS HITTING/FLDG STATS": PRINT
	COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "RETURN TO MAIN MENU"
82 I$ = GetKeyPress$
	I = VAL(I$): IF I < 1 OR I > 4 THEN 82
	IF I = 1 THEN 83
	IF I = 2 THEN 130
	IF I = 3 THEN 700
	IF I = 4 THEN 55

83 CLS : COLOR 14: PRINT "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

96 FOR I = 0 TO 22
	IF I = 20 THEN GOSUB 5050 ELSE 97

97 COLOR 3
	IF B$(I) = "XXX" THEN 120
	PRINT LEFT$(B$(I), 10);
	COLOR 7
	IF B%(I, 0) = -1 THEN PRINT TAB(12); "L";
	IF B%(I, 0) = 0 THEN PRINT TAB(12); "S";
	IF B%(I, 0) = 1 THEN PRINT TAB(12); "R";
	IF B%(I, 0) = -2 THEN PRINT TAB(12); "LP";
	IF B%(I, 0) = 2 THEN PRINT TAB(12); "RP";
	PRINT TAB(14); USING "#"; B%(I, 22); : IF B%(I, 23) <> -1 THEN PRINT USING "#"; B%(I, 23); : IF B%(I, 24) <> -1 THEN PRINT USING "#"; B%(I, 24); : IF B%(I, 25) <> -1 THEN PRINT USING "#"; B%(I, 25);
	PRINT TAB(20); USING "###"; B%(I, 3);
	PRINT TAB(24); USING "###"; B%(I, 4);
	PRINT TAB(28); USING "###"; B%(I, 5);
	PRINT TAB(32); USING "###"; B%(I, 6);
	PRINT TAB(36); USING "##"; B%(I, 7);
	PRINT TAB(39); USING "##"; B%(I, 8);
	PRINT TAB(42); USING "##"; B%(I, 9);
	PRINT TAB(45); USING "###"; B%(I, 10);
	PRINT TAB(49); USING "###"; B%(I, 11);
	PRINT TAB(53); USING "###"; B%(I, 12);
	PRINT TAB(57); USING "###"; B%(I, 13);
	PRINT TAB(61); USING "##"; B%(I, 14);
	PRINT TAB(64); USING "##"; B%(I, 18);
	I2 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000)

117 IF B%(I, 4) = 0 OR I2 = 0 THEN PRINT TAB(68); ".000"
	IF I2 > 0 AND I2 < 100 THEN PRINT TAB(68); ".0"; : PRINT TAB(70); USING "##"; I2
	IF I2 > 100 THEN PRINT TAB(68); "."; : PRINT TAB(69); USING "###"; I2

120 NEXT: GOSUB 4000

121 COLOR 11: LOCATE 20: PRINT "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
122 I$ = GetKeyPress$

123 IF I$ = CHR$(9) THEN 600

124 IF I$ <> CHR$(9) THEN 81

130 CLS : COLOR 12: PRINT T$
	COLOR 14: PRINT "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"
	app = 0

133 FOR I = 0 TO 21
	IF I = 18 THEN GOSUB 5025 ELSE 135

135 COLOR 3
	IF P$(I) = "XXX" THEN 156
	app = app + p%(i,4)
	PRINT LEFT$(P$(I), 10);

136 COLOR 7
	IF P%(I, 0) = -1 THEN PRINT TAB(14); "L";
	IF P%(I, 0) = 1 THEN PRINT TAB(14); "R";

137 PRINT TAB(16); USING "##"; P%(I, 1);

138 PRINT TAB(19); USING "##"; P%(I, 2);

139 PRINT TAB(22); USING "##"; P%(I, 3);

140 PRINT TAB(25); USING "##"; P%(I, 4);

141 PRINT TAB(28); USING "##"; P%(I, 5);

142 PRINT TAB(31); USING "###"; P%(I, 6);

143 PRINT TAB(35); USING "###"; P%(I, 7);

144 PRINT TAB(39); USING "###"; P%(I, 8);
	PRINT TAB(43); USING "###"; P%(I, 9);

146 PRINT TAB(46); USING "##.##"; P%(I, 10) / 100;

148 PRINT TAB(53); USING "##"; P%(I, 12);

149 PRINT TAB(56); USING "##"; P%(I, 13);

150 PRINT TAB(59); USING "##"; P%(I, 16);

151 PRINT TAB(62); USING "##"; P%(I, 33);

152 PRINT TAB(66); USING "##"; P%(I, 18)

156 NEXT: GOSUB 5020

157 COLOR 11: LOCATE 24: PRINT "HIT ANY KEY TO CONTINUE";
158 I$ = GetKeyPress$

159 GOTO 81

500 CLS

501 COLOR 12: LOCATE 5, 10: PRINT T$

502 COLOR 3: LOCATE 7, 10: PRINT "LEAGUE BATTING AVG."; : LOCATE 7, 38: COLOR 7: PRINT USING "#####"; L%(1)

503 COLOR 3: LOCATE , 10: PRINT "LEAGUE STRIKE OUT AVG."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(2)

504 COLOR 3: LOCATE , 10: PRINT "LEAGUE BASE ON BALLS AVG."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(3)
	COLOR 3: LOCATE , 10: PRINT "LEAGUE HOME RUN AVG."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(4)

506 COLOR 3: LOCATE , 10: PRINT "DOUBLE PLAYS AVG."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(5)

507 COLOR 3: LOCATE , 10: PRINT "PARK DOUBLE ADJ."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(6)

508 COLOR 3: LOCATE , 10: PRINT "PARK TRIPLE ADJ."; : LOCATE , 38: COLOR 7: PRINT USING "#####"; L%(7)

509 COLOR 3: LOCATE , 10: PRINT "PARK HOME RUN ADJ."; : LOCATE , 37: COLOR 7: PRINT USING "######"; K9(0)

510 COLOR 3: LOCATE , 10: PRINT "PARK FOUL GROUND ADJ."; : LOCATE , 37: COLOR 7: PRINT USING "######"; L%(8)

520 COLOR 15: LOCATE 23, 10: PRINT "HIT ANY KEY TO CONTINUE"
521 I$ = GetKeyPress$

522 GOTO 81

600 CLS : COLOR 14: PRINT "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

602 FOR I = 0 TO 22
	IF I = 20 THEN GOSUB 5010 ELSE 603

603 COLOR 3: PRINT LEFT$(B$(I), 10);
	COLOR 7
	PRINT TAB(13); USING "####"; B%(I, 26);
	PRINT TAB(19); USING "#"; B%(I, 33);

606 PRINT TAB(23); USING "#"; B%(I, 34);

607 PRINT TAB(26); USING "####"; B%(I, 27);

608 PRINT TAB(32); USING "#"; B%(I, 35);

609 PRINT TAB(36); USING "#"; B%(I, 36);

610 PRINT TAB(39); USING "####"; B%(I, 28);

611 PRINT TAB(45); USING "#"; B%(I, 37);

612 PRINT TAB(49); USING "#"; B%(I, 38);

613 PRINT TAB(52); USING "####"; B%(I, 29);

614 PRINT TAB(58); USING "#"; B%(I, 39);
	PRINT TAB(62); USING "#"; B%(I, 40);

616 PRINT TAB(66); USING "#"; B%(I, 16);

617 PRINT TAB(71); USING "#"; B%(I, 30)

618 NEXT

621 COLOR 11: LOCATE 20: PRINT "HIT TAB KEY FOR FIELDING STATS/ANY KEY TO CONTINUE"
622 I$ = GetKeyPress$

623 IF I$ = CHR$(9) THEN 83

624 IF I$ <> CHR$(9) THEN 81

700 CLS : COLOR 14: PRINT "PITCHER        G  AB   R   H DB TR HR RBI  BB  SO  SB CS BAVG  R B FAVG"

701 FOR I = 0 TO 21

702 COLOR 3: PRINT LEFT$(P$(I), 10);
	COLOR 7: PRINT TAB(14); USING "###"; P%(I, 19);
	PRINT TAB(18); USING "###"; P%(I, 20);
	PRINT TAB(22); USING "###"; P%(I, 21);
	PRINT TAB(26); USING "###"; P%(I, 22);

703 PRINT TAB(30); USING "##"; P%(I, 23);

704 PRINT TAB(33); USING "##"; P%(I, 24);
	PRINT TAB(36); USING "##"; P%(I, 25);
	PRINT TAB(39); USING "###"; P%(I, 26);

707 PRINT TAB(43); USING "###"; P%(I, 27);

708 PRINT TAB(47); USING "###"; P%(I, 28);

709 PRINT TAB(52); USING "##"; P%(I, 29);

710 PRINT TAB(55); USING "##"; P%(I, 30);

714 IF P%(I, 11) <> 999 THEN PRINT TAB(59); USING ".###"; P%(I, 11) / 1000; : GOTO 715
	I2 = 0: IF P%(I, 20) > 0 THEN I2 = INT((P%(I, 22) / P%(I, 20) + .0005) * 1000)
	IF P%(I, 20) = 0 OR I2 = 0 THEN PRINT TAB(59); ".000";
	IF I2 > 0 AND I2 < 100 THEN PRINT TAB(59); ".0"; : PRINT TAB(61); USING "##"; I2;
	IF I2 > 100 THEN PRINT TAB(59); "."; : PRINT TAB(60); USING "###"; I2;

715 PRINT TAB(64); USING "#"; P%(I, 31);

716 PRINT TAB(66); USING "#"; P%(I, 32);

717 PRINT TAB(68); USING "####"; P%(I, 17)

720 NEXT

721 COLOR 11: LOCATE 24: PRINT "HIT ANY KEY TO CONTINUE";
722 I$ = GetKeyPress$

723 GOTO 81

5000 FOR K=0 TO 15
	TB%(K)=0
	NEXT
	FOR K=0 TO 33
	TP%(K)=0
	NEXT
	FOR K = 4 TO 18: FOR I = 0 TO 22
	IF B$(I) = "XXX" THEN 3002
	TB%(K - 3) = TB%(K - 3) + B%(I, K)

3002 NEXT: NEXT
	FOR K = 4 TO 18: FOR I = 0 TO 21
	IF P$(I) = "XXX" OR P%(I,11)<>999 THEN 2005
	TB%(K - 3) = TB%(K - 3) + P%(I, K + 16)

2005 NEXT: NEXT

3003 FOR K = 1 TO 33: FOR I = 0 TO 21
	IF P$(I) = "XXX" THEN 3006

3004 TP%(K) = TP%(K) + P%(I, K)

3006 NEXT: NEXT
	ERX = 0: FOR I = 0 TO 21
	IF P$(I) = "XXX" THEN 3012

3011 ERX = ERX + ((P%(I, 6) / 9) * (P%(I, 10) / 100))

3012 NEXT

3025 RETURN

4000 PRINT : COLOR 14: PRINT "TOTALS"; : LOCATE , 23: PRINT USING "####    ####   ###    ###    ####    ###     .###"; TB%(1); TB%(3); TB%(5); TB%(7); TB%(9); TB%(11); TB%(3) / TB%(1)

4001 LOCATE , 27: PRINT USING "####   ####   ###     ###     ###"; TB%(2); TB%(4); TB%(6); TB%(8); TB%(10)
	RETURN

5020 PRINT : COLOR 14: PRINT "TOTALS"; : LOCATE , 14: PRINT USING "####  ####      ####    ####     #.##    ###   ###"; TP%(1); TP%(3); TP%(6); TP%(8); (ERX * 9) / TP%(6); TP%(13); TP%(33)

4021 LOCATE , 17: PRINT USING "####      ###    ####    ####            ###"; TP%(2); TP%(5); TP%(7); TP%(9); TP%(16)
	print "TOTAL APPEARANCES: "APP

4022 RETURN

5050 LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
5001 I$ = GetKeyPress$

5002 CLS : COLOR 12: PRINT T$
	COLOR 14: PRINT "BATTER     B POS     G  AB   R   H DB TR HR RBI  BB  SO  SB CS GA  BAVG"

5003 RETURN

5010 LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
5011 I$ = GetKeyPress$

5012 CLS : COLOR 12: PRINT T$
	COLOR 14: PRINT "BATTER       AVG ARM RGE  AVG ARM RGE  AVG ARM RGE  AVG ARM RGE RUN BUNT"

5013 RETURN

5025 LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
5021 I$ = GetKeyPress$
	CLS : COLOR 12: PRINT T$
	COLOR 14: PRINT "PITCHER      T  W  L SV  G GS  IP   H  BB  SO  ERA  GA HR CG WP HLDR"

5023 RETURN

6000 FOR I = 1 TO 15: TB%(I) = 0: NEXT

6001 FOR I = 1 TO 33: TP%(I) = 0: NEXT

6002 RETURN

4500 LOCATE 10, 8

	CALL CSMsgBox("NO TEAMS MATCHING ID WERE FOUND... PRESS ANY KEY TO SELECT ANOTHER ID", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	RETURN

4550 LOCATE 10, 8

	CALL CSMsgBox("NO COMPILED STATS WERE FOUND... PRESS ANY KEY TO RETURN TO CONTINUE", 26, Pass)
	I$ = GetKeyPress$
	CALL CSMsgBox("", 0, Pass)
	RETURN



'----------------------------------------
'      PadRight Function
'----------------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function
FUNCTION PadRight$ (X$, Length) 
'Essentially this pads space to the right of the string
	PadRight$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


'----------------------------------------
'      TITLE Subroutine
'----------------------------------------
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
