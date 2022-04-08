'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Groundball Calculations"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon	
_AllowFullScreen

DEFINT A-Z
DECLARE SUB TITLE ()

DIM K9(1) AS LONG, D$(2)
DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), Q$(1843), RH%(21)
DIM SP%(29), TB%(29), BA%(29), DP%(29)
DIM teamItems$(200),V%(200)
DIM Y AS LONG
DIM PD AS SINGLE, SLP AS SINGLE, BAP AS SINGLE
DIM CH AS SINGLE, RHP AS SINGLE
OPEN "DEV.BAS" FOR INPUT AS #2
FOR I = 0 TO 2
INPUT #2, D$(I)
NEXT
CLOSE 2

500 COLOR 15, 1
	 FG = 0
	 BG = 7
	COLR = OneColor%(FG, BG)             'pack FG and BG into a single byte
	CLS : LOCATE , 16: PRINT "SPECIFY LEAGUE STATS": PRINT
	LOCATE 7, 1: QPrint "League Batting Average ", 30, -1
	LOCATE , 24: QPrint FUsing(STR$(BP), "######"), COLR, -1
	LOCATE 9, 1: QPrint "League Slugging Percentage ", 30, -1
	LOCATE , 28: QPrint FUsing(STR$(SA), "######"), COLR, -1
	LOCATE 11, 1: QPrint "Total Hits in League ", 30, -1
	LOCATE , 22: QPrint FUsing(STR$(PH), "######"), COLR, -1
	LOCATE 13, 1: QPrint "Total Earned Runs in League ", 30, -1
	LOCATE , 29: QPrint FUsing(STR$(ER), "######"), COLR, -1

GetDiv1:
	LOCATE 7, 24                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 6     'specify field parameters
	CALL TextIn(BP$, Max, NumOnly, CapsOn, XCode, COLR)        'get input
	BP = VAL(BP$)
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv2:
	LOCATE 9, 28                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 6    'specify field parameters
	CALL TextIn(SA$, Max, NumOnly, CapsOn, XCode, COLR)        'get input
	SA = VAL(SA$)
	IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv3:
	LOCATE 11, 22                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 6    'specify field parameters
	CALL TextIn(PH$, Max, NumOnly, CapsOn, XCode, COLR)        'get input
	PH = VAL(PH$)
	IF XCode = 1 GOTO GetDiv2:           'can't back up any more, stay here
	IF XCode = 2 GOTO Done               'all done entering data

GetDiv4:
	LOCATE 13, 29                         'location of beginning of field
	CapsOn = 0: NumOnly = 0: Max = 6     'specify field parameters
	CALL TextIn(ER$, Max, NumOnly, CapsOn, XCode, COLR)        'get input
	ER = VAL(ER$)
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

21 MD = SA - BP

46 PD = PH / (ER) * 100

51 CLS
	GOTO 300

111 FOR I = 0 TO 22
	TB%(I) = B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)

112 IF B%(I, 4) = 0 THEN SP%(I) = 0: BA%(I) = 0: GOTO 126
	SLP = TB%(I) / B%(I, 4)

116 SP%(I) = SLP * 1000

117 B%(I, 20) = BP

121 BAP = B%(I, 6) / B%(I, 4)
	BA%(I) = BAP * 1000

126 DP%(I) = SP%(I) - BA%(I)

131 DI = DP%(I) - MD
	CH = INT(DI / 10)
	B%(I, 18) = 52 - CH

132 IF B%(I, 18) < 34 THEN B%(I, 18) = 34

133 IF B%(I, 18) > 64 THEN B%(I, 18) = 64

136 NEXT I

141 FOR I = 0 TO 21

142 IF P%(I, 6) = 0 OR P%(I, 10) = 0 THEN P%(I, 12) = 52: GOTO 151

143 P%(I, 14) = BP

145 RHP = P%(I, 7) / (P%(I, 10) / 900 * P%(I, 6))
	RH%(I) = RHP * 100
	PRINT RH%(I)
	'INT(P%(I,7)*900/(P%(I,10)*P%(I,6))*100)/100

146 DF = RH%(I) - PD
	CH = INT(DF / 10) * 2
	P%(I, 12) = INT(CH + 52)
	RH%(I) = RH%(I) / 10

147 IF P%(I, 12) > 64 THEN P%(I, 12) = 64

148 IF P%(I, 12) < 34 THEN P%(I, 12) = 34

151 NEXT I

156 CLS : PRINT "BATTER         BA            SA            GA"
	FOR I = 0 TO 22
	IF B$(I) = "" OR B$(I) = "XXX" THEN 157
	PRINT B$(I), BA%(I), SP%(I), B%(I, 18)

157 NEXT

161 PRINT "HIT ANY KEY TO CONTINUE";

166 U$ = GetKeyPress$

171 CLS : PRINT "PITCHER               HITS/RUN ALLOWED     GA"

172 FOR I = 0 TO 21
	IF P$(I) = "" OR P$(I) = "XXX" THEN 173
	PRINT P$(I), , INT(RH%(I)) / 10, P%(I, 12)

173 NEXT

176 PRINT "HIT ANY KEY TO CONTINUE";

177 U$ = GetKeyPress$

181 CLS

'    L%(1)=267
'    L%(2)=228
'    L%(3)=94
'    L%(4)=100
'    L%(6)=99
'    L%(7)=99
'    K9(0)=99

1385 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
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

235 CLS
	PRINT "EDIT ANOTHER TEAM (YN) ";
236 I$ = GetKeyPress$

237 IF UCASE$(I$) = "Y" THEN 300

246 RUN "HELLO.EXE"
	END

300 COLOR 15, 0: CLS
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

1441 GOSUB 1100

182 IF BO% = 1 THEN BO% = 0: GOTO 300

183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
	LOCATE , 33: PRINT "ANY CHANGE (YN)";
	I$=GetKeyPress$
	IF UCASE$(I$) = "Y" THEN 300
	GOTO 111

2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT

2720 BO% = 1: RETURN

1100 NT = 0
	IF NOT _FILEEXISTS(D$(0) + "FCTEAMS." + YN$) THEN 2060
OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
	Y = LOF(1)
	FIELD #1, 15 AS Q$(0)
	FOR I1 = 1 TO Y / 4342
	GET #1, I1
	FOR I2 = 1 TO 15
	IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
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
T$ = RTrim$ (Q$(0))
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = RTrim$ (Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16)): 'PRINT B$(I),B%(I,I1)
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = RTrim$ (Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = RTrim$ (Q$(1840))
M$ = RTrim$ (Q$(1841))
NN$ = RTrim$ (Q$(1842))
CK = CVI(Q$(1843))
CLOSE 1
RETURN

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
