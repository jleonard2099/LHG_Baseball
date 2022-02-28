DEFINT A-Z
DECLARE SUB TITLE ()
DECLARE SUB GETKEY ()
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
5 CLS
6 DIM H(162), O$(162), V(162), L$(162), WP$(162), LP$(162), I(162), Z1$(40)
  DIM Z2$(40), Z1%(40)
  DIM HW(50), AW(50), HL(50), AL(50), R1(200), R2(200), R3(200), R4(200)
7 DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
  DIM P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21), Q$(15)
  DIM VV$(200), VV%(200), D$(3), ITEM$(10), K9(0) AS LONG
  DIM DV$(4), TR4(4), TR3(4), TR2(4), TR1(4), THW(4), THL(4), TAW(4), TAL(4)
  DIM Y AS LONG
  OPEN "DEV.BAS" FOR INPUT AS #1
  FOR I = 0 TO 3
  INPUT #1, D$(I)
  NEXT
  CLOSE #1
600 CALL TITLE
    ITEM$(1) = "ANALYZE A TEAM'S RECORD                "
    ITEM$(2) = "ANALYZE ANOTHER TEAM IN THE SAME LEAGUE"
    ITEM$(3) = "SELECT A NEW TEAM AND LEAGUE           "
    ITEM$(4) = "RETURN TO MAIN MENU                    "
     SELECTION% = 1
     Count% = 4
     Start% = 1
     SCANCODE% = 4
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 22
     CALL Box0(5, 21, 10, 61, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN END
     I = SELECTION%
612 IF I = 1 THEN 39
613 IF I = 2 THEN 620
614 IF I = 3 THEN 621
615 IF I = 4 THEN 700
39 COLOR 15, 0: CLS
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
    FOR I = 1 TO 200: VV$(I) = "": NEXT
    IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 1441
    CLS : PRINT "MAKE SURE PROPER TEAMS DISK IS IN DRIVE "; D$(0)
    CALL GETKEY
1441 GOSUB 1100
182 IF BO% = 1 THEN BO% = 0: GOTO 600
183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12)
    PRINT T$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 39
    GOTO 40

1100 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN BO% = 1: GOSUB 4500: RETURN
     OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0)
     FOR I1 = 1 TO Y / 4342
     GET #1, I1
     FOR I2 = 1 TO 15
     IF ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
     NEXT I2
     NT = NT + 1
     VV$(NT) = Q$(0): VV%(NT) = I1
1103 NEXT I1
     CLOSE 1
1102 SELECTION% = 1
     Count% = NT
     Start% = 1
     FileColr% = 79
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
COLOR , 4: CLS : LOCATE , 33: COLOR 14: PRINT "TEAM SELECTION": COLOR 15
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15: PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14: PRINT "ENTER"; : COLOR 15: PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN BO% = 1: RETURN
     T = VV%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
   IF NOT EXIST(D$(0) + "FCTEAMS." + YN$) THEN BO% = 1: GOSUB 4500: RETURN
   OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
GET #1, T
T$ = QPRTrim$(Q$(0))
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT
   CLOSE 1
   RETURN

40 ER = 0
119 B$ = T$
    B$ = QPRTrim(B$)
    FOR XX = 4 TO 11
    IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
    NEXT XX
    QQ = 8
    FOR XX = 11 TO 4 STEP -1
    IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
    GOTO 6620
6615 NEXT XX
6620
     Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
    CLS
    IF UCASE$(LEFT$(D$(1), 1)) <> "A" AND UCASE$(LEFT$(D$(1), 1)) <> "B" THEN 111
    PRINT "MAKE SURE PROPER TEAMS DISK IS IN DRIVE "; D$(1)
    CALL GETKEY
111 IF NOT EXIST(D$(1) + Z$ + "S") THEN GOSUB 4550: GOTO 600
    OPEN D$(1) + Z$ + "S" FOR INPUT AS #1
115 INPUT #1, Y
120 FOR J = 1 TO 162: INPUT #1, H(J): NEXT
122 FOR J = 1 TO 162: INPUT #1, O$(J): NEXT
123 FOR J = 1 TO 162: INPUT #1, V(J): NEXT
124 FOR J = 1 TO 162: INPUT #1, L$(J): NEXT
125 FOR J = 1 TO 162: INPUT #1, WP$(J): NEXT
126 FOR J = 1 TO 162: INPUT #1, LP$(J): NEXT
127 FOR J = 1 TO 162: INPUT #1, I(J): NEXT
130 CLOSE #1
42 IF SL = 1 THEN 60

45 Spec$ = D$(2) + "*.LIB"
    Count = FCount%(Spec$)
   IF Count = 0 THEN GOSUB 4575: GOTO 600
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 33
    COLOR 14
    PRINT "SELECT LEAGUE": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1
    COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 600
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 F$ = QPRTrim$(F$)
    LG$ = LEFT$(F$, LEN(F$) - 4)
60 COLOR 15, 0
   CLS
   COLOR 12: LOCATE 12, 5
   PRINT "V"; : COLOR 7: PRINT "IEW OR "; : COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT"
61 M$ = INKEY$: IF M$ = "" THEN 61
   VPP = 0
62 IF UCASE$(M$) = "P" THEN 80
65 VPP = 1
80 Q$ = LG$ + ".LIB"
   CLS
   IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 82
   PRINT "MAKE SURE LEADERS DISK IS IN DRIVE "; D$(2)
   CALL GETKEY
82 OPEN D$(2) + Q$ FOR INPUT AS #3
    INPUT #3, LG$: INPUT #3, D1$: INPUT #3, D2$: INPUT #3, D3$: INPUT #3, D4$
    FOR X = 1 TO 40
    INPUT #3, Z1$(X): INPUT #3, Z1%(X): INPUT #3, Z2$(X)
    NEXT
85 CLOSE #3
   LOCATE 18, 18: PRINT "COMPUTING W/L RECORDS . . . PLEASE WAIT"
145 FOR Q = 1 TO 40: AW(Q) = 0: HW(Q) = 0: AL(Q) = 0: HL(Q) = 0: R1(Q) = 0: R2(Q) = 0: R3(Q) = 0: R4(Q) = 0: NEXT
146 FOR K = 1 TO 4: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: TR1(K) = 0: TR2(K) = 0: TR3(K) = 0: TR4(K) = 0: NEXT
148 FOR K = 1 TO 40
149 IF Z1$(K) = "XXX" THEN 181
150 FOR I = 1 TO Y

152 IF H(I) > V(I) AND L$(I) = "A" AND O$(I) = Z1$(K) THEN AW(K) = AW(K) + 1
153 IF H(I) > V(I) AND L$(I) = "H" AND O$(I) = Z1$(K) THEN HW(K) = HW(K) + 1
154 IF H(I) < V(I) AND L$(I) = "A" AND O$(I) = Z1$(K) THEN AL(K) = AL(K) + 1
155 IF H(I) < V(I) AND L$(I) = "H" AND O$(I) = Z1$(K) THEN HL(K) = HL(K) + 1
156 IF O$(I) = Z1$(K) AND L$(I) = "H" THEN R1(K) = R1(K) + H(I)
157 IF O$(I) = Z1$(K) AND L$(I) = "H" THEN R2(K) = R2(K) + V(I)
158 IF O$(I) = Z1$(K) AND L$(I) = "A" THEN R3(K) = R3(K) + H(I)
159 IF O$(I) = Z1$(K) AND L$(I) = "A" THEN R4(K) = R4(K) + V(I)
180 NEXT I
181 NEXT K
    RDD=0
    IF VPP = 1 THEN 190 ELSE 1105
190 DV$(1) = D1$
191 DV$(2) = D2$
192 DV$(3) = D3$
193 DV$(4) = D4$
200 CLS : COLOR 12
201 LOCATE 2, 35: PRINT Z$
202 COLOR 3: LOCATE 4: PRINT TAB(15); STRING$(7, 196); " HOME "; STRING$(7, 196); TAB(35); CHR$(194); STRING$(7, 196); " AWAY "; STRING$(9, 196); TAB(58); CHR$(194); STRING$(6, 196); " TOTALS "; STRING$(6, 196)
203 LOCATE 5: PRINT TAB(16); "W"; TAB(19); "L"; TAB(23); "PCT."; TAB(28); "RF"; TAB(32); "RA"; TAB(35); CHR$(179); TAB(38); "W"; TAB(41); "L"; TAB(45); "PCT."; TAB(50); "RF"; TAB(54); "RA"; TAB(58); CHR$(179); TAB(61); "W"; TAB(64); "L"; TAB(68);  _
"PCT."; TAB(73); "RF"; TAB(77); "RA"
205 RDD = RDD + 1
206 IF RDD = 1 THEN 210
207 IF RDD = 2 THEN 211
208 IF RDD = 3 THEN 212
209 IF RDD = 4 THEN 213
210 DD = 1: EE = 10: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
211 DD = 11: EE = 20: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
212 DD = 21: EE = 30: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
213 DD = 31: EE = 40: IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 400 ELSE 215
215 COLOR 11: PRINT DV$(RDD); : COLOR 3: PRINT TAB(35); CHR$(179); TAB(58); CHR$(179)
218 FOR K = DD TO EE
219 IF Z1$(K) = "XXX" OR Z1$(K) = T$ THEN 250
220 COLOR 15: LOCATE , 1: PRINT LEFT$(Z1$(K), 13);
221 COLOR 7: LOCATE , 15: PRINT USING "##"; HW(K);
222 LOCATE , 18: PRINT USING "##"; HL(K);
223 IF HW(K) = HW(K) + HL(K) AND HW(K) <> 0 THEN LOCATE , 21: PRINT "1.000";
224 IF HW(K) = 0 THEN LOCATE , 22: PRINT ".000";
225 IF HW(K) > 0 AND HW(K) < HW(K) + HL(K) THEN LOCATE , 22: PRINT USING ".###"; (HW(K) / (HW(K) + HL(K)));
226 LOCATE , 27: PRINT USING "###"; R1(K);
227 LOCATE , 31: PRINT USING "###"; R2(K);
    COLOR 3: PRINT TAB(35); CHR$(179);
228 COLOR 7: LOCATE , 37: PRINT USING "##"; AW(K);
229 LOCATE , 40: PRINT USING "##"; AL(K);
230 IF AW(K) = AW(K) + AL(K) AND AW(K) <> 0 THEN LOCATE , 43: PRINT "1.000";
231 IF AW(K) = 0 THEN LOCATE , 44: PRINT ".000";
232 IF AW(K) > 0 AND AW(K) < AW(K) + AL(K) THEN LOCATE , 44: PRINT USING ".###"; (AW(K) / (AW(K) + AL(K)));
233 LOCATE , 49: PRINT USING "###"; R3(K);
234 LOCATE , 53: PRINT USING "###"; R4(K);
    COLOR 3: PRINT TAB(58); CHR$(179);
235 COLOR 7: LOCATE , 60: PRINT USING "##"; HW(K) + AW(K);
236 LOCATE , 63: PRINT USING "##"; HL(K) + AL(K);
237 IF (HW(K) + AW(K)) = (HW(K) + AW(K) + HL(K) + AL(K)) AND (HW(K) + AW(K)) <> 0 THEN LOCATE , 66: PRINT "1.000";
238 IF (HW(K) + AW(K)) = 0 THEN LOCATE , 67: PRINT ".000";
239 IF (HW(K) + AW(K)) > 0 AND (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) THEN LOCATE , 67: PRINT USING ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
240 LOCATE , 72: PRINT USING "###"; R1(K) + R3(K);
241 LOCATE , 76: PRINT USING "###"; R2(K) + R4(K)
    COLOR 3: PRINT TAB(35); CHR$(179); TAB(58); CHR$(179);
242 THW(RDD) = THW(RDD) + HW(K)
243 THL(RDD) = THL(RDD) + HL(K)
244 TAW(RDD) = TAW(RDD) + AW(K)
245 TAL(RDD) = TAL(RDD) + AL(K)
246 TR1(RDD) = TR1(RDD) + R1(K)
247 TR2(RDD) = TR2(RDD) + R2(K)
248 TR3(RDD) = TR3(RDD) + R3(K)
249 TR4(RDD) = TR4(RDD) + R4(K)
250 NEXT K
260 PRINT
261 COLOR 11: LOCATE , 5: PRINT "TOTALS";
262 LOCATE , 15: PRINT USING "##"; THW(RDD);
263 LOCATE , 18: PRINT USING "##"; THL(RDD);
264 IF THW(RDD) = THW(RDD) + THL(RDD) AND THW(RDD) <> 0 THEN LOCATE , 21: PRINT "1.000";
265 IF THW(RDD) = 0 THEN LOCATE , 22: PRINT ".000";
266 IF THW(RDD) > 0 AND THW(RDD) < THW(RDD) + THL(RDD) THEN LOCATE , 22: PRINT USING ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
267 LOCATE , 27: PRINT USING "###"; TR1(RDD);
268 LOCATE , 31: PRINT USING "###"; TR2(RDD);
    COLOR 3: PRINT TAB(35); CHR$(179);
269 COLOR 11: LOCATE , 37: PRINT USING "##"; TAW(RDD);
270 LOCATE , 40: PRINT USING "##"; TAL(RDD);
271 IF TAW(RDD) = TAW(RDD) + TAL(RDD) AND TAW(RDD) <> 0 THEN LOCATE , 43: PRINT "1.000";
272 IF TAW(RDD) = 0 THEN LOCATE , 44: PRINT ".000";
273 IF TAW(RDD) > 0 AND TAW(RDD) < TAW(RDD) + TAL(RDD) THEN LOCATE , 44: PRINT USING ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
274 LOCATE , 49: PRINT USING "###"; TR3(RDD);
275 LOCATE , 53: PRINT USING "###"; TR4(RDD);
    COLOR 3: PRINT TAB(58); CHR$(179);
280 COLOR 11: LOCATE , 59: PRINT USING "###"; THW(RDD) + TAW(RDD);
281 LOCATE , 62: PRINT USING "###"; THL(RDD) + TAL(RDD);
282 IF THW(RDD) + TAW(RDD) = THW(RDD) + THL(RDD) + TAW(RDD) + TAL(RDD) AND THW(RDD) + TAW(RDD) <> 0 THEN LOCATE , 66: PRINT "1.000";
283 IF THW(RDD) + TAW(RDD) = 0 THEN LOCATE , 67: PRINT ".000";
284 IF THW(RDD) + TAW(RDD) > 0 AND THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) THEN LOCATE , 67: PRINT USING ".###"; (THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD));
285 LOCATE , 72: PRINT USING "###"; TR1(RDD) + TR3(RDD);
286 LOCATE , 76: PRINT USING "###"; TR2(RDD) + TR4(RDD)
390 LOCATE 22, 5: COLOR 14: INPUT "HIT ENTER TO CONTINUE"; DUMMY$
395 IF RDD = 4 THEN 400 ELSE 200
400 CLS : COLOR 12: LOCATE 2, 35: PRINT Z$
401 COLOR 3: LOCATE 4: PRINT TAB(15); STRING$(7, 196); " HOME "; STRING$(7, 196); TAB(35); CHR$(194); STRING$(7, 196); " AWAY "; STRING$(9, 196); TAB(58); CHR$(194); STRING$(6, 196); " TOTALS "; STRING$(6, 196)
402 LOCATE 5: PRINT TAB(16); "W"; TAB(19); "L"; TAB(23); "PCT."; TAB(28); "RF"; TAB(32); "RA"; TAB(35); CHR$(179); TAB(38); "W"; TAB(41); "L"; TAB(45); "PCT."; TAB(50); "RF"; TAB(54); "RA"; TAB(58); CHR$(179); TAB(61); "W"; TAB(64); "L"; TAB(68);  _
"PCT."; TAB(73); "RF"; TAB(77); "RA"
403 COLOR 11: PRINT "DIVISIONS";
404 COLOR 3: PRINT TAB(35); CHR$(179); TAB(58); CHR$(179)
450 FOR W = 1 TO 4
451 IF DV$(W) = "XXX" OR DV$(W) = "" THEN 490 ELSE 461
461 COLOR 15: LOCATE , 1: PRINT DV$(W);
462 COLOR 7: LOCATE , 15: PRINT USING "##"; THW(W);
463 LOCATE , 18: PRINT USING "##"; THL(W);
464 IF THW(W) = THW(W) + THL(W) AND THW(W) <> 0 THEN LOCATE , 21: PRINT "1.000"
465 IF THW(W) = 0 THEN LOCATE , 22: PRINT ".000";
466 IF THW(W) > 0 AND THW(W) < THW(W) + THL(W) THEN LOCATE , 22: PRINT USING ".###"; (THW(W) / (THW(W) + THL(W)));
467 LOCATE , 27: PRINT USING "###"; TR1(W);
468 LOCATE , 31: PRINT USING "###"; TR2(W);
    COLOR 3: PRINT TAB(35); CHR$(179);
469 COLOR 7: LOCATE , 37: PRINT USING "##"; TAW(W);
470 LOCATE , 40: PRINT USING "##"; TAL(W);
471 IF TAW(W) = TAW(W) + TAL(W) AND TAW(W) <> 0 THEN LOCATE , 43: PRINT "1.000";
472 IF TAW(W) = 0 THEN LOCATE , 44: PRINT ".000";
473 IF TAW(W) > 0 AND TAW(W) < TAW(W) + TAL(W) THEN LOCATE , 44: PRINT USING ".###"; (TAW(W) / (TAW(W) + TAL(W)));
474 LOCATE , 49: PRINT USING "###"; TR3(W);
475 LOCATE , 53: PRINT USING "###"; TR4(W);
    COLOR 3: PRINT TAB(58); CHR$(179);
480 COLOR 7: LOCATE , 59: PRINT USING "###"; THW(W) + TAW(W);
481 LOCATE , 62: PRINT USING "###"; THL(W) + TAL(W);
482 IF THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) AND THW(W) + TAW(W) <> 0 THEN LOCATE , 66: PRINT "1.000";
483 IF (THW(W) + TAW(W)) = 0 THEN LOCATE , 67: PRINT ".000";
484 IF (THW(W) + TAW(W)) > 0 AND THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) THEN LOCATE , 67: PRINT USING ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
485 LOCATE , 72: PRINT USING "###"; TR1(W) + TR3(W);
486 LOCATE , 76: PRINT USING "###"; TR2(W) + TR4(W)
    COLOR 3: PRINT TAB(35); CHR$(179); TAB(58); CHR$(179);
490 NEXT W
491 THWW = 0: THLL = 0: TAWW = 0: TALL = 0
492 FOR I = 1 TO 4
493 THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
494 NEXT
495 TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
496 FOR I = 1 TO 4
497 TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
498 NEXT
500 PRINT
501 COLOR 11: LOCATE , 3: PRINT "TOTALS";
502 LOCATE , 14: PRINT USING "###"; THWW;
503 LOCATE , 17: PRINT USING "###"; THLL;
504 IF THWW = THWW + THLL AND THWW <> 0 THEN LOCATE , 21: PRINT "1.000";
505 IF THWW = 0 THEN LOCATE , 22: PRINT ".000";
506 IF THWW > 0 AND THWW < THWW + THLL THEN LOCATE , 22: PRINT USING ".###"; THWW / (THWW + THLL);
507 LOCATE , 27: PRINT USING "###"; TR11;
508 LOCATE , 31: PRINT USING "###"; TR22;
    COLOR 3: PRINT TAB(35); CHR$(179);
509 COLOR 11: LOCATE , 36: PRINT USING "###"; TAWW;
510 LOCATE , 39: PRINT USING "###"; TALL;
511 IF TAWW = TAWW + TALL AND TAWW <> 0 THEN LOCATE , 43: PRINT "1.000";
512 IF TAWW = 0 THEN LOCATE , 44: PRINT ".000";
513 IF TAWW > 0 AND TAWW < TAWW + TALL THEN LOCATE , 44: PRINT USING ".###"; TAWW / (TAWW + TALL);
514 LOCATE , 49: PRINT USING "###"; TR33;
515 LOCATE , 53: PRINT USING "###"; TR44;
    COLOR 3: PRINT TAB(58); CHR$(179);
516 COLOR 11: LOCATE , 59: PRINT USING "###"; THWW + TAWW;
517 LOCATE , 62: PRINT USING "###"; THLL + TALL;
518 IF THWW + TAWW = THWW + TAWW + THLL + TALL AND THWW + TAWW <> 0 THEN LOCATE , 66: PRINT "1.000";
519 IF THWW + TAWW = 0 THEN LOCATE , 67: PRINT ".000";
520 IF THWW + TAWW > 0 AND THWW + TAWW < THWW + TAWW + THLL + TALL THEN LOCATE , 67: PRINT USING ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
521 LOCATE , 72: PRINT USING "###"; TR11 + TR33;
522 LOCATE , 76: PRINT USING "###"; TR22 + TR44
523 LOCATE 22, 5: COLOR 14: INPUT "HIT ENTER TO CONTINUE"; DUMMY$
524 LOCATE 23: COLOR 15: PRINT "VIEW TEAM AGAIN (Y/N)"
525 I$ = INKEY$: IF I$ = "" THEN 525
526 IF UCASE$(I$) = "Y" THEN 528
527 GOTO 600
528 RDD = 0: GOTO 630

620 RDD = 0: SL = 1: VPP = 0: GOTO 39
621 RDD = 0: SL = 0: VPP = 0: GOTO 39
630 FOR K = 1 TO 4: THW(K) = 0: THL(K) = 0: TAW(K) = 0: TAL(K) = 0: TR1(K) = 0: TR2(K) = 0: TR3(K) = 0: TR4(K) = 0: NEXT
632 GOTO 200
700 COLOR 15
    IF DRIVE >= 67 THEN 703
701 CLS : PRINT "MAKE SURE GAME DISK IS IN DRIVE"
    CALL GETKEY
703 RUN "HELLO.EXE"
1105 LPRINT CHR$(15)
1101 WIDTH "LPT1:1", 130
1110 DV$(1) = D1$
1111 DV$(2) = D2$
1112 DV$(3) = D3$
1113 DV$(4) = D4$
1200 LPRINT TAB(5); Z$
1201 LPRINT TAB(22); "------- HOME -------"; TAB(54); "------- AWAY -------"; TAB(86); "-------- TOTALS -------"
1202 LPRINT TAB(22); "W"; TAB(26); "L"; TAB(30); "PCT."; TAB(36); "RF"; TAB(40); "RA"; TAB(54); "W"; TAB(58); "L"; TAB(62); "PCT."; TAB(68); "RF"; TAB(72); "RA"; TAB(86); "W"; TAB(91); "L"; TAB(97); "PCT."; TAB(103); "RF"; TAB(107); "RA"
1204 RDD = RDD + 1
1205 IF RDD = 1 THEN 1210
2206 IF RDD = 2 THEN 1211
1207 IF RDD = 3 THEN 1212
1208 IF RDD = 4 THEN 1213
1210 DD = 1: EE = 10
     IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1211 DD = 11: EE = 20
     IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1212 DD = 21: EE = 30
     IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1213 DD = 31: EE = 40
     IF DV$(RDD) = "XXX" OR DV$(RDD) = "" THEN 1401 ELSE 1215
1215 LPRINT TAB(2); DV$(RDD)
1218 FOR K = DD TO EE
1219 IF Z1$(K) = "XXX" OR Z1$(K) = T$ THEN 1250
1220 LPRINT Z1$(K);
1221 LPRINT TAB(21); USING "##"; HW(K);
1222 LPRINT TAB(25); USING "##"; HL(K);
1223 IF HW(K) = HW(K) + HL(K) AND HW(K) <> 0 THEN LPRINT TAB(28); "1.000";
1224 IF HW(K) = 0 THEN LPRINT TAB(29); ".000";
1225 IF HW(K) > 0 AND HW(K) < HW(K) + HL(K) THEN LPRINT TAB(29); USING ".###"; (HW(K) / (HW(K) + HL(K)));
1226 LPRINT TAB(35); USING "###"; R1(K);
1227 LPRINT TAB(39); USING "###"; R2(K);
1228 LPRINT TAB(53); USING "##"; AW(K);
1229 LPRINT TAB(57); USING "##"; AL(K);
1230 IF AW(K) = AW(K) + AL(K) AND AW(K) <> 0 THEN LPRINT TAB(60); "1.000";
1231 IF AW(K) = 0 THEN LPRINT TAB(61); ".000";
1232 IF AW(K) > 0 AND AW(K) < AW(K) + AL(K) THEN LPRINT TAB(61); USING ".###"; (AW(K) / (AW(K) + AL(K)));
1233 LPRINT TAB(67); USING "###"; R3(K);
1234 LPRINT TAB(71); USING "###"; R4(K);
1235 LPRINT TAB(85); USING "##"; HW(K) + AW(K);
1236 LPRINT TAB(90); USING "##"; HL(K) + AL(K);
1237 IF HW(K) + AW(K) = HW(K) + AW(K) + HL(K) + AL(K) AND HW(K) + AW(K) <> 0 THEN LPRINT TAB(95); "1.000";
1238 IF (HW(K) + AW(K)) = 0 THEN LPRINT TAB(96); ".000";
1239 IF (HW(K) + AW(K)) > 0 AND (HW(K) + AW(K)) < (HW(K) + AW(K) + HL(K) + AL(K)) THEN LPRINT TAB(96); USING ".###"; ((HW(K) + AW(K)) / (HW(K) + AW(K) + HL(K) + AL(K)));
     LPRINT TAB(102); USING "###"; R1(K) + R3(K);
1241 LPRINT TAB(106); USING "###"; R2(K) + R4(K)
1242 THW(RDD) = THW(RDD) + HW(K)
1243 THL(RDD) = THL(RDD) + HL(K)
1244 TAW(RDD) = TAW(RDD) + AW(K)
1245 TAL(RDD) = TAL(RDD) + AL(K)
1246 TR1(RDD) = TR1(RDD) + R1(K)
1247 TR2(RDD) = TR2(RDD) + R2(K)
1248 TR3(RDD) = TR3(RDD) + R3(K)
1249 TR4(RDD) = TR4(RDD) + R4(K)
1250 NEXT K
1261 LPRINT TAB(5); "TOTALS";
1262 LPRINT TAB(21); USING "##"; THW(RDD);
1263 LPRINT TAB(25); USING "##"; THL(RDD);
1264 IF THW(RDD) = THW(RDD) + THL(RDD) AND THW(RDD) <> 0 THEN LPRINT TAB(28); "1.000";
1265 IF THW(RDD) = 0 THEN LPRINT TAB(29); ".000";
1266 IF THW(RDD) > 0 AND THW(RDD) < THW(RDD) + THL(RDD) THEN LPRINT TAB(29); USING ".###"; (THW(RDD) / (THW(RDD) + THL(RDD)));
1267 LPRINT TAB(35); USING "###"; TR1(RDD);
1268 LPRINT TAB(39); USING "###"; TR2(RDD);
1269 LPRINT TAB(53); USING "##"; TAW(RDD);
1270 LPRINT TAB(57); USING "##"; TAL(RDD);
1271 IF TAW(RDD) = TAW(RDD) + TAL(RDD) AND TAW(RDD) <> 0 THEN LPRINT TAB(60); "1.000";
1272 IF TAW(RDD) = 0 THEN LPRINT TAB(61); ".000";
1273 IF TAW(RDD) > 0 AND TAW(RDD) < TAW(RDD) + TAL(RDD) THEN LPRINT TAB(61); USING ".###"; (TAW(RDD) / (TAW(RDD) + TAL(RDD)));
1274 LPRINT TAB(67); USING "###"; TR3(RDD);
1275 LPRINT TAB(71); USING "###"; TR4(RDD);
1280 LPRINT TAB(84); USING "###"; THW(RDD) + TAW(RDD);
1281 LPRINT TAB(89); USING "###"; THL(RDD) + TAL(RDD);
1282 IF THW(RDD) + TAW(RDD) = THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) AND THW(RDD) + TAW(RDD) <> 0 THEN LPRINT TAB(95); "1.000";
1283 IF THW(RDD) + TAW(RDD) = 0 THEN LPRINT TAB(96); ".000";
1284 IF THW(RDD) + TAW(RDD) > 0 AND THW(RDD) + TAW(RDD) < THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD) THEN LPRINT TAB(96); USING ".###"; ((THW(RDD) + TAW(RDD)) / (THW(RDD) + TAW(RDD) + THL(RDD) + TAL(RDD)));
1285 LPRINT TAB(102); USING "###"; TR1(RDD) + TR3(RDD);
1286 LPRINT TAB(106); USING "###"; TR2(RDD) + TR4(RDD)
1287 LPRINT
1395 IF RDD = 4 THEN 1401 ELSE 1204
1401 LPRINT : LPRINT : LPRINT : LPRINT : LPRINT TAB(22); "------- HOME -------"; TAB(54); "------- AWAY -------"; TAB(86); "-------- TOTALS -------"
1402 LPRINT TAB(22); "W"; TAB(26); "L"; TAB(30); "PCT."; TAB(36); "RF"; TAB(40); "RA"; TAB(54); "W"; TAB(58); "L"; TAB(62); "PCT."; TAB(68); "RF"; TAB(72); "RA"; TAB(86); "W"; TAB(91); "L"; TAB(97); "PCT."; TAB(103); "RF"; TAB(107); "RA"
1403 PRINT
1450 FOR W = 1 TO 4
1451 IF DV$(W) = "XXX" OR DV$(W) = "" THEN 1490 ELSE 1461
1461 LPRINT TAB(2); DV$(W);
1462 LPRINT TAB(21); USING "##"; THW(W);
1463 LPRINT TAB(25); USING "##"; THL(W);
1464 IF THW(W) = THW(W) + THL(W) AND THW(W) <> 0 THEN LPRINT TAB(28); "1.000";
1465 IF THW(W) = 0 THEN LPRINT TAB(29); ".000";
1466 IF THW(W) > 0 AND THW(W) < THW(W) + THL(W) THEN LPRINT TAB(29); USING ".###"; (THW(W) / (THW(W) + THL(W)));
1467 LPRINT TAB(35); USING "###"; TR1(W);
1468 LPRINT TAB(39); USING "###"; TR2(W);
1469 LPRINT TAB(53); USING "##"; TAW(W);
1470 LPRINT TAB(57); USING "##"; TAL(W);
1471 IF TAW(W) = TAW(W) + TAL(W) AND TAW(W) <> 0 THEN LPRINT TAB(60); "1.000";
1472 IF TAW(W) = 0 THEN LPRINT TAB(61); ".000";
1473 IF TAW(W) > 0 AND TAW(W) < TAW(W) + TAL(W) THEN LPRINT TAB(61); USING ".###"; (TAW(W) / (TAW(W) + TAL(W)));
1474 LPRINT TAB(67); USING "###"; TR3(W);
1475 LPRINT TAB(71); USING "###"; TR4(W);
1480 LPRINT TAB(84); USING "###"; THW(W) + TAW(W);
1481 LPRINT TAB(89); USING "###"; THL(W) + TAL(W);
1482 IF THW(W) + TAW(W) = THW(W) + TAW(W) + THL(W) + TAL(W) AND THW(W) + TAW(W) <> 0 THEN LPRINT TAB(95); "1.000";
1483 IF (THW(W) + TAW(W)) = 0 THEN LPRINT TAB(96); ".000";
1484 IF (THW(W) + TAW(W)) > 0 AND THW(W) + TAW(W) < THW(W) + TAW(W) + THL(W) + TAL(W) THEN LPRINT TAB(96); USING ".###"; ((THW(W) + TAW(W)) / (THW(W) + TAW(W) + THL(W) + TAL(W)));
1485 LPRINT TAB(102); USING "###"; TR1(W) + TR3(W);
1486 LPRINT TAB(106); USING "###"; TR2(W) + TR4(W)
1490 NEXT W
1491 THWW = 0: THLL = 0: TAWW = 0: TALL = 0
1492 FOR I = 1 TO 4
1493 THWW = THWW + THW(I): THLL = THLL + THL(I): TAWW = TAWW + TAW(I): TALL = TALL + TAL(I)
1494 NEXT
1495 TR11 = 0: TR22 = 0: TR33 = 0: TR44 = 0
1496 FOR I = 1 TO 4
1497 TR11 = TR11 + TR1(I): TR22 = TR22 + TR2(I): TR33 = TR33 + TR3(I): TR44 = TR44 + TR4(I)
1498 NEXT
1500 LPRINT TAB(5); "TOTALS";
1502 LPRINT TAB(20); USING "###"; THWW;
1503 LPRINT TAB(24); USING "###"; THLL;
1504 IF THWW = THWW + THLL AND THWW <> 0 THEN LPRINT TAB(28); "1.000";
1505 IF THWW = 0 THEN LPRINT TAB(29); ".000";
1506 IF THWW > 0 AND THWW < THWW + THLL THEN LPRINT TAB(29); USING ".###"; THWW / (THWW + THLL);
1507 LPRINT TAB(35); USING "###"; TR11;
1508 LPRINT TAB(39); USING "###"; TR22;
1509 LPRINT TAB(52); USING "###"; TAWW;
1510 LPRINT TAB(56); USING "###"; TALL;
1511 IF TAWW = TAWW + TALL AND TAWW <> 0 THEN LPRINT TAB(60); "1.000";
1512 IF TAWW = 0 THEN LPRINT TAB(61); ".000";
1513 IF TAWW > 0 AND TAWW < TAWW + TALL THEN LPRINT TAB(61); USING ".###"; TAWW / (TAWW + TALL);
1514 LPRINT TAB(67); USING "###"; TR33;
1515 LPRINT TAB(71); USING "###"; TR44;
1516 LPRINT TAB(84); USING "###"; THWW + TAWW;
1517 LPRINT TAB(89); USING "###"; THLL + TALL;
1518 IF THWW + TAWW = THWW + TAWW + THLL + TALL AND THWW + TAWW <> 0 THEN LPRINT TAB(95); "1.000";
1519 IF THWW + TAWW = 0 THEN LPRINT TAB(96); ".000";
1520 IF THWW + TAWW > 0 AND THWW + TAWW < THWW + TAWW + THLL + TALL THEN LPRINT TAB(96); USING ".###"; (THWW + TAWW) / (THWW + TAWW + THLL + TALL);
1521 LPRINT TAB(102); USING "###"; TR11 + TR33;
1522 LPRINT TAB(106); USING "###"; TR22 + TR44
1523 LPRINT CHR$(18)
1524 WIDTH "LPT1:1", 80
1525 LPRINT CHR$(12)
1552 GOTO 600
4500 LOCATE 10, 8
     CALL CSMsgBox("NO TEAM FILES WERE FOUND MATCHING THIS ID.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     RETURN
4550 LOCATE 10, 8
     CALL CSMsgBox("NO COMPILED STATS WERE FOUND FOR THIS TEAM.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3806 I$ = INKEY$: IF I$ = "" THEN 3806
     CALL CSMsgBox("", 0, Cnf)
     RETURN
4575 LOCATE 10, 8
     CALL CSMsgBox("NO LEAGUE FILES WERE FOUND.  PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3807 I$ = INKEY$: IF I$ = "" THEN 3807
     CALL CSMsgBox("", 0, Cnf)
     RETURN

STATIC SUB CSMsgBox (Message$, Wdth, Cnf AS Config)
    IF LEN(Message$) THEN
       Row = CSRLIN                             'Get the Row Position
       REDIM Temp$(23)                          'Make a temporary array to
                                                '  hold the parsed up lines
       MesLen = LEN(Message$)                   'Get the total length of the
                                                '  help message
       LN = 0                                   'Line number
       Strt = 1                                 'Starting parse position
       '*** Parse the message into lines
       DO
           WHILE MID$(Message$, Strt, 1) = " "  'Skip over blanks
              Strt = Strt + 1
           WEND
           ND = Strt + Wdth                     'Guess at the end of line
                                                'Loop backwards to find
           WHILE MID$(Message$, ND, 1) <> " " AND ND <= MesLen AND ND > Strt
               ND = ND - 1                      '  blank space
           WEND
           LN = LN + 1                          'Increment the Line number
                                                'Pull out the new line
           Temp$(LN) = MID$(Message$, Strt, ND - Strt)
           Strt = ND + 1                        'Reset starting posit. for
                                                '  next line
       LOOP UNTIL ND >= MesLen                  'Get more unless @ end of mes
       BotRow = Row + LN + 1                    'Find the bottom row #
       IF BotRow > 24 THEN BotRow = 24          'No farther than line 24
       ArraySize = 80 * (BotRow - Row + 2)      'Find the array size to use
       REDIM ScrBuf(ArraySize)                  '  for the background screen
       MScrnSave Row, 1, BotRow + 1, 80, ScrBuf(0)
       BoxWdth = Wdth + 4                       'Find the outer box width
       ColPos = 40 - (BoxWdth \ 2)              'Starting posit for centered
                                                '  box
       LOCATE Row, ColPos                       'Print the top of box
       MQPrint "É" + STRING$(Wdth + 2, "Í") + "»", Cnf.MenBox
       Box$ = "º" + SPACE$(Wdth + 2) + "º"      'Make the box sides
       FOR N = 1 TO LN                          'Print message lines
           LOCATE N + Row, ColPos
           MQPrint Box$, Cnf.MenBox
           LOCATE , ColPos + 2
           MQPrint Temp$(N), Cnf.ActivCh
       NEXT
       LOCATE N + Row, ColPos                   'Print the bottom of box
       MQPrint "È" + STRING$(Wdth + 2, "Í") + "¼", Cnf.MenBox
       MPaintBox Row + 1, ColPos + BoxWdth, N + Row, ColPos + BoxWdth + 1, 8
       MPaintBox N + Row + 1, ColPos + 2, N + Row + 1, ColPos + BoxWdth + 1, 8
       Pass = 1                                 'Set flag say we've been here
       ERASE Temp$                              'Don't need this anymore
    ELSEIF Pass = 1 THEN                        'If we've been here before,
                                                '  restore the screen
       MScrnRest Row, 1, BotRow + 1, 80, SEG ScrBuf(0)
       ERASE ScrBuf                             'Clean up memory
       Pass = 0                                 'Reset flag for next call
    END IF
END SUB

SUB GETKEY ()
    LOCATE 24, 1: PRINT "PRESS ANY KEY TO CONTINUE";
1240 I$ = INKEY$: IF I$ = "" THEN 1240
END SUB

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

