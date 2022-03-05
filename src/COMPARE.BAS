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
  CLS
  DIM K9(1) AS LONG, Q$(1844), FS%(22, 4, 4), D$(4)
  DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21)
  DIM B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22)
  DIM VV$(200), V%(200), NP$(21), ITEM$(10)
  DIM P1%(22), B1%(23), TB%(15), TP(33) AS LONG
  DIM Y AS LONG
  DIM I2 AS SINGLE, I3 AS SINGLE, I4 AS SINGLE, I5 AS SINGLE, ERX AS SINGLE
   '$INCLUDE: 'DEFCNF.BI'
   '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
   DRIVE = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 3: INPUT #2, D$(I)
   NEXT
   CLOSE #2
10 CALL TITLE
   ITEM$(1) = "VIEW/PRINT STATISTICAL COMPARISONS"
   ITEM$(2) = "RETURN TO UTILITIES MENU          "
     SELECTION% = 1
     Count% = 2
     Start% = 1
     SCANCODE% = 2
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 24
     CALL Box0(5, 23, 8, 58, 2, 31)
     CALL SHOWCURSOR
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HIDECURSOR
     COLOR , 0
     IF SCANCODE% = 27 THEN END
     I1 = SELECTION%
     IF I1 = 2 THEN 550
40  COLOR 15, 0: CLS
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
182 IF BO% = 1 THEN BO% = 0: GOTO 10
183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12)
    PRINT T$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 40
    GOTO 119

1100 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN BO% = 1: GOSUB 4500: RETURN
     OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0)
     FOR I1 = 1 TO Y / 4342
     GET #1, I1
     FOR I2 = 1 TO 15
     IF LEFT$(Q$(0),2) = "XX"OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
     NEXT I2
     NT = NT + 1
     VV$(NT) = Q$(0): V%(NT) = I1
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
     T = V%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN GOSUB 4500: BO% = 1: RETURN
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
T$ = Q$(0)
1532 IF RIGHT$(T$, 1) = " " THEN T$ = LEFT$(T$, LEN(T$) - 1): GOTO 1532
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = QPRTrim$(Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = QPRTrim$(Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = QPRTrim$(Q$(1840)): M$ = QPRTrim$(Q$(1841)): NN$ = QPRTrim$(Q$(1842))
    CK = CVI(Q$(1843))
    CLOSE 1
    RETURN

119 B$ = T$
    B$ = QPRTrim(B$)
    FOR XX = 4 TO 11
    IF MID$(B$, XX, 1) = " " OR MID$(B$, XX, 1) = "." THEN MID$(B$, XX, 1) = "_"
    NEXT XX
    FOR XX = 12 TO 14
    IF MID$(B$, XX, 2) = "ST" THEN MID$(B$, 9, 1) = "_": MID$(B$, 10, 2) = "ST"
    NEXT XX
    QQ = 8
    FOR XX = 11 TO 4 STEP -1
    IF MID$(B$, XX, 1) = "_" THEN QQ = XX - 4: GOTO 6615
    GOTO 6620
6615 NEXT XX
6620 REM:Z$ = LEFT$(Z$, 11)
     REM:Z$ = LEFT$(RIGHT$(B$, LEN(B$) - 3), 8) + "." + LEFT$(B$, 2)
     Z$ = MID$(B$, 4, QQ) + "." + LEFT$(B$, 2)
    GOSUB 9000
120 IF RIGHT$(Z$,1)="_" THEN Z$=LEFT$(Z$,LEN(Z$)-1):GOTO 120
    IF NOT EXIST%(D$(1) + Z$) THEN GOSUB 4550: GOTO 10
    OPEN D$(1) + Z$ FOR INPUT AS #1
    INPUT #1, A$: INPUT #1, W2%: INPUT #1, L2%
    FOR I = 0 TO 22: INPUT #1, B$(I): INPUT #1, B1%(I): NEXT I
    FOR I = 0 TO 21: INPUT #1, P$(I): INPUT #1, P1%(I): NEXT I
    FOR I = 0 TO 22: FOR I1 = 0 TO 21: INPUT #1, B0%(I, I1): NEXT I1: NEXT I
    FOR I = 0 TO 21: FOR I1 = 0 TO 41: INPUT #1, P0%(I, I1): NEXT I1: NEXT I
    INPUT #1, D0%: INPUT #1, D1%
    FOR I = 0 TO 21: INPUT #1, T(I): NEXT I
    FOR I = 0 TO 22: INPUT #1, T1(I): NEXT I
    CLOSE #1
170 COLOR 15, 0: CLS
    LOCATE 12, 5: COLOR 12: PRINT "V"; : COLOR 7: PRINT "IEW OR "; : COLOR 12: PRINT "P"; : COLOR 7: PRINT "RINT"
171 I$ = INKEY$: IF I$ = "" THEN 171
    IF UCASE$(I$) = "P" THEN 600
190 GOSUB 9200
200 COLOR 0, 0: CLS
    FOR I = 0 TO 22: IF LEFT$(B$(I), 3) = "XXX" THEN 201 ELSE 202
201 B$(I) = ""
202 NEXT
    FOR I = 0 TO 21: IF LEFT$(P$(I), 3) = "XXX" THEN 205 ELSE 206
205 P$(I) = ""
206 NEXT
    COLOR 9: LOCATE 1, 22: PRINT "STAT COMPARISONS FOR "; : COLOR 15: PRINT T$
    FOR I = 0 TO 16
    COLOR 12: LOCATE I + 4, 10: PRINT USING "##"; I + 1; : PRINT ". "; : COLOR 7: PRINT LEFT$(B$(I), 14): NEXT
    FOR I = 17 TO 22
    COLOR 12: LOCATE I - 13, 30: PRINT USING "##"; I + 1; : PRINT ". "; : COLOR 7: PRINT LEFT$(B$(I), 14): NEXT
    FOR I = 0 TO 10
    COLOR 12: LOCATE I + 10, 30: PRINT USING "##"; I + 24; : PRINT ". "; : COLOR 3: PRINT LEFT$(P$(I), 14): NEXT
    FOR I = 11 TO 21
    COLOR 12: LOCATE I - 7, 50: PRINT USING "##"; I + 24; : PRINT ". "; : COLOR 3: PRINT LEFT$(P$(I), 14): NEXT
    COLOR 12: LOCATE , 50: PRINT "46. "; : COLOR 14: PRINT "View Players in Sequence"
    COLOR 12: LOCATE , 50: PRINT "47. "; : COLOR 14: PRINT "View Another Team"
    COLOR 12: LOCATE , 50: PRINT "48. "; : COLOR 14: PRINT "Exit to DOS"
    COLOR 12: LOCATE , 50: PRINT "49. "; : COLOR 14: PRINT "Return to UTILITIES menu"
229 COLOR 15: LOCATE 23, 30: INPUT "ENTER # TO VIEW "; B9
    IF B9 < 1 OR B9 > 49 THEN GOTO 229
    IF B9 >= 1 AND B9 <= 23 THEN I = B9 - 1: CLS : GOSUB 5000: GOTO 265
    IF B9 >= 24 AND B9 <= 45 THEN I = B9 - 24: CLS : GOSUB 6000: GOTO 400
    IF B9 = 46 THEN BI = 1: CLS : GOTO 260
    IF B9 = 47 THEN GOTO 40
    IF B9 = 48 THEN CLS : END
    IF B9 = 49 THEN GOSUB 8106: RUN "HELLO.EXE"
260 GOSUB 5000
    I = I + 1
    FOR I = 0 TO 22
    LOCATE 2, 30: COLOR 15, 0: PRINT STRING$(40, 177)
    IF B$(I) = "XXX" OR B$(I) = "END" OR B$(I) = "" THEN 391
265 IF B%(I, 22) = 2 THEN PST$ = "Catcher"
    IF B%(I, 22) = 3 THEN PST$ = "First Base"
    IF B%(I, 22) = 4 THEN PST$ = "Second Base"
    IF B%(I, 22) = 5 THEN PST$ = "Third Base"
    IF B%(I, 22) = 6 THEN PST$ = "Shortstop"
    IF B%(I, 22) = 7 THEN PST$ = "Left Field"
    IF B%(I, 22) = 8 THEN PST$ = "Center Field"
    IF B%(I, 22) = 9 THEN PST$ = "Right Field"
    COLOR 11: LOCATE 2, 2: PRINT T$
    COLOR 15, 0: LOCATE 2, 30: PRINT STRING$(14, 177): COLOR 10: LOCATE 2, 30: PRINT LEFT$(B$(I), 14); : PRINT " - "; : PRINT PST$
    COLOR 12: LOCATE 5, 2: PRINT "REGULAR "
    LOCATE 6, 2: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 7, 4: PRINT STRING$(8, 177)
    LOCATE 4, 13: COLOR 0, 11: PRINT "  G  "; :
    PRINT TAB(19); "AB "; :
    PRINT TAB(24); "R  "; :
    PRINT TAB(28); "H  "; :
    PRINT TAB(31); "2B "; :
    PRINT TAB(35); "3B "; :
    PRINT TAB(39); "HR "; :
    PRINT TAB(43); "RBI "; :
    PRINT TAB(48); "BB "; :
    PRINT TAB(52); "SO "; :
    PRINT TAB(56); "SB "; :
    PRINT TAB(59); "CS "; :
    PRINT TAB(63); "AVG "; :
    PRINT TAB(68); "SLG "; :
    PRINT TAB(73); "OBP"; :
    COLOR 0, 11: LOCATE 9, 13: PRINT "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
    LOCATE 10, 13: PRINT "  RUNS"; :
    PRINT TAB(21); " HITS"; :
    PRINT TAB(30); " 2B "; :
    PRINT TAB(37); " 3B "; :
    PRINT TAB(44); " HR "; :
    PRINT TAB(50); " RBI "; :
    PRINT TAB(58); " BB "; :
    PRINT TAB(65); " SO "; :
    PRINT TAB(70); " %-SB "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 0, 11: LOCATE 15, 13: PRINT "FLD.PCT."
    COLOR 1, 11: LOCATE 5, 13: PRINT USING "###"; B%(I, 3);
    IF B%(I, 4) = 0 OR B%(I, 5) = 0 THEN LOCATE 11, 13: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 5) > 0 THEN LOCATE 11, 13: PRINT USING "###.##"; B%(I, 4) / B%(I, 5)
    LOCATE 5, 16: PRINT USING "#####"; B%(I, 4)
    IF B%(I, 4) = 0 OR B%(I, 6) = 0 THEN LOCATE 11, 19: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 6) > 0 THEN LOCATE 11, 19: PRINT USING "####.##"; B%(I, 4) / B%(I, 6)
    LOCATE 5, 21: PRINT USING "####"; B%(I, 5)
    IF B%(I, 4) = 0 OR B%(I, 7) = 0 THEN LOCATE 11, 26: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 7) > 0 THEN LOCATE 11, 26: PRINT USING "####.##"; B%(I, 4) / B%(I, 7)
    LOCATE 5, 25: PRINT USING "####"; B%(I, 6)
    IF B%(I, 4) = 0 OR B%(I, 8) = 0 THEN LOCATE 11, 33: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 8) > 0 THEN LOCATE 11, 33: PRINT USING "####.##"; B%(I, 4) / B%(I, 8)
    LOCATE 5, 29: PRINT USING "####"; B%(I, 7)
    IF B%(I, 4) = 0 OR B%(I, 9) = 0 THEN LOCATE 11, 40: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 9) > 0 THEN LOCATE 11, 40: PRINT USING "####.##"; B%(I, 4) / B%(I, 9)
    LOCATE 5, 33: PRINT USING "####"; B%(I, 8)
    IF B%(I, 4) = 0 OR B%(I, 10) = 0 THEN LOCATE 11, 47: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 10) > 0 THEN LOCATE 11, 47: PRINT USING "####.##"; B%(I, 4) / B%(I, 10)
    LOCATE 5, 37: PRINT USING "####"; B%(I, 9)
    IF B%(I, 4) = 0 OR B%(I, 11) = 0 THEN LOCATE 11, 54: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 11) > 0 THEN LOCATE 11, 54: PRINT USING "####.##"; B%(I, 4) / B%(I, 11)
    LOCATE 5, 41: PRINT USING "#####"; B%(I, 10)
    IF B%(I, 4) = 0 OR B%(I, 12) = 0 THEN LOCATE 11, 61: PRINT " ---.--"
    IF B%(I, 4) > 0 AND B%(I, 12) > 0 THEN LOCATE 11, 61: PRINT USING "####.##"; B%(I, 4) / B%(I, 12)
    LOCATE 5, 46: PRINT USING "####"; B%(I, 11)
    LOCATE 5, 50: PRINT USING "####"; B%(I, 12)
    IF B%(I, 13) + B%(I, 14) = 0 OR B%(I, 13) = 0 THEN LOCATE 11, 68: PRINT "    00% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 1, 11: IF B%(I, 13) > 0 AND B%(I, 14) = 0 THEN LOCATE 11, 68: PRINT "   100% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 1, 11: IF B%(I, 13) > 0 AND B%(I, 14) > 0 THEN LOCATE 11, 68: PRINT "   "; : PRINT USING "###"; (B%(I, 13) / (B%(I, 13) + B%(I, 14))) * 100; : PRINT "% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 1, 11: LOCATE 5, 54: PRINT USING "####"; B%(I, 13)
    LOCATE 5, 58: PRINT USING "###"; B%(I, 14); : PRINT " "
    I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
    IF B%(I, 4) = 0 OR I2 = 0 THEN LOCATE 5, 62: PRINT ".000"; : PRINT " "
    IF I2 > 0 AND I2 < 100 THEN LOCATE 5, 62: PRINT ".0"; : PRINT USING "##"; I2; : PRINT " "
    IF I2 >= 100 THEN LOCATE 5, 62: PRINT "."; : PRINT USING "###"; I2; : PRINT " "
    IF B%(I, 4) = 0 OR I3 = 0 THEN LOCATE 5, 67: PRINT ".000"; : PRINT " "
    IF I3 > 0 AND I3 < 100 THEN LOCATE 5, 67: PRINT ".0"; : PRINT USING "##"; I3; : PRINT " "
    IF I3 >= 100 THEN LOCATE 5, 67: PRINT "."; : PRINT USING "###"; I3; : PRINT " "
    IF B%(I, 4) = 0 OR I4 = 0 THEN LOCATE 5, 72: PRINT ".000"; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 1, 11: IF I4 > 0 AND I4 < 100 THEN LOCATE 5, 72: PRINT ".0"; : PRINT USING "##"; I4; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 1, 11: IF I4 >= 100 THEN LOCATE 5, 72: PRINT "."; : PRINT USING "###"; I4; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 7, 0: LOCATE 7, 15: PRINT STRING$(63, 177)
    COLOR 1, 11: LOCATE 16, 13: PRINT "  ."; : PRINT USING "###"; B%(I, 26); : PRINT "  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 12: LOCATE 11, 2: PRINT "REGULAR "
    LOCATE 12, 2: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 13, 4: PRINT STRING$(8, 177)
    COLOR 12: LOCATE 16, 2: PRINT "REGULAR "
    LOCATE 17, 2: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 18, 4: PRINT STRING$(8, 177)
    COLOR 14, 11: LOCATE 6, 13: PRINT USING "###"; B0%(I, 14);
    IF B0%(I, 0) = 0 OR B0%(I, 2) = 0 THEN LOCATE 12, 13: PRINT "---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 2) > 0 THEN LOCATE 12, 13: PRINT USING "###.##"; B0%(I, 0) / B0%(I, 2)
    LOCATE 6, 16: PRINT USING "#####"; B0%(I, 0)
    IF B0%(I, 0) = 0 OR B0%(I, 1) = 0 THEN LOCATE 12, 19: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 1) > 0 THEN LOCATE 12, 19: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 1)
    LOCATE 6, 21: PRINT USING "####"; B0%(I, 2)
    IF B0%(I, 0) = 0 OR B0%(I, 4) = 0 THEN LOCATE 12, 26: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 4) > 0 THEN LOCATE 12, 26: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 4)
    LOCATE 6, 25: PRINT USING "####"; B0%(I, 1)
    IF B0%(I, 0) = 0 OR B0%(I, 5) = 0 THEN LOCATE 12, 33: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 5) > 0 THEN LOCATE 12, 33: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 5)
    LOCATE 6, 29: PRINT USING "####"; B0%(I, 4)
    IF B0%(I, 0) = 0 OR B0%(I, 6) = 0 THEN LOCATE 12, 40: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 6) > 0 THEN LOCATE 12, 40: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 6)
    LOCATE 6, 33: PRINT USING "####"; B0%(I, 5)
    IF B0%(I, 0) = 0 OR B0%(I, 3) = 0 THEN LOCATE 12, 47: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 3) > 0 THEN LOCATE 12, 47: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 3)
    LOCATE 6, 37: PRINT USING "####"; B0%(I, 6)
    IF B0%(I, 0) = 0 OR B0%(I, 7) = 0 THEN LOCATE 12, 54: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 7) > 0 THEN LOCATE 12, 54: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 7)
    LOCATE 6, 41: PRINT USING "#####"; B0%(I, 3)
    IF B0%(I, 0) = 0 OR B0%(I, 8) = 0 THEN LOCATE 12, 61: PRINT " ---.--"
    IF B0%(I, 0) > 0 AND B0%(I, 8) > 0 THEN LOCATE 12, 61: PRINT USING "####.##"; B0%(I, 0) / B0%(I, 8)
    LOCATE 6, 46: PRINT USING "####"; B0%(I, 7)
    LOCATE 6, 50: PRINT USING "####"; B0%(I, 8)
    IF B0%(I, 9) + B0%(I, 10) = 0 OR B0%(I, 9) = 0 THEN LOCATE 12, 68: PRINT "    00% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF B0%(I, 9) > 0 AND B0%(I, 10) = 0 THEN LOCATE 12, 68: PRINT "   100% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF B0%(I, 9) > 0 AND B0%(I, 10) > 0 THEN LOCATE 12, 68: PRINT "   "; : PRINT USING "###"; (B0%(I, 9) / (B0%(I, 10) + B0%(I, 9))) * 100; : PRINT "% "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: LOCATE 6, 54: PRINT USING "####"; B0%(I, 9)
    LOCATE 6, 58: PRINT USING "###"; B0%(I, 10); : PRINT " "
    I2 = 0: I3 = 0: I4 = 0: IF B0%(I, 0) > 0 THEN I2 = INT((B0%(I, 1) / B0%(I, 0) + .0005) * 1000): I3 = INT(((B0%(I, 1) + B0%(I, 4) + 2 * B0%(I, 5) + 3 * B0%(I, 6)) / B0%(I, 0) + .0005) * 1000): I4 = INT(((B0%(I, 1) + B0%(I, 7)) / (B0%(I, 7) + B0%( _
I, 0)) + .0005) * 1000)
    IF B0%(I, 0) = 0 OR I2 = 0 THEN LOCATE 6, 62: PRINT ".000"; : PRINT " "
    IF I2 > 0 AND I2 < 100 THEN LOCATE 6, 62: PRINT ".0"; : PRINT USING "##"; I2; : PRINT " "
    IF I2 >= 100 THEN LOCATE 6, 62: PRINT "."; : PRINT USING "###"; I2; : PRINT " "
    IF B0%(I, 0) = 0 OR I3 = 0 THEN LOCATE 6, 67: PRINT ".000"; : PRINT " "
    IF I3 > 0 AND I3 < 100 THEN LOCATE 6, 67: PRINT ".0"; : PRINT USING "##"; I3; : PRINT " "
    IF I3 >= 100 THEN LOCATE 6, 67: PRINT "."; : PRINT USING "###"; I3; : PRINT " "
    IF B0%(I, 0) = 0 OR I4 = 0 THEN LOCATE 6, 72: PRINT ".000"; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF I4 > 0 AND I4 < 100 THEN LOCATE 6, 72: PRINT ".0"; : PRINT USING "##"; I4; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF I4 >= 100 THEN LOCATE 6, 72: PRINT "."; : PRINT USING "###"; I4; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 7, 0: LOCATE 13, 15: PRINT STRING$(63, 177)
    IF B0%(I, 16) = 0 AND B0%(I, 17) = 0 THEN I5 = 0 ELSE I5 = ((B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11))) * 1000
    COLOR 14, 11: IF I5 = 0 THEN LOCATE 17, 13: PRINT "  .000  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF I5 > 0 AND I5 < 1000 THEN LOCATE 17, 13: PRINT "  ."; : PRINT USING "###"; I5; : PRINT "  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 11: IF I5 = 1000 THEN LOCATE 17, 13: PRINT USING "##"; I5 / 1000; : PRINT ".000  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 7, 0: LOCATE 18, 15: PRINT STRING$(8, 177)
    COLOR 15, 0: LOCATE 23, 4: PRINT "HIT ANY KEY TO CONTINUE"
    IF BI = 1 THEN 380 ELSE 381
380 LOCATE 23, 28: PRINT "OR ESC TO RETURN TO MENU"
381 I$ = INKEY$: IF I$ = "" THEN 381
    IF I$ = CHR$(0) THEN 391
    IF I$ = CHR$(27) THEN 388
    IF BI = 0 THEN 387
    IF BI = 1 THEN 391
    IF I < 17 THEN 391 ELSE 395
387 CLS : GOTO 200
388 BI = 0: GOTO 200
    IF BI = 1 THEN 391 ELSE 395
391 NEXT
    IF BI = 1 THEN GOTO 395
395 CLS : GOSUB 6000
    FOR I = 0 TO 21
    IF P$(I) = "XXX" OR P$(I) = "END" OR P$(I) = "" THEN 508
400 COLOR 11: LOCATE 2, 2: PRINT T$
    COLOR 15, 0: LOCATE 2, 30: PRINT STRING$(14, 177): COLOR 13: LOCATE 2, 30: PRINT LEFT$(P$(I), 14)
    COLOR 10: LOCATE 5, 2: PRINT "REGULAR "
    LOCATE 6, 2: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 7, 4: PRINT STRING$(8, 177)
    COLOR 0, 12: LOCATE 4, 14: PRINT "  G "; :
    PRINT TAB(20); " W "; :
    PRINT TAB(25); " L "; :
    PRINT TAB(30); " SV "; :
    PRINT TAB(37); " IP "; :
    PRINT TAB(43); " H  "; :
    PRINT TAB(48); " BB "; :
    PRINT TAB(53); " SO "; :
    PRINT TAB(58); " ERA "; :
    PRINT TAB(63); " GS"; :
    PRINT TAB(66); " CG"
    COLOR 10, 0: LOCATE 10, 2: PRINT "REGULAR "
    LOCATE 11, 2: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 12, 4: PRINT STRING$(8, 177)
    COLOR 0, 12: LOCATE 9, 14: PRINT " H/9IP "; :
    PRINT TAB(21); " SO/9IP "; :
    PRINT TAB(29); " BB/9IP "; :
    PRINT TAB(37); " HR ALL"; :
    COLOR 14, 12: LOCATE 5, 14: PRINT USING "###"; P%(I, 4); :
    LOCATE , 17: PRINT USING "#####"; P%(I, 1); :
    LOCATE , 22: PRINT USING "#####"; P%(I, 2); :
    LOCATE , 27: PRINT USING "######"; P%(I, 3); :
    LOCATE , 33: PRINT USING "#####.#"; P%(I, 6); :
    LOCATE , 40: PRINT USING "#####"; P%(I, 7); :
    LOCATE , 45: PRINT USING "######"; P%(I, 8); :
    LOCATE , 51: PRINT USING "#####"; P%(I, 9); :
    LOCATE , 56: PRINT USING "###.##"; (P%(I, 10)) / 100; :
    LOCATE , 62: PRINT USING "####"; P%(I, 5); :
    LOCATE , 66: PRINT USING "###"; P%(I, 16); : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 14, 12: IF P%(I, 6) = 0 THEN LOCATE 10, 14: PRINT "  0.00";
    IF P%(I, 6) > 0 THEN LOCATE 10, 14: PRINT USING "###.##"; (P%(I, 7) * 9) / P%(I, 6)
    IF P%(I, 6) = 0 THEN LOCATE 10, 20: PRINT "    0.00";
    IF P%(I, 6) > 0 THEN LOCATE 10, 20: PRINT USING "#####.##"; (P%(I, 9) * 9) / P%(I, 6)
    IF P%(I, 6) = 0 THEN LOCATE 10, 28: PRINT "    0.00";
    IF P%(I, 6) > 0 THEN LOCATE 10, 28: PRINT USING "#####.##"; (P%(I, 8) * 9) / P%(I, 6)
    LOCATE 10, 36: PRINT USING "########"; P%(I, 13); : COLOR 7, 0: PRINT STRING$(2, 177)
    COLOR 15, 12: LOCATE 6, 14: PRINT USING "###"; P0%(I, 8); :
    LOCATE , 17: PRINT USING "#####"; P0%(I, 6); :
    LOCATE , 22: PRINT USING "#####"; P0%(I, 7); :
    LOCATE , 27: PRINT USING "######"; P0%(I, 12); :
    LOCATE , 33: PRINT USING "#####"; INT(P0%(I, 0) / 3);
    IF P0%(I, 0) - (INT(P0%(I, 0) / 3) * 3) = 0 THEN PRINT ".0";
    IF P0%(I, 0) - (INT(P0%(I, 0) / 3) * 3) > 0 THEN PRINT "."; : PRINT USING "#"; P0%(I, 0) - (INT(P0%(I, 0) / 3) * 3);
    LOCATE , 40: PRINT USING "#####"; P0%(I, 1); :
    LOCATE , 45: PRINT USING "######"; P0%(I, 4); :
    LOCATE , 51: PRINT USING "#####"; P0%(I, 5); :
    I2 = 0
    IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 3) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LOCATE 6, 56: PRINT "  0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LOCATE 6, 56: PRINT USING "###"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LOCATE 6, 56: PRINT USING "###"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN PRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN PRINT "0"; : PRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN PRINT USING "##"; I4;
    LOCATE 6, 62: PRINT USING "####"; P0%(I, 9);
    LOCATE , 66: PRINT USING "###"; P0%(I, 10); : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 7, 16: PRINT STRING$(55, 177)
    COLOR 15, 12: I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 1) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LOCATE 11, 14: PRINT "  0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LOCATE 11, 14: PRINT USING "###"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LOCATE 11, 14: PRINT USING "###"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN PRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN PRINT "0"; : PRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN PRINT USING "##"; I4;
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 5) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LOCATE 11, 20: PRINT "    0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LOCATE 11, 20: PRINT USING "#####"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LOCATE 11, 20: PRINT USING "#####"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN PRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN PRINT "0"; : PRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN PRINT USING "##"; I4;
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 4) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LOCATE 11, 28: PRINT "    0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LOCATE 11, 28: PRINT USING "#####"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LOCATE 11, 28: PRINT USING "#####"; I3; : PRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN PRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN PRINT "0"; : PRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN PRINT USING "##"; I4;
    LOCATE 11, 36: PRINT USING "########"; P0%(I, 14); : COLOR 7, 0: PRINT STRING$(2, 177)
    LOCATE 12, 16: PRINT STRING$(30, 177)
    COLOR 15, 0: LOCATE 23, 4: PRINT "HIT ANY KEY TO CONTINUE"
    IF BI = 1 THEN 503 ELSE 504
503 LOCATE 23, 28: PRINT "OR ESC TO RETURN TO MENU"
504 I$ = INKEY$: IF I$ = "" THEN 504
    IF I$ = CHR$(0) THEN 508
    IF I$ = CHR$(27) THEN 510
    IF BI = 1 THEN 508 ELSE 510
508 NEXT
    IF BI = 1 THEN GOTO 510
510 BI = 0: GOTO 200
550 GOSUB 8106: RUN "HELLO.EXE"
600 LPRINT CHR$(15)
    WIDTH "LPT1:1", 140
    LPRINT "STAT COMPARISONS FOR "; T$
    LPRINT : LPRINT :
    LPRINT "BATTER";
    LPRINT TAB(21); "  G";
    LPRINT TAB(26); " AB";
    LPRINT TAB(31); "  R";
    LPRINT TAB(36); "  H";
    LPRINT TAB(41); " 2B";
    LPRINT TAB(45); " 3B";
    LPRINT TAB(49); " HR";
    LPRINT TAB(54); "RBI";
    LPRINT TAB(59); " BB";
    LPRINT TAB(64); " SO";
    LPRINT TAB(69); " SB";
    LPRINT TAB(74); " CS";
    LPRINT TAB(79); "AVG";
    LPRINT TAB(85); "SLG";
    LPRINT TAB(91); "OBP";
    LPRINT TAB(97); "FLD.PCT"
    FOR I = 0 TO 22
    IF B$(I) = "XXX" OR B$(I) = "END" OR B$(I) = "" THEN 735
    LPRINT LEFT$(B$(I), 14);
    LPRINT TAB(16); "Reg.";
    LPRINT TAB(21); USING "###"; B%(I, 3);
    LPRINT TAB(26); USING "###"; B%(I, 4);
    LPRINT TAB(31); USING "###"; B%(I, 5);
    LPRINT TAB(36); USING "###"; B%(I, 6);
    LPRINT TAB(42); USING "##"; B%(I, 7);
    LPRINT TAB(46); USING "##"; B%(I, 8);
    LPRINT TAB(50); USING "##"; B%(I, 9);
    LPRINT TAB(54); USING "###"; B%(I, 10);
    LPRINT TAB(59); USING "###"; B%(I, 11);
    LPRINT TAB(64); USING "###"; B%(I, 12);
    LPRINT TAB(69); USING "###"; B%(I, 13);
    LPRINT TAB(74); USING "###"; B%(I, 14);
    I2 = 0: I3 = 0: I4 = 0: IF B%(I, 4) > 0 THEN I2 = INT((B%(I, 6) / B%(I, 4) + .0005) * 1000): I3 = INT(((B%(I, 6) + B%(I, 7) + 2 * B%(I, 8) + 3 * B%(I, 9)) / B%(I, 4) + .0005) * 1000): I4 = INT(((B%(I, 6) + B%(I, 11)) / (B%(I, 11) + B%(I, 4)) +  _
.0005) * 1000)
    IF B%(I, 4) = 0 OR I2 = 0 THEN LPRINT TAB(79); ".000";
    IF I2 > 0 AND I2 < 100 THEN LPRINT TAB(79); ".0"; : LPRINT USING "##"; I2;
    IF I2 >= 100 THEN LPRINT TAB(79); "."; : LPRINT USING "###"; I2;
    IF B%(I, 4) = 0 OR I3 = 0 THEN LPRINT TAB(84); ".000";
    IF I3 > 0 AND I3 < 100 THEN LPRINT TAB(84); ".0"; : LPRINT USING "##"; I3;
    IF I3 >= 100 THEN LPRINT TAB(84); "."; : LPRINT USING "###"; I3;
    IF B%(I, 4) = 0 OR I4 = 0 THEN LPRINT TAB(90); ".000";
    IF I4 > 0 AND I4 < 100 THEN LPRINT TAB(90); ".0"; : LPRINT USING "##"; I4;
    IF I4 >= 100 THEN LPRINT TAB(90); "."; : LPRINT USING "###"; I4;
    LPRINT TAB(99); "."; : LPRINT USING "###"; B%(I, 26)
    LPRINT TAB(16); "Lge.";
    LPRINT TAB(21); USING "###"; B0%(I, 14);
    LPRINT TAB(26); USING "###"; B0%(I, 0);
    LPRINT TAB(31); USING "###"; B0%(I, 2);
    LPRINT TAB(36); USING "###"; B0%(I, 1);
    LPRINT TAB(42); USING "##"; B0%(I, 4);
    LPRINT TAB(46); USING "##"; B0%(I, 5);
    LPRINT TAB(50); USING "##"; B0%(I, 6);
    LPRINT TAB(54); USING "###"; B0%(I, 3);
    LPRINT TAB(59); USING "###"; B0%(I, 7);
    LPRINT TAB(64); USING "###"; B0%(I, 8);
    LPRINT TAB(69); USING "###"; B0%(I, 9);
    LPRINT TAB(74); USING "###"; B0%(I, 10);
    I2 = 0: I3 = 0: I4 = 0: IF B0%(I, 0) > 0 THEN I2 = INT((B0%(I, 1) / B0%(I, 0) + .0005) * 1000): I3 = INT(((B0%(I, 1) + B0%(I, 4) + 2 * B0%(I, 5) + 3 * B0%(I, 6)) / B0%(I, 0) + .0005) * 1000): I4 = INT(((B0%(I, 1) + B0%(I, 7)) / (B0%(I, 7) + B0%( _
I, 0)) + .0005) * 1000)
    IF B0%(I, 0) = 0 OR I2 = 0 THEN LPRINT TAB(79); ".000";
    IF I2 > 0 AND I2 < 100 THEN LPRINT TAB(79); ".0"; : LPRINT USING "##"; I2;
    IF I2 >= 100 THEN LPRINT TAB(79); "."; : LPRINT USING "###"; I2;
    IF B0%(I, 0) = 0 OR I3 = 0 THEN LPRINT TAB(84); ".000";
    IF I3 > 0 AND I3 < 100 THEN LPRINT TAB(84); ".0"; : LPRINT USING "##"; I3;
    IF I3 >= 100 THEN LPRINT TAB(84); "."; : LPRINT USING "###"; I3;
    IF B0%(I, 0) = 0 OR I4 = 0 THEN LPRINT TAB(90); ".000";
    IF I4 > 0 AND I4 < 100 THEN LPRINT TAB(90); ".0"; : LPRINT USING "##"; I4;
    IF I4 >= 100 THEN LPRINT TAB(90); "."; : LPRINT USING "###"; I4;
    IF B0%(I, 16) = 0 AND B0%(I, 17) = 0 THEN I5 = 0 ELSE I5 = ((B0%(I, 16) + B0%(I, 17)) / (B0%(I, 16) + B0%(I, 17) + B0%(I, 11))) * 1000
    IF I5 = 0 THEN LPRINT TAB(99); ".000  "
    IF I5 > 0 AND I5 < 1000 THEN LPRINT TAB(99); "."; : LPRINT USING "###"; I5
    IF I5 = 1000 THEN LPRINT TAB(98); USING "#"; I5 / 1000; : LPRINT ".000  "
    LPRINT
735 NEXT I
    GOSUB 7000
    LPRINT CHR$(12)
    LPRINT "STAT COMPARISONS FOR "; T$
    LPRINT : LPRINT :
    LPRINT "PITCHER";
    LPRINT TAB(25); "  G";
    LPRINT TAB(30); "  W";
    LPRINT TAB(35); "  L";
    LPRINT TAB(40); " SV";
    LPRINT TAB(47); " IP";
    LPRINT TAB(52); "  H";
    LPRINT TAB(56); " BB";
    LPRINT TAB(61); " SO";
    LPRINT TAB(67); "ERA";
    LPRINT TAB(72); " GS";
    LPRINT TAB(78); " CG";
    LPRINT TAB(85); "H/9IP";
    LPRINT TAB(92); "SO/9IP";
    LPRINT TAB(99); "BB/9IP";
    LPRINT TAB(107); "HR ALL."
    FOR I = 0 TO 21
    IF P$(I) = "XXX" OR P$(I) = "END" OR P$(I) = "" THEN 983
    LPRINT LEFT$(P$(I), 14);
    LPRINT TAB(18); "REG.";
    LPRINT TAB(25); USING "###"; P%(I, 4);
    LPRINT TAB(31); USING "##"; P%(I, 1);
    LPRINT TAB(36); USING "##"; P%(I, 2);
    LPRINT TAB(41); USING "##"; P%(I, 3);
    LPRINT TAB(45); USING "###.#"; P%(I, 6);
    LPRINT TAB(52); USING "###"; P%(I, 7);
    LPRINT TAB(56); USING "###"; P%(I, 8);
    LPRINT TAB(61); USING "###"; P%(I, 9);
    LPRINT TAB(66); USING "##.##"; (P%(I, 10)) / 100;
    LPRINT TAB(73); USING "##"; P%(I, 5);
    LPRINT TAB(79); USING "##"; P%(I, 16);
    IF P%(I, 6) = 0 THEN LPRINT TAB(85); " 0.00";
    IF P%(I, 6) > 0 THEN LPRINT TAB(85); USING "##.##"; (P%(I, 7) * 9) / P%(I, 6);
    IF P%(I, 6) = 0 THEN LPRINT TAB(93); " 0.00";
    IF P%(I, 6) > 0 THEN LPRINT TAB(93); USING "##.##"; (P%(I, 9) * 9) / P%(I, 6);
    IF P%(I, 6) = 0 THEN LPRINT TAB(100); " 0.00";
    IF P%(I, 6) > 0 THEN LPRINT TAB(100); USING "##.##"; (P%(I, 8) * 9) / P%(I, 6);
    LPRINT TAB(109); USING "##"; P%(I, 13)
    LPRINT TAB(18); "LGE.";
    LPRINT TAB(25); USING "###"; P0%(I, 8);
    LPRINT TAB(31); USING "##"; P0%(I, 6);
    LPRINT TAB(36); USING "##"; P0%(I, 7);
    LPRINT TAB(41); USING "##"; P0%(I, 12);
    LPRINT TAB(45); USING "###"; INT(P0%(I, 0) / 3);
    IF P0%(I, 0) - (INT(P0%(I, 0) / 3) * 3) > 0 THEN LPRINT "."; : LPRINT USING "#"; P0%(I, 0) - (INT(P0%(I, 0) / 3) * 3);
    LPRINT TAB(52); USING "###"; P0%(I, 1);
    LPRINT TAB(56); USING "###"; P0%(I, 4);
    LPRINT TAB(61); USING "###"; P0%(I, 5);
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 3) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LPRINT TAB(66); " 0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LPRINT TAB(66); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LPRINT TAB(66); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN LPRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN LPRINT "0"; : LPRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN LPRINT USING "##"; I4;
    LPRINT TAB(73); USING "##"; P0%(I, 9);
    LPRINT TAB(79); USING "##"; P0%(I, 10);
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 1) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LPRINT TAB(85); " 0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LPRINT TAB(85); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LPRINT TAB(85); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN LPRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN LPRINT "0"; : LPRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN LPRINT USING "##"; I4;
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 5) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LPRINT TAB(93); " 0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LPRINT TAB(93); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LPRINT TAB(93); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN LPRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN LPRINT "0"; : LPRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN LPRINT USING "##"; I4;
    I2 = 0: IF P0%(I, 0) > 0 THEN I2 = INT((((P0%(I, 4) * 9) / (P0%(I, 0) / 3)) + .0005) * 100)
    I3 = INT(I2 / 100): I4 = I2 - (I3 * 100)
    IF P0%(I, 0) = 0 OR I2 = 0 THEN LPRINT TAB(100); " 0.00";
    IF P0%(I, 0) > 0 AND I2 > 0 AND I2 <= 999 THEN LPRINT TAB(100); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I2 >= 1000 THEN LPRINT TAB(100); USING "##"; I3; : LPRINT ".";
    IF P0%(I, 0) > 0 AND I4 = 0 AND I2 > 0 THEN LPRINT "00";
    IF P0%(I, 0) > 0 AND I4 > 0 AND I4 < 10 AND I2 > 0 THEN LPRINT "0"; : LPRINT USING "#"; I4;
    IF P0%(I, 0) > 0 AND I4 >= 10 AND I2 > 0 THEN LPRINT USING "##"; I4;
    LPRINT TAB(109); USING "##"; P0%(I, 14)
    LPRINT
983 NEXT
    LPRINT CHR$(18)
    WIDTH "LPT1:1", 80
1000 GOTO 10
5000 FOR dc = 1 TO 20
     COLOR 15, 0: LOCATE dc, 1
     PRINT STRING$(79, 177)
     NEXT
     RETURN
6000 FOR dc = 1 TO 14
     COLOR 15, 0: LOCATE dc, 1
     PRINT STRING$(78, 177)
     NEXT
     RETURN
7000 LPRINT CHR$(12)
     LPRINT "STAT COMPARISONS FOR "; T$
     LPRINT : LPRINT :
     LPRINT TAB(20); " [ - - - - - - - - - - AT BATS PER - - - - - - - - - - - ]"
     LPRINT "BATTER";
     LPRINT TAB(26); "R";
     LPRINT TAB(33); "H";
     LPRINT TAB(40); "2B";
     LPRINT TAB(47); "3B";
     LPRINT TAB(54); "HR";
     LPRINT TAB(61); "RBI";
     LPRINT TAB(68); "BB";
     LPRINT TAB(75); "SO";
     LPRINT TAB(79); "SUCCESS % - SB"
     FOR I = 0 TO 16
     IF B$(I) = "XXX" OR B$(I) = "END" OR B$(I) = "" THEN 7725
     LPRINT LEFT$(B$(I), 14);
     LPRINT TAB(16); "Reg.";
     IF B%(I, 4) = 0 OR B%(I, 5) = 0 THEN LPRINT TAB(21); "---.--";
     IF B%(I, 4) > 0 AND B%(I, 5) > 0 THEN LPRINT TAB(21); USING "###.##"; B%(I, 4) / B%(I, 5);
     IF B%(I, 4) = 0 OR B%(I, 6) = 0 THEN LPRINT TAB(28); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 6) > 0 THEN LPRINT TAB(28); USING "###.##"; B%(I, 4) / B%(I, 6);
     IF B%(I, 4) = 0 OR B%(I, 7) = 0 THEN LPRINT TAB(36); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 7) > 0 THEN LPRINT TAB(36); USING "###.##"; B%(I, 4) / B%(I, 7);
     IF B%(I, 4) = 0 OR B%(I, 8) = 0 THEN LPRINT TAB(43); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 8) > 0 THEN LPRINT TAB(43); USING "###.##"; B%(I, 4) / B%(I, 8);
     IF B%(I, 4) = 0 OR B%(I, 9) = 0 THEN LPRINT TAB(50); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 9) > 0 THEN LPRINT TAB(50); USING "###.##"; B%(I, 4) / B%(I, 9);
     IF B%(I, 4) = 0 OR B%(I, 10) = 0 THEN LPRINT TAB(58); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 10) > 0 THEN LPRINT TAB(58); USING "###.##"; B%(I, 4) / B%(I, 10);
     IF B%(I, 4) = 0 OR B%(I, 11) = 0 THEN LPRINT TAB(64); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 11) > 0 THEN LPRINT TAB(64); USING "###.##"; B%(I, 4) / B%(I, 11);
     IF B%(I, 4) = 0 OR B%(I, 12) = 0 THEN LPRINT TAB(71); "---.--";
    IF B%(I, 4) > 0 AND B%(I, 12) > 0 THEN LPRINT TAB(71); USING "###.##"; B%(I, 4) / B%(I, 12);
     IF B%(I, 13) + B%(I, 14) = 0 OR B%(I, 13) = 0 THEN LPRINT TAB(83); " 00%";
    IF B%(I, 13) > 0 AND B%(I, 14) = 0 THEN LPRINT TAB(83); "100%";
    IF B%(I, 13) > 0 AND B%(I, 14) > 0 THEN LPRINT TAB(83); USING "###"; (B%(I, 13) / (B%(I, 13) + B%(I, 14))) * 100; : LPRINT TAB(86); "%"
     LPRINT TAB(16); "Leg.";
     IF B0%(I, 0) = 0 OR B0%(I, 2) = 0 THEN LPRINT TAB(21); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 2) > 0 THEN LPRINT TAB(21); USING "###.##"; B0%(I, 0) / B0%(I, 2);
     IF B0%(I, 0) = 0 OR B0%(I, 1) = 0 THEN LPRINT TAB(28); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 1) > 0 THEN LPRINT TAB(28); USING "###.##"; B0%(I, 0) / B0%(I, 1);
     IF B0%(I, 0) = 0 OR B0%(I, 4) = 0 THEN LPRINT TAB(36); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 4) > 0 THEN LPRINT TAB(36); USING "###.##"; B0%(I, 0) / B0%(I, 4);
     IF B0%(I, 0) = 0 OR B0%(I, 5) = 0 THEN LPRINT TAB(43); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 5) > 0 THEN LPRINT TAB(43); USING "###.##"; B0%(I, 0) / B0%(I, 5);
     IF B0%(I, 0) = 0 OR B0%(I, 6) = 0 THEN LPRINT TAB(50); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 6) > 0 THEN LPRINT TAB(50); USING "###.##"; B0%(I, 0) / B0%(I, 6);
     IF B0%(I, 0) = 0 OR B0%(I, 3) = 0 THEN LPRINT TAB(58); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 3) > 0 THEN LPRINT TAB(58); USING "###.##"; B0%(I, 0) / B0%(I, 3);
     IF B0%(I, 0) = 0 OR B0%(I, 7) = 0 THEN LPRINT TAB(64); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 7) > 0 THEN LPRINT TAB(64); USING "###.##"; B0%(I, 0) / B0%(I, 7);
     IF B0%(I, 0) = 0 OR B0%(I, 8) = 0 THEN LPRINT TAB(71); "---.--";
    IF B0%(I, 0) > 0 AND B0%(I, 8) > 0 THEN LPRINT TAB(71); USING "###.##"; B0%(I, 0) / B0%(I, 8);
     IF B0%(I, 9) + B0%(I, 10) = 0 OR B0%(I, 9) = 0 THEN LPRINT TAB(83); " 00%"
    IF B0%(I, 9) > 0 AND B0%(I, 10) = 0 THEN LPRINT TAB(83); "100%"
    IF B0%(I, 9) > 0 AND B0%(I, 10) > 0 THEN LPRINT TAB(83); USING "###"; (B0%(I, 9) / (B0%(I, 10) + B0%(I, 9))) * 100; : LPRINT TAB(86); "%"
7725 NEXT
     RETURN
8000 LOCATE 13, 20: COLOR 14: PRINT "ERROR::: INVALID TEAM # OR STAT FILE NAME!!"
     LOCATE 15, 20: LINE INPUT "HIT ENTER TO RE-ENTER"; DUMMY$
     FOR J = 5 TO 15
     LOCATE J
     PRINT "                                                                         "
     NEXT
     RETURN
8106 IF DRIVE >= 67 THEN RETURN
     CLS : COLOR 7: PRINT "MAKE SURE GAME DISK IS IN DRIVE"
     LOCATE 23: PRINT "HIT ANY KEY TO CONTINUE"
8108 I$ = INKEY$: IF I$ = "" THEN 8108
     RETURN
9000 FOR K = 4 TO 18: FOR I = 0 TO 22
     TB%(K - 3) = TB%(K - 3) + B%(I, K)
     NEXT: NEXT
     FOR K = 1 TO 33
     FOR I = 0 TO 21
     TP(K) = TP(K) + P%(I, K)
     NEXT: NEXT
     ERX = 0: FOR I = 0 TO 21
     ERX = ERX + ((P%(I, 6) / 9) * (P%(I, 10) / 100))
     NEXT
     FOR K = 4 TO 14: FOR I = 0 TO 21
     IF P%(I,11)=999 THEN TB%(K - 3) = TB%(K - 3) + P%(I, K+16)
     NEXT: NEXT
     RETURN
9200 CLS : GOSUB 9700: LOCATE 1, 35: COLOR L%(11), L%(12): PRINT T$
     COLOR 10, 0: LOCATE 3, 2: PRINT "BATTING "
     COLOR 12: LOCATE 4, 2: PRINT "REGULAR "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 5, 2: COLOR 12: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 6, 4: PRINT STRING$(8, 177)
     LOCATE 3, 13: COLOR 0, 11: PRINT "  AB"; :
     PRINT TAB(18); "  R"; :
     PRINT TAB(23); "  H"; :
     PRINT TAB(27); " 2B"; :
     PRINT TAB(31); " 3B"; :
     PRINT TAB(35); " HR"; :
     PRINT TAB(39); " RBI"; :
     PRINT TAB(43); " BB"; :
     PRINT TAB(49); " SO"; :
     PRINT TAB(53); " SB"; :
     PRINT TAB(57); " CS"; :
     PRINT TAB(61); " AVG"; :
     PRINT TAB(66); " SLG"; :
     PRINT TAB(72); "OBP"; :
     COLOR 10, 0: LOCATE 9, 2: PRINT "BATTING "
     COLOR 12, 0: LOCATE 10, 2: PRINT "REGULAR "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 11, 2: COLOR 12: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 12, 4: PRINT STRING$(8, 177)
     COLOR 0, 11: LOCATE 8, 13: PRINT "  - - - - - - - - - - AT BATS PER - - - - - - - - - - - SUCCESS"
     LOCATE 9, 13: PRINT "  RUNS"; :
     PRINT TAB(21); " HITS"; :
     PRINT TAB(30); " 2B "; :
     PRINT TAB(37); " 3B "; :
     PRINT TAB(44); " HR "; :
     PRINT TAB(50); " RBI "; :
     PRINT TAB(58); " BB "; :
     PRINT TAB(65); " SO "; :
     PRINT TAB(70); " %-SB "; : COLOR 7, 0: PRINT STRING$(2, 177)
     COLOR 1, 11: LOCATE 4, 13: PRINT USING "####"; TB%(1);
     LOCATE , 17: PRINT USING "####"; TB%(2);
     LOCATE , 21: PRINT USING "#####"; TB%(3);
     LOCATE , 26: PRINT USING "####"; TB%(4);
     LOCATE , 30: PRINT USING "####"; TB%(5);
     LOCATE , 34: PRINT USING "####"; TB%(6);
     LOCATE , 38: PRINT USING "#####"; TB%(7);
     LOCATE , 43: PRINT USING "####"; TB%(8);
     LOCATE , 47: PRINT USING "#####"; TB%(9);
     LOCATE , 52: PRINT USING "####"; TB%(10);
     LOCATE , 56: PRINT USING "####"; TB%(11); : PRINT " ";
     LOCATE , 61: PRINT USING ".###"; TB%(3) / TB%(1); : PRINT " ";
     LOCATE , 66: PRINT USING ".###"; (TB%(3) + TB%(4) + TB%(5) * 2 + TB%(6) * 3) / TB%(1); : PRINT " ";
     LOCATE , 71: PRINT USING ".###"; (TB%(3) + TB%(8)) / (TB%(1) + TB%(8)); : COLOR 7, 0: PRINT STRING$(2, 177)
     COLOR 14, 11: LOCATE 5, 13: PRINT USING "####"; T(0);
     LOCATE , 17: PRINT USING "####"; T(2);
     LOCATE , 21: PRINT USING "#####"; T(1);
     LOCATE , 26: PRINT USING "####"; T(4);
     LOCATE , 30: PRINT USING "####"; T(5);
     LOCATE , 34: PRINT USING "####"; T(6);
     LOCATE , 38: PRINT USING "#####"; T(3);
     LOCATE , 43: PRINT USING "####"; T(7);
     LOCATE , 47: PRINT USING "#####"; T(8);
     LOCATE , 52: PRINT USING "####"; T(9);
     LOCATE , 56: PRINT USING "####"; T(10); : PRINT " ";
     LOCATE , 61: PRINT USING ".###"; T(1) / T(0); : PRINT " ";
     LOCATE , 66: PRINT USING ".###"; (T(1) + T(4) + T(5) * 2 + T(6) * 3) / T(0); : PRINT " ";
     LOCATE , 71: PRINT USING ".###"; (T(1) + T(7)) / (T(0) + T(7)); : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE , 15: PRINT STRING$(62, 177)
     COLOR 1, 11
     IF TB%(2) = 0 THEN LOCATE 10, 13: PRINT " ---.--";
     IF TB%(2) > 0 THEN LOCATE 10, 13: PRINT USING "###.##"; TB%(1) / TB%(2);
     IF TB%(3) = 0 THEN LOCATE , 18: PRINT " ---.--";
     IF TB%(3) > 0 THEN LOCATE , 18: PRINT USING "#####.##"; TB%(1) / TB%(3);
     IF TB%(4) = 0 THEN LOCATE , 26: PRINT " ---.--";
     IF TB%(4) > 0 THEN LOCATE , 26: PRINT USING "####.##"; TB%(1) / TB%(4);
     IF TB%(5) = 0 THEN LOCATE , 33: PRINT " ---.--";
     IF TB%(5) > 0 THEN LOCATE , 33: PRINT USING "####.##"; TB%(1) / TB%(5);
     IF TB%(6) = 0 THEN LOCATE , 40: PRINT " ---.--";
     IF TB%(6) > 0 THEN LOCATE , 40: PRINT USING "####.##"; TB%(1) / TB%(6);
     IF TB%(7) = 0 THEN LOCATE , 47: PRINT " ---.--";
     IF TB%(7) > 0 THEN LOCATE , 47: PRINT USING "####.##"; TB%(1) / TB%(7);
     IF TB%(8) = 0 THEN LOCATE , 54: PRINT " ---.--";
     IF TB%(8) > 0 THEN LOCATE , 54: PRINT USING "####.##"; TB%(1) / TB%(8);
     IF TB%(9) = 0 THEN LOCATE , 61: PRINT " ---.--";
     IF TB%(9) > 0 THEN LOCATE , 61: PRINT USING "####.##"; TB%(1) / TB%(9);
     IF TB%(10) = 0 THEN LOCATE , 68: PRINT " ---.--";
     IF TB%(10) > 0 THEN LOCATE , 68: PRINT USING "####.##"; TB%(10) / (TB%(10) + TB%(11)); : PRINT " "; : COLOR 7, 0: PRINT STRING$(2, 177)
     COLOR 14, 11
     IF T(2) = 0 THEN LOCATE 11, 13: PRINT " ---.--";
     IF T(2) > 0 THEN LOCATE 11, 13: PRINT USING "###.##"; T(0) / T(2);
     IF T(1) = 0 THEN LOCATE , 18: PRINT " ---.--";
     IF T(1) > 0 THEN LOCATE , 18: PRINT USING "#####.##"; T(0) / T(1);
     IF T(4) = 0 THEN LOCATE , 26: PRINT " ---.--";
     IF T(4) > 0 THEN LOCATE , 26: PRINT USING "####.##"; T(0) / T(4);
     IF T(5) = 0 THEN LOCATE , 33: PRINT " ---.--";
     IF T(5) > 0 THEN LOCATE , 33: PRINT USING "####.##"; T(0) / T(5);
     IF T(6) = 0 THEN LOCATE , 40: PRINT " ---.--";
     IF T(6) > 0 THEN LOCATE , 40: PRINT USING "####.##"; T(0) / T(6);
     IF T(3) = 0 THEN LOCATE , 47: PRINT " ---.--";
     IF T(3) > 0 THEN LOCATE , 47: PRINT USING "####.##"; T(0) / T(3);
     IF T(7) = 0 THEN LOCATE , 54: PRINT " ---.--";
     IF T(7) > 0 THEN LOCATE , 54: PRINT USING "####.##"; T(0) / T(7);
     IF T(8) = 0 THEN LOCATE , 61: PRINT " ---.--";
     IF T(8) > 0 THEN LOCATE , 61: PRINT USING "####.##"; T(0) / T(8);
     IF T(9) = 0 THEN LOCATE , 68: PRINT " ---.--";
     IF T(9) > 0 THEN LOCATE , 68: PRINT USING "####.##"; T(9) / (T(9) + T(10)); : PRINT " "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE , 15: PRINT STRING$(63, 177)
     COLOR 10, 0: LOCATE 14, 2: PRINT "PITCHING"
     COLOR 12, 0: LOCATE 15, 2: PRINT "REGULAR "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 16, 2: COLOR 12: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 17, 4: PRINT STRING$(8, 177)
     COLOR 0, 12: LOCATE 14, 14: PRINT "  SV";
     PRINT TAB(20); " IP";
     PRINT TAB(25); "  H";
     PRINT TAB(30); "  BB";
     PRINT TAB(37); "  SO";
     PRINT TAB(43); " HRA";
     PRINT TAB(48); "  CG";
     PRINT TAB(53); "  ERA";
     COLOR 10, 0: LOCATE 19, 2: PRINT "PITCHING"
     COLOR 12, 0: LOCATE 20, 2: PRINT "REGULAR "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 21, 2: COLOR 12: PRINT "LEAGUE  "; : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE 22, 4: PRINT STRING$(8, 177)
     COLOR 0, 12: LOCATE 19, 14: PRINT " H/9IP ";
     PRINT TAB(21); " SO/9IP ";
     PRINT TAB(29); " BB/9IP";
     COLOR 15, 12: LOCATE 15, 14: PRINT USING "####"; TP(3);
     LOCATE , 18: PRINT USING "#####"; TP(6);
     LOCATE , 23: PRINT USING "#####"; TP(7);
     LOCATE , 28: PRINT USING "######"; TP(8);
     LOCATE , 34: PRINT USING "#######"; TP(9);
     LOCATE , 41: PRINT USING "######"; TP(13);
     LOCATE , 47: PRINT USING "#####"; TP(16);
     LOCATE , 52: PRINT USING "###.##"; (ERX * 9) / TP(6); : COLOR 7, 0: PRINT STRING$(2, 177)
     COLOR 15, 12: LOCATE 20, 14: PRINT USING "###.##"; TP(7) * 9 / TP(6);
     LOCATE , 20: PRINT USING "#####.##"; TP(9) * 9 / TP(6);
     LOCATE , 28: PRINT USING "#####.##"; TP(8) * 9 / TP(6); : COLOR 7, 0: PRINT STRING$(2, 177)
     COLOR 14, 12: LOCATE 16, 14: PRINT USING "####"; T1(12);
     LOCATE , 18: PRINT USING "#####"; T1(0) / 3;
     LOCATE , 23: PRINT USING "#####"; T1(1);
     LOCATE , 28: PRINT USING "######"; T1(4);
     LOCATE , 34: PRINT USING "#######"; T1(5);
     LOCATE , 41: PRINT USING "######"; T1(14);
     LOCATE , 47: PRINT USING "#####"; T1(10);
     LOCATE , 52: PRINT USING "###.##"; (T1(3) * 9) / (T1(0) / 3); : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE , 16: COLOR 7, 0: PRINT STRING$(44, 177)
     COLOR 14, 12: LOCATE 21, 14: PRINT USING "###.##"; T1(1) * 9 / (T1(0) / 3);
     LOCATE , 20: PRINT USING "#####.##"; T1(5) * 9 / (T1(0) / 3);
     LOCATE , 28: PRINT USING "#####.##"; T1(4) * 9 / (T1(0) / 3); : COLOR 7, 0: PRINT STRING$(2, 177)
     LOCATE , 16: COLOR 7, 0: PRINT STRING$(22, 177)
     COLOR 15, 0: LOCATE 22, 48: PRINT "HIT ANY KEY TO CONTINUE"
     FOR I = 1 TO 15: TB%(I) = 0: NEXT
     FOR I = 1 TO 33: TP(I) = 0: NEXT
9500 I$ = INKEY$: IF I$ = "" THEN 9500
     RETURN
9700 FOR dc = 1 TO 23
     COLOR 15, 0: LOCATE dc, 1
     PRINT STRING$(79, 177)
     NEXT
     RETURN
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

