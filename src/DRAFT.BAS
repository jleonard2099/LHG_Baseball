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
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
  DIM L%(13), L1%(13), B%(22, 79), B1%(22, 79), P%(21, 88), P1%(21, 88)
  DIM SA%(24), SA1%(24)
  DIM TS%(11), TS1%(11), YN$(5)
  DIM B$(22), B1$(22), P$(21), P1$(21), Q$(2056), VV$(200), V%(200), VV%(200), V$(200)
  DIM K9(1) AS LONG, E6 AS LONG
  DIM D$(2), Y AS LONG
  OPEN "DEV.BAS" FOR INPUT AS #1
  FOR I = 0 TO 2
  INPUT #1, D$(I)
  NEXT I
  CLOSE #1
10 CLS
   FOR I = 0 TO 22
   B$(I) = "XXX"
   B1$(I) = "XXX"
   FOR I1 = 0 TO 79
   B%(I, I1) = 0
   B1%(I, I1) = 0
   NEXT: NEXT
   FOR I = 0 TO 21
   P$(I) = "XXX"
   P1$(I) = "XXX"
   FOR I1 = 0 TO 88
   P%(I, I1) = 0
   P1%(I, I1) = 0
   NEXT: NEXT
   FOR I = 0 TO 11
   TS%(I) = 0
   TS1%(I) = 0
   NEXT
   FOR I = 1 TO 24
   SA%(I) = 0
   SA1%(I) = 0
   NEXT

12 CLS : COLOR 12, 0: PRINT TAB(31); "DRAFT TEAM UTILITY": PRINT
   COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "DRAFT A NEW TEAM": PRINT
   COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "DRAFT TO AN EXISTING TEAM": PRINT
   COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "RETURN TO GAME MENU"
13 I$ = INKEY$: IF I$ = "" THEN 13
14 I = VAL(I$): IF I < 0 OR I > 2 THEN 13
15 IF I = 0 THEN DNT% = 0: GOTO 305
16 IF I = 2 THEN 1009
17 IF I = 1 THEN DNT% = 1: GOTO 2000
305 CLS : PRINT "INPUT TEAM INFORMATION"
315 LOCATE 2: INPUT "1. DISK ID "; YN$(0): IF C1 = 1 THEN 415
325 LOCATE 3: INPUT "2. TEAM NAME "; T$: IF C1 = 1 THEN 415
335 LOCATE 4: INPUT "3. LGE BA (WHL #) "; L%(1): IF C1 = 1 THEN 415
345 LOCATE 5: INPUT "4. LGE SO % (WHL #) "; L%(2): IF C1 = 1 THEN 415
355 LOCATE 6: INPUT "5. LGE BB % (WHL #) "; L%(3): IF C1 = 1 THEN 415
365 LOCATE 7: INPUT "6. LGE HR % (WHL #) "; L%(4): IF C1 = 1 THEN 415
375 LOCATE 8: INPUT "7. TM DP/G X 10 "; L%(5): IF C1 = 1 THEN 415
385 LOCATE 9: INPUT "8. PARK DOUBLE ADJUSTMENT (WHL #) "; L%(6): IF C1 = 1 THEN 415
395 LOCATE 10: INPUT "9. PARK TRIPLE ADJUSTMENT (WHL #) "; L%(7): IF C1 = 1 THEN 415
405 LOCATE 11: INPUT "10. PARK HOME RUN ADJUSTMENT (WHL #) "; K9(0): IF C1 = 1 THEN 415
406 LOCATE 12: INPUT "11. PARK FOUL GROUND ADJUSTMENT (WHL #) "; L%(8): IF C1 = 1 THEN 415
407 LOCATE 13: INPUT "12. OUTDOOR = 0 / INDOOR = 1 "; L%(9): IF C1 = 1 THEN 415
408 LOCATE 14: INPUT "13. GRASS = 0 / TURF = 1 "; L%(10): IF C1 = 1 THEN 415
409 LOCATE 15: INPUT "14. STADIUM NAME "; S$: IF C1 = 1 THEN 415
410 LOCATE 16: INPUT "15. PARK ID NUMBER "; L%(13): IF C1 = 1 THEN 415
411 LOCATE 17: INPUT "16. MANAGER NAME "; M$: IF C1 = 1 THEN 415
412 LOCATE 18: INPUT "17. TEAM TEXT COLOR (0-15) "; L%(11): IF C1 = 1 THEN 415
413 LOCATE 19: INPUT "18. TEAM BACKGROUND (0-8) "; L%(12): IF C1 = 1 THEN 415
414 LOCATE 20: INPUT "19. TEAM ABBREVIATION "; NN$: IF C1 = 1 THEN 415
415 LOCATE 21, 1: COLOR L%(11), L%(12): PRINT "SAMPLE TEXT APPEARANCE": COLOR 15, 0
    C1 = 1: LOCATE 22: INPUT "# TO CHANGE (0 TO CONTINUE) "; B9
416 IF B9 < 0 OR B9 > 19 THEN 415
418 ON B9 GOTO 315, 325, 335, 345, 355, 365, 375, 385, 395, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414
41 C1 = 0
50 CLS : COLOR L%(11), L%(12): PRINT T$; : COLOR 15, 0: PRINT "     (DRAFTING)"
51 PRINT : PRINT "BATTERS"; : LOCATE , 40: PRINT "PITCHERS"
   FOR I = 0 TO 10
   LOCATE I + 5, 1: PRINT USING "##"; I; : PRINT " "; B$(I); TAB(20); : PRINT USING "##"; I + 12; : PRINT " "; B$(I + 12)
   NEXT
   LOCATE , 1: PRINT USING "##"; 11; : PRINT " "; B$(11)
   PRINT
   FOR I = 0 TO 10
   LOCATE I + 5, 40: PRINT USING "##"; I; : PRINT " "; P$(I); TAB(60); : PRINT USING "##"; I + 11; : PRINT " "; P$(I + 11)
   NEXT
54 LOCATE 18, 1
   COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "DRAFT BATTER"
   COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "DRAFT PITCHER"
   COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "SAVE DRAFT"
55 I$ = INKEY$: IF I$ = "" THEN 55
   I1 = VAL(I$): IF I1 < 1 OR I1 > 3 THEN 55
56 IF I1 = 3 THEN 1000
57 IF I1 = 1 THEN 70
58 LOCATE 22, 40: PRINT "PITCHER # (0-21) ";
59 N = 2: INPUT P
60 IF P < 0 OR P > 21 THEN 58
61 GOTO 100
70 LOCATE 21, 1: PRINT "BATTER # (0-22) ";
71 N = 1: INPUT B
72 IF B < 0 OR B > 22 THEN 70
100 COLOR 15, 0
    CLS
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
    YN$(1) = NY$
    FOR I = 1 TO 200: VV$(I) = "": V%(I) = 1: NEXT
    GOSUB 1100
182 IF BO% = 1 THEN BO% = 0: GOTO 100
183 LOCATE 2, 33: COLOR L1%(11), L1%(12): PRINT T1$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 100
    COLOR 15, 0
    GOTO 150

105 OPEN D$(0) + "FCTEAMS." + YN$(1) FOR RANDOM AS #1 LEN = 4342
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
GET #1, D
     T1$ = QPRTrim$(Q$(0))
     FOR I = 1 TO 13: L1%(I) = CVI(Q$(I)): NEXT: E6 = CVS(Q$(14))
     FOR I = 0 TO 22: B1$(I) = QPRTrim$(Q$(I * 43 + 15))
     FOR I1 = 0 TO 41: B1%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
     NEXT: NEXT
     FOR I = 0 TO 21: P1$(I) = QPRTrim$(Q$(I * 38 + 1004))
     FOR I1 = 0 TO 36: P1%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
     NEXT: NEXT
     S1$ = QPRTrim$(Q$(1840))
     M1$ = QPRTrim$(Q$(1841))
     NN1$ = QPRTrim$(Q$(1842))
     CLOSE 1
     IF NOT EXIST%(D$(0) + "LR." + YN$(1)) THEN 110
     OPEN D$(0) + "LR." + YN$(1) FOR RANDOM AS #1 LEN = 4108
     FOR X = 0 TO 11
     FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 4108 - 2 - 2 * X AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 14
     FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 1 TO 24
     FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 24
     FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 24
     FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
     NEXT
     NEXT
     GET #1, D
     FOR X = 0 TO 11
     TS1%(X) = CVI(Q$(X + 1))
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 14
     B1%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
     NEXT: NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     P1%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
     NEXT: NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     P1%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
     NEXT: NEXT
     FOR X = 1 TO 24
     SA1%(X) = CVI(Q$(X + 950))
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 24
     B1%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
     NEXT: NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 24
     P1%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
     NEXT: NEXT
     CLOSE 1
     IF TS1%(0) = 999 THEN 110
     FOR X = 0 TO 22
     FOR X1 = 42 TO 79
     B1%(X, X1) = 0
     NEXT: NEXT
     FOR X = 0 TO 21
     FOR X1 = 37 TO 88
     P1%(X, X1) = 0
     NEXT: NEXT
     FOR X = 1 TO 24
     SA1%(X) = 0
     NEXT
     FOR X = 0 TO 11
     TS1%(X) = 0
     NEXT
110 RETURN


150 CLS : COLOR L1%(11), L1%(12): PRINT T1$: COLOR 15, 0
151 PRINT : COLOR 14: PRINT "BATTERS"; : LOCATE , 40: PRINT "PITCHERS": COLOR 15, 0
    FOR I = 0 TO 10
    LOCATE I + 5, 1: PRINT USING "##"; I; : PRINT " "; B1$(I); TAB(20); : PRINT USING "##"; I + 12; : PRINT " "; B1$(I + 12)
    NEXT
    LOCATE , 1: PRINT USING "##"; 11; : PRINT " "; B1$(11)
    PRINT
    FOR I = 0 TO 10
    LOCATE I + 5, 40: PRINT USING "##"; I; : PRINT " "; P1$(I); TAB(60); : PRINT USING "##"; I + 11; : PRINT " "; P1$(I + 11)
    NEXT
154 IF N = 1 THEN 160
155 LOCATE 21, 40: PRINT "PITCHER # TO DRAFT ";
156 INPUT P1
157 IF P1 < 0 OR P1 > 21 THEN 155
158 GOTO 200
160 LOCATE 21, 1: PRINT "BATTER # TO DRAFT ";
161 INPUT B1
162 IF B1 < 0 OR B1 > 22 THEN 160
171 B$(B) = B1$(B1)
172 FOR I = 0 TO 79: B%(B, I) = B1%(B1, I): NEXT I
    IF TS%(0) = 999 THEN 174
    IF TS1%(0) <> 999 THEN 174
    FOR I = 0 TO 11
    TS%(I) = TS1%(I)
    NEXT
    FOR I = 1 TO 24
    SA%(I) = SA1%(I)
    NEXT
174 GOTO 50
200 P$(P) = P1$(P1)
201 FOR I = 0 TO 88: P%(P, I) = P1%(P1, I): NEXT I
    IF TS%(0) = 999 THEN 203
    IF TS1%(0) <> 999 THEN 203
    FOR I = 0 TO 11
    TS%(I) = TS1%(I)
    NEXT
    FOR I = 1 TO 24
    SA%(I) = SA1%(I)
    NEXT
203 GOTO 50

1000 GOSUB 1050
     OPEN D$(0) + "FCTEAMS." + YN$(0) FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     IF DNT% = 0 THEN T = Y / 4342 + 1
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

1386 SCK = 0
     FOR X = 1 TO 24
     SCK = SCK + SA%(X)
     NEXT
     IF SCK <= 0 THEN 1006
     TS%(0) = 999
     FOR X = 1 TO 11
     TS%(X) = -1
     NEXT
     OPEN D$(0) + "LR." + YN$(0) FOR RANDOM AS #1 LEN = 4108
     FOR X = 0 TO 11
     FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 4108 - 2 - 2 * X AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 14
     FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 1 TO 24
     FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 24
     FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 24
     FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
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

1006 CLS
     GOSUB 4500
     U$ = "DRAFT ANOTHER TEAM (YN)"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1009 I$ = INKEY$: IF I$ = "" THEN 1009
1010 IF UCASE$(I$) = "Y" THEN 10
1011 CLS
     GOSUB 4500
     U$ = "INSERT GAME DISK IN DRIVE "
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     IF DRIVE >= 67 THEN 1013
     U$ = "HIT ANY KEY TO CONTINUE"
     LOCATE , (80 - LEN(U$)) / 2
     PRINT U$
     COLOR 15, 0
1012 I$ = INKEY$: IF I$ = "" THEN 1012
1013 RUN "HELLO.EXE"
     END
1050 IF UCASE$(LEFT$(D$(0), 1)) = "C" THEN 1055
     CLS : PRINT "INSERT DISK TO SAVE TEAM ON IN DRIVE "; D$(0)
     LOCATE 23, 1: PRINT "HIT ANY KEY TO CONTINUE"
1051 I$ = INKEY$: IF I$ = "" THEN 1051
1055 RETURN
4500 JJ = 2
     LOCATE 6, 8: COLOR 15, 1
     PRINT " "; CHR$(201); STRING$(61, 205); CHR$(187); " ";
     FOR II = 1 TO JJ: LOCATE 6 + II, 8
     PRINT " º"; STRING$(61, 32); "º "; : NEXT
     LOCATE 7 + JJ, 8
     PRINT " "; CHR$(200); STRING$(61, 205); CHR$(188); " "; : LOCATE 7
     RETURN
1100 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$(1)) THEN BO% = 1: GOSUB 4500: RETURN
     OPEN D$(0) + "FCTEAMS." + YN$(1) FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0)
     FOR I1 = 1 TO Y / 4342
     GET #1, I1
     FOR I2 = 1 TO 15
     IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1103
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
     D = V%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
     GOTO 105
1200 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$(0)) THEN BO% = 1: GOSUB 4500: RETURN
     OPEN D$(0) + "FCTEAMS." + YN$(0) FOR RANDOM AS #1 LEN = 4342
     Y = LOF(1)
     FIELD #1, 15 AS Q$(0)
     FOR I1 = 1 TO Y / 4342
     GET #1, I1
     FOR I2 = 1 TO 15
     IF LEFT$(Q$(0),2) = "XX" OR LEFT$(Q$(0),1) = " " OR ASC(MID$(Q$(0), I2, 1)) < 32 OR ASC(MID$(Q$(0), I2, 1)) > 123 OR UCASE$(LEFT$(Q$(0), 3)) = "NOT" OR UCASE$(LEFT$(Q$(0), 3)) = "UN" THEN 1203
     NEXT I2
     NT = NT + 1
     VV$(NT) = Q$(0): V%(NT) = I1
1203 NEXT I1
     CLOSE 1
1202 SELECTION% = 1
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
     GOTO 2500

2000 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE 7, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1202: GOTO 2001
    YN$(0) = NY$
    FOR I = 1 TO 200: V$(I) = "": NEXT
    GOSUB 1200
2001 IF BO% = 1 THEN BO% = 0: GOTO 2000
2002 LOCATE 2, 33: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 2002
    IF UCASE$(I$) = "Y" THEN 2000
    COLOR 15, 0
    GOTO 50

2500 OPEN D$(0) + "FCTEAMS." + YN$(0) FOR RANDOM AS #1 LEN = 4342
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
   T$ = QPRTrim$(Q$(0))
   FOR I = 1 TO 13
   L%(I) = CVI(Q$(I))
   NEXT
   K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = QPRTrim$(Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = QPRTrim$(Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = QPRTrim$(Q$(1840))
M$ = QPRTrim$(Q$(1841))
NN$ = QPRTrim$(Q$(1842))
CK = CVI(Q$(1843))
CLOSE 1
     IF NOT EXIST%(D$(0) + "LR." + YN$(0)) THEN 4000
     OPEN D$(0) + "LR." + YN$(0) FOR RANDOM AS #1 LEN = 4108
     FOR X = 0 TO 11
     FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 4108 - 2 - 2 * X AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 14
     FIELD #1, 24 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 668 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 14
     FIELD #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 AS X$, 2 AS Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) AS X$
     NEXT
     NEXT
     FOR X = 1 TO 24
     FIELD #1, 1900 + (X - 1) * 2 AS X$, 2 AS Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 AS X$
     NEXT
     FOR X = 0 TO 22
     FOR X1 = 1 TO 24
     FIELD #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) AS X$
     NEXT
     NEXT
     FOR X = 0 TO 21
     FOR X1 = 1 TO 24
     FIELD #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 AS X$, 2 AS Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) AS X$
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
     IF TS%(0) = 999 THEN 4000
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
4000 RETURN

RETURN

