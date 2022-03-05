DEFINT A-Z
DIM U6 AS SINGLE
REM: STILL HAVE TO TAKE OLD FORMAT AND WRITE TO NEW FORMAT....HAVE REMOVED
REM: NEED FOR TEAM ABBREV....FIND WAY TO PUT MO AND DATE IN INTEGER VARIABLES
REM: POSSIBLY STORE EACH PART OF DATE IN 3 FIELDS ON INTEGER 6 VS 8 BYTES
REM: TIME SAME WAY, CONVERT TO MILITARY EQUIVAKLENT FOR STORAGE PURPOSES
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION Monitor% ()    'determines monitor type
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE SUB KILLFILE (FILENAME$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB TITLE ()
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
'$INCLUDE: 'QEditype.BI'
DIM Ed AS EditInfo              'create the editor's data structure
DECLARE SUB QEdit (array$(), Ky$, Action, Ed AS EditInfo)
   DIM VV$(200), V%(200), TS$(1), MO$(3), U5$(4), TE$(2), CM$(1), PU$(48)
   DIM Q$(377), AP%(1), T(34), FT$(1), FR$(1), T$(0), Z1$(40), Z1%(40), Z2$(40)
   DIM H$(20), V$(20), HS%(1300), VS%(1300), L%(13), D$(2), WE$(1), E%(13)
   DIM Y AS LONG, ITEM$(8)
   DIM Y1 AS SINGLE
   COMMON SHARED VV$(), V%(), TS$(), DY(), Q$()
   COMMON SHARED YN$(), NG%(), HS%(), VS%()

    F2$ = CHR$(0) + CHR$(60)        'extended code for the F2 function key
                                    '(used in this demo to change margins)
    F3$ = CHR$(0) + CHR$(61)

   '$INCLUDE: 'DEFCNF.BI'
   '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
   DATA HH,C1,C2,CC: FOR X = 0 TO 3: READ MO$(X): NEXT
   DATA 15,0,15,1,19,4,4,6,3,1,2,3,14
   FOR X = 1 TO 13: READ E%(X): NEXT
10 REDIM DY(1300)
   REDIM NG%(1300, 18)
   REDIM YN$(1300, 1)
   OPEN "DEV.BAS" FOR INPUT AS #1: FOR I = 0 TO 2: INPUT #1, D$(I): NEXT: CLOSE 1
   DRIVE = GetDrive%

16 COLOR 7, 0: CLS
   NG%(0, 8) = 0
   NG%(0, 3) = 1
   NG%(0, 7) = 0
   NG%(0, 2) = 1
   NG%(0, 14) = 4
   DT$ = "04-04-93"
   TM$ = "07:30P"

   TS$(0) = "N": TS$(1) = "Y"

   U5$(0) = "HUMAN OPPONENT             "
   U5$(1) = "COMPUTER IS VISITING TEAM  "
   U5$(2) = "COMPUTER IS HOME TEAM      "
   U5$(3) = "COMPUTER VS COMPUTER       "
   PU$(0) = "1980 - PRESENT"
   PU$(1) = "1973 - 1979   "
   PU$(2) = "1955 - 1972   "
   PU$(3) = "1946 - 1954   "
   PU$(4) = "1933 - 1945   "
   PU$(5) = "1919 - 1932   "
   PU$(6) = "1909 - 1918   "
   PU$(7) = "1894 - 1908   "
   PU$(8) = "1876 - 1893   "

11  CALL TITLE

     LOCATE 16: PRINT : COLOR 2
     PRINT "GENERATING A STANDARD SEASON SCHEDULE IS TO BE USED IN CONJUNCTION"
     PRINT "WITH THE STANDINGS AND LEAGUE LEADERS OPTIONS. YOU MUST ALREADY HAVE"
     PRINT "CREATED AN APPROPRIATE LEAGUE FOR THIS OPTION TO PROPERLY FUNCTION."

   ITEM$(1) = "ENTER A NEW SCHEDULE                             "
   ITEM$(2) = "RETRIEVE/EDIT/PLAY AN EXISTING SCHEDULE          "
   ITEM$(3) = "EXTRACT ONE TEAM'S SCHEDULE FROM LEAGUE SCHEDULE "
   ITEM$(4) = "CHANGE THE MODE OF PLAY FOR A TEAM IN A SCHEDULE "
   ITEM$(5) = "MAKE GLOBAL CHANGES TO A SCHEDULE                "
   ITEM$(6) = "GENERATE A STANDARD SEASON SCHEDULE              "
   ITEM$(7) = "BOXSCORE MAINTENANCE / SAVE BOXSCORE PARAMETERS  "
   ITEM$(8) = "EXIT AUTO SCHEDULER TO FULL COUNT MENU           "

     SELECTION% = 1
     Count% = 8
     Start% = 1
     SCANCODE% = 8
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 16
     CALL Box0(5, 15, 14, 65, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN SELECTION% = 8

     D3% = SELECTION% - 1

   ON SELECTION% GOTO 20, 600, 600, 600, 600, 1300, 4500
   IF DRIVE >= 67 THEN RUN "HELLO.EXE"
   PRINT : PRINT "MAKE SURE FULL COUNT MAIN DISK IS IN DRIVE"
   LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
15 I$ = INKEY$: IF I$ = "" THEN 15
   RUN "HELLO.EXE"

20 AP%(0) = 0: NS% = 0: TS% = 0
21 GOSUB 4100

25 CLS
   LOCATE 23, 1: COLOR , 1: PRINT TAB(80); : LOCATE 23, 1
   COLOR 13: PRINT "MA"; : COLOR 14: PRINT "-MODE OF PLAY  ";
   COLOR 13: PRINT "WEA"; : COLOR 14: PRINT "-USE WEATHER";
   COLOR 11: PRINT " G"; : COLOR 14: PRINT "-GOTO GAME";
   COLOR 11: PRINT " B"; : COLOR 14: PRINT "-BOXSCORE OPTIONS"
   LOCATE 24, 1: COLOR , 1: PRINT TAB(80); : LOCATE 24, 1
   COLOR 11: PRINT "C"; : COLOR 14: PRINT "-SAVE AND CONTINUE  ";
   COLOR 11: PRINT "A"; : COLOR 14: PRINT "-ADD A GAME  ";
   COLOR 11: PRINT "D"; : COLOR 14: PRINT "-DELETE A GAME  ";
   COLOR 11: PRINT "E"; : COLOR 14: PRINT "-EDIT A GAME  ";
   COLOR 11: PRINT "PGUP  PGDN";
   LOCATE 25, 1: COLOR , 1: PRINT TAB(80); : LOCATE 25, 1
   COLOR 11: PRINT "S"; : COLOR 14: PRINT "-SORT GAMES  ";
   COLOR 11: PRINT "P"; : COLOR 14: PRINT "-PLAY GAMES   ";
   COLOR 11: PRINT "X"; : COLOR 14: PRINT "-SAVE AND EXIT  ";
   COLOR 11: PRINT "Q"; : COLOR 14: PRINT "-QUIT  ";
   COLOR 11: PRINT "R"; : COLOR 14: PRINT "-RESET SCORES";


   LOCATE 1, 1: PRINT TAB(80); : LOCATE 1, 1: PRINT F$: LOCATE 1, 33: COLOR 14: PRINT "AUTO SCHEDULER"
   COLOR , 0
50 LOCATE 2, 1
   COLOR 14
   PRINT "GAME  DATE      TIME    VISITING TEAM        HOME TEAM            MA  DH  WEA": COLOR 15
   FOR X = 1 TO 20
   LOCATE X + 2, 1
   IF X + NS% * 20 > AP%(0) THEN 100

   T1 = NG%(X + NS% * 20, 0): YN$ = YN$(X + NS% * 20, 0)
   GOSUB 9000
   V$(X) = T$(0)
   T1 = NG%(X + NS% * 20, 1): YN$ = YN$(X + NS% * 20, 1)
   GOSUB 9000
   H$(X) = T$(0)


   PRINT USING "####  "; X + NS% * 20;
   PRINT USING "##/##/##"; NG%(X + NS% * 20, 17); NG%(X + NS% * 20, 16); NG%(X + NS% * 20, 18);
   LOCATE , 17
   I$ = "A"
   HR% = NG%(X + NS% * 20, 15) / 100
   MIN% = NG%(X + NS% * 20, 15) - HR% * 100
   IF HR% = 12 THEN I$ = "P"
   IF HR% > 12 THEN HR% = HR% - 12: I$ = "P"
   TM$ = RIGHT$(STR$(HR%), 2) + ":"
   IF MIN% < 10 THEN TM$ = TM$ + "0" + RIGHT$(STR$(MIN%), 1)
   IF MIN% >= 10 THEN TM$ = TM$ + RIGHT$(STR$(MIN%), 2)
   TM$ = TM$ + I$
   PRINT TM$;
   LOCATE , 25: PRINT V$(X); " ";
   IF NG%(X + NS% * 20, 13) = 1 THEN PRINT USING "###"; VS%(X + NS% * 20);
   LOCATE , 46: PRINT H$(X); " ";
   IF NG%(X + NS% * 20, 13) = 1 THEN PRINT USING "###"; HS%(X + NS% * 20);
   LOCATE , 67: PRINT MO$(NG%(X + NS% * 20, 8));
   LOCATE , 72: IF NG%(X + NS% * 20, 3) = 1 THEN PRINT "Y"; :  ELSE PRINT "N";
   LOCATE , 75: PRINT TS$(1 - NG%(X + NS% * 20, 5))
   NEXT
100 I$ = INKEY$: IF I$ = "" THEN 100
   IF UCASE$(I$) = "A" THEN GOSUB 179: GOTO 25
   IF UCASE$(I$) = "E" THEN GOSUB 140: GOTO 25
   IF UCASE$(I$) = "D" THEN GOSUB 200: GOTO 25
   IF UCASE$(I$) = "G" THEN GOSUB 120: GOTO 25
   IF UCASE$(I$) = "B" THEN 5000
   IF UCASE$(I$) = "S" THEN GOSUB 8500: GOTO 25
   IF UCASE$(I$) = "C" THEN GOSUB 350: GOTO 25
   IF UCASE$(I$) = "X" THEN GOSUB 350: GOTO 10
   IF I$ = CHR$(0) + "Q" THEN GOSUB 130: GOTO 25
   IF I$ = CHR$(0) + "I" THEN GOSUB 135: GOTO 25
   IF UCASE$(I$) = "Q" THEN 10
   IF UCASE$(I$) = "P" THEN GOSUB 350: GOTO 300
   IF UCASE$(I$) = "R" THEN GOSUB 400: GOTO 25
   IF I$ = CHR$(27) THEN 10
   GOTO 100
120 REM:GOTO A SPECIFIC PAGE
    LOCATE 23, 1: PRINT "ENTER GAME # TO GO TO        "
    LOCATE 23, 1: INPUT "ENTER GAME # TO GO TO "; NG%
    IF NG% < 1 OR NG% > AP%(0) THEN 120
    NS% = INT((NG% - 1) / 20)
    RETURN
130 REM *** PAGE DOWN ***
    IF NS% >= BS% THEN NS% = BS%: GOTO 133
    NS% = NS% + 1
133 RETURN
135 REM *** PAGE UP ***
    IF NS% = 0 THEN 137
    NS% = NS% - 1
137 RETURN
140 REM *** EDIT A GAME ***
    LOCATE 23, 1: PRINT "ENTER GAME # TO EDIT        "
    LOCATE 23, 1: INPUT "ENTER GAME # TO EDIT "; NG%
    IF NG% < 1 OR NG% > AP%(0) THEN 140
    V$ = V$(NG% - INT(NG% / 20) * 20)
    H$ = H$(NG% - INT(NG% / 20) * 20)
    IF NG% - INT(NG% / 20) * 20 = 0 THEN V$ = V$(20): H$ = H$(20)
    I$ = "A"
    HR% = INT(NG%(NG%, 15) / 100)
    MIN% = NG%(NG%, 15) - HR% * 100
    IF HR% = 12 THEN I$ = "P"
    IF HR% > 12 THEN HR% = HR% - 12: I$ = "P"
    TM$ = RIGHT$(STR$(HR%), 2) + ":"
    IF MIN% < 10 THEN TM$ = TM$ + "0" + RIGHT$(STR$(MIN%), 1)
    IF MIN% >= 10 THEN TM$ = TM$ + RIGHT$(STR$(MIN%), 2)
    TM$ = TM$ + I$
    DT$ = RIGHT$(STR$(NG%(NG%, 17)), 2) + "/"
    DY% = VAL(RIGHT$(STR$(NG%(NG%, 16)), 2))
    IF DY% < 10 THEN DY$ = "0" + RIGHT$(STR$(NG%(NG%, 16)), 1)
    IF DY% >= 10 THEN DY$ = RIGHT$(STR$(NG%(NG%, 16)), 2)
    DT$ = DT$ + DY$ + "/" + RIGHT$(STR$(NG%(NG%, 18)), 2)
    GOTO 149
179 REM *** ADD A GAME TO THE SCHEDULE ***
    V$ = "": H$ = ""
    AP%(0) = AP%(0) + 1
    NG% = AP%(0)
    BS% = INT((NG% - 1) / 20): IF NS% < BS% THEN NS% = BS%
    FOR X = 0 TO 18
    IF X = 13 THEN 139
    NG%(NG%, X) = NG%(NG% - 1, X)
139 NEXT
    FOR X = 0 TO 1: YN$(NG%, X) = YN$(NG% - 1, X): NEXT
149 CLS : COLOR 2: PRINT TAB(29); "SELECT GAME OPTIONS": PRINT
151 LOCATE 2, 2: COLOR 14: PRINT "(1) "; : COLOR 15: PRINT U5$(NG%(NG%, 8))
152 LOCATE 3, 2: COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "VISITING TEAM: "; V$
    LOCATE 4, 2: COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "HOME TEAM: "; H$
154 LOCATE 5, 2: COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "DESIGNATED HITTER: "; TS$(NG%(NG%, 3))
157 LOCATE 6, 2: COLOR 14: PRINT "(5) "; : COLOR 15: PRINT USING "DISPLAY PAUSE IN SECONDS: ##.#  "; NG%(NG%, 4) / 10
160 LOCATE 7, 2: COLOR 14: PRINT "(6) "; : COLOR 15: PRINT "GAME DATE (XX-XX-XX) "; DT$; "   "
161 LOCATE 8, 2: COLOR 14: PRINT "(7) "; : COLOR 15: PRINT "GAME TIME (XX:XXP) "; TM$; "   "
158 LOCATE 9, 2: COLOR 14: PRINT "(8) "; : COLOR 15: PRINT "USE WEATHER EFFECTS: "; TS$(1 - NG%(NG%, 5))
    LOCATE 10, 2: COLOR 14: PRINT "(9) "; : COLOR 15: PRINT "USE INJURIES "; TS$(1 - NG%(NG%, 6))
    LOCATE 11, 2: COLOR 14: PRINT "(A) "; : COLOR 15: PRINT "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - NG%(NG%, 7))
    LOCATE 12, 2: COLOR 14: PRINT "(B) "; : COLOR 15: PRINT "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - NG%(NG%, 9))
    LOCATE 13, 2: COLOR 14: PRINT "(C) "; : COLOR 15: PRINT "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - NG%(NG%, 10))
    LOCATE 14, 2: COLOR 14: PRINT "(D) "; : COLOR 15: PRINT "USE VGA SCREENS, IF ABLE: "; TS$(1 - NG%(NG%, 11))
    LOCATE 15, 2: COLOR 14: PRINT "(E) "; : COLOR 15: PRINT "PITCHER ERA : "; PU$(NG%(NG%, 14))
176 LOCATE 16, 2: COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "ACCEPT SETTINGS"
175 I$ = INKEY$: IF I$ = "" THEN 175
    IF UCASE$(I$) = "A" THEN I$ = "10"
    IF UCASE$(I$) = "B" THEN I$ = "11"
    IF UCASE$(I$) = "C" THEN I$ = "12"
    IF UCASE$(I$) = "D" THEN I$ = "13"
    IF UCASE$(I$) = "E" THEN I$ = "14"
    I3% = VAL(I$): IF I3% < 0 OR I3% > 14 THEN 175
    ON I3% GOTO 180, 181, 181, 184, 187, 190, 191, 192, 251, 252, 253, 254, 255, 193
    IF V$ <> "" AND H$ <> "" AND TM$ <> "" AND DT$ <> "" THEN 250
    GOTO 175
250 I$ = RIGHT$(TM$, 1): TM$ = LEFT$(TM$, LEN(TM$) - 1)
    MIN% = VAL(RIGHT$(TM$, 2))
    TM$ = LEFT$(TM$, LEN(TM$) - 3)
    HR% = VAL(TM$)
    IF UCASE$(I$) = "P" AND HR% < 12 THEN HR% = HR% + 12
    NG%(NG%, 15) = HR% * 100 + MIN%
    NG%(NG%, 17) = VAL(LEFT$(DT$, 2))
    NG%(NG%, 18) = VAL(MID$((DT$), 7, 2))
    NG%(NG%, 16) = VAL(MID$((DT$), 4, 2))
    RETURN
195 NG%(NG%, 11) = 1 - NG%(NG%, 11): GOTO 149
251 NG%(NG%, 6) = 1 - NG%(NG%, 6): GOTO 149
252 NG%(NG%, 7) = 1 - NG%(NG%, 7): GOTO 149
253 NG%(NG%, 9) = 1 - NG%(NG%, 9): GOTO 149
254 NG%(NG%, 10) = 1 - NG%(NG%, 10): GOTO 149
255 NG%(NG%, 11) = 1 - NG%(NG%, 11): GOTO 149
193 NG%(NG%, 14) = NG%(NG%, 14) + 1
   IF NG%(NG%, 14) > 8 THEN NG%(NG%, 14) = 0
   GOTO 149
194 NG%(NG%, 10) = 1 - NG%(NG%, 10): GOTO 149
180 NG%(NG%, 8) = NG%(NG%, 8) + 1: IF NG%(NG%, 8) > 3 THEN NG%(NG%, 8) = 0
    GOTO 151
181 COLOR 15, 0: CLS
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
    GOSUB 1100
182 IF BO% = 1 THEN BO% = 0: GOTO 181
183 LOCATE 2, 33: COLOR L%(11), L%(12): PRINT T$(0): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 181
    COLOR 15, 0
    NG%(NG%, I3% - 2) = T1: YN$(NG%, I3% - 2) = YN$
    IF I3% = 2 THEN V$ = T$(0): V$(NG% - INT(NG% / 20) * 20) = T$(0)
    IF I3% = 3 THEN H$ = T$(0): H$(NG% - INT(NG% / 20) * 20) = T$(0)
    GOTO 149
184 NG%(NG%, 3) = 1 - NG%(NG%, 3): GOTO 154
187 LOCATE 6, 32: INPUT U6: NG%(NG%, 4) = U6 * 10: GOTO 157
190 LOCATE 7, 27: INPUT DT$: GOTO 160
191 LOCATE 8, 25: INPUT TM$: GOTO 161
192 NG%(NG%, 5) = 1 - NG%(NG%, 5): GOTO 158
200 REM *** DELETE A GAME FROM THE SCHEDULE ***
    LOCATE 24, 1: INPUT "ENTER GAME # TO DELETE "; NG%
    IF NG% < 1 OR NG% > AP%(0) THEN 200
    FOR X = NG% TO AP%(0) - 1
    FOR X1 = 0 TO 18: NG%(X, X1) = NG%(X + 1, X1): NEXT
    VS%(X) = VS%(X + 1)
    HS%(X) = HS%(X + 1)
    FOR X1 = 0 TO 1: YN$(X, X1) = YN$(X + 1, X1): NEXT
    NEXT
    AP%(0) = AP%(0) - 1: BS% = INT((AP%(0) - 1) / 20)
    IF NS% > BS% THEN NS% = BS%
    RETURN
300 REM *** PLAY GAMES ***
    COLOR , 0: CLS
    JJ = 8
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "TOTAL GAMES ="; AP%(0)
    LOCATE , 12: PRINT "SELECT GAME # TO START AND END WITH": PRINT
    LOCATE , 12: INPUT "ENTER START GAME #: "; SG%: PRINT
    LOCATE , 12: INPUT "ENTER END GAME #:"; EG%: PRINT
    LOCATE , 12: PRINT "ANY CHANGE (YN)"
301 I$ = INKEY$: IF I$ = "" THEN 301
    IF UCASE$(I$) = "Y" THEN 300
    IF SG% <= 0 THEN SG% = 1
    IF EG% > AP%(0) THEN EG% = AP%(0)
    OPEN "GAME82" FOR OUTPUT AS #2
    PRINT #2, F$
    PRINT #2, SG%
    PRINT #2, EG%
    PRINT #2, 0
    CLOSE 2
    GOSUB 1200
    OPEN "TICKER" FOR OUTPUT AS #2
    FOR I = 1 TO 32: FOR I1 = 0 TO 1: FOR I2 = 1 TO 14
    PRINT #2, -1: NEXT: NEXT: NEXT
    FOR I = 1 TO 32: FOR I1 = 0 TO 1: PRINT #2, "XXX"
    NEXT: NEXT
    FOR I = SG% TO SG% + 31
    PRINT #2, NG%(I, 12)
    NEXT
    CLOSE 2
335 RUN "LOADER.EXE"
350 REM *** SAVE SCHEDULE FILE ***
    IF EXIST%(D$(0) + F$) THEN KILL D$(0) + F$
365 OPEN D$(0) + F$ FOR RANDOM AS #1 LEN = 48
    FOR X = 0 TO 18
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 48 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 38 AS X$, 2 AS Q$(22), 2 AS Q$(23), 3 AS Q$(20), 3 AS Q$(21)
    FOR I = 1 TO AP%(0)
    FOR X = 0 TO 18: LSET Q$(X + 1) = MKI$(NG%(I, X)): NEXT
    FOR X = 0 TO 1: LSET Q$(20 + X) = YN$(I, X): NEXT
    LSET Q$(22) = MKI$(VS%(I)): LSET Q$(23) = MKI$(HS%(I))
    PUT #1, I
    NEXT
    CLOSE 1
    RETURN
400 REM *** RESET GAMES ***
    FOR I = 1 TO AP%(0)
    NG%(I, 13) = 0
    NEXT
    RETURN
600 REM *** LOAD IN A SAVED SCHEDULE ***
   Spec$ = D$(0) + "*.SCD"
   Count = FCount%(Spec$)
   IF Count = 0 THEN 4050
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)
    CLS
    COLOR , 1: CLS : LOCATE , 28: COLOR 14
    PRINT "SELECT SCHEDULE TO EDIT": COLOR 15
     SELECTION% = 1
     Start% = 1
     FileColr% = 31
     HiLiteColr% = 112
     Rows% = 20
     Columns% = 4
     Spacing% = 6
     Row% = 4
     Column% = 1
    LOCATE 25, 1: COLOR 14: PRINT "ESC"; : COLOR 15
    PRINT "-SELECT NEW ID  PGDN  PGUP  "; : COLOR 14
    PRINT "ENTER"; : COLOR 15
    PRINT "-SELECTS SCHEDULE";
CALL ShowCursor
620 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 10
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 620
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 620
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 620
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
604 I$ = INKEY$: IF I$ = "" THEN 604
    IF UCASE$(I$) = "Y" THEN 600
    OPEN D$(0) + F$ FOR RANDOM AS #1 LEN = 48
    Y = LOF(1)
    AP%(0) = Y / 48
    BS% = INT((AP%(0) - 1) / 20)
    FOR X = 0 TO 18
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 48 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 38 AS X$, 2 AS Q$(22), 2 AS Q$(23), 3 AS Q$(20), 3 AS Q$(21)
    FOR I = 1 TO AP%(0)
    GET #1, I
    FOR X = 0 TO 18: NG%(I, X) = CVI(Q$(X + 1)): NEXT
    IF NG%(I, 14) > 8 THEN NG%(I, 14) = 0
    FOR X = 0 TO 1: YN$(I, X) = Q$(20 + X): NEXT
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
    IF NG%(I, 6) > 1 THEN NG%(I, 6) = 1
    IF NG%(I, 7) > 1 THEN NG%(I, 7) = 1
    IF NG%(I, 9) > 1 THEN NG%(I, 9) = 1
    IF NG%(I, 10) > 1 THEN NG%(I, 10) = 1
    IF NG%(I, 11) > 1 THEN NG%(I, 11) = 1
    NEXT
    CLOSE 1
    IF D3% = 2 OR D3% = 3 OR D3% = 4 THEN 700
    GOTO 25
700 REM:EXTRACT A SCHEDULE FROM ANOTHER
    IF D3% = 3 THEN 725
    IF D3% = 4 THEN GOSUB 900: GOSUB 350: GOTO 25
    COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 1
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER NAME FOR SUB-SCHEDULE: ": PRINT
    LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    LOCATE , 12: PRINT "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
    LOCATE , 12: PRINT "WILL AUTOMATICALLY ADD .SCD"
    LOCATE , 12: PRINT "SAMPLE NAMES ARE PHI73, STL68, MONTREAL, ETC"
    LOCATE 7, 37: INPUT SF$
    IF SF$ = "" THEN 700
    LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
705 I$ = INKEY$: IF I$ = "" THEN 705
   IF UCASE$(I$) = "Y" THEN 700
   COLOR 15, 0
   CLS
725 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 4
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "SELECTING TEAM TO CHANGE CONTROL FOR"
    LOCATE , 12: PRINT "ENTER DISK ID: ": PRINT
    LOCATE , 12: PRINT "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
    LOCATE , 12: PRINT "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
    LOCATE , 12: PRINT
    LOCATE , 12: PRINT "HIT <ENTER> TO USE PREVIOUS ID"
    LOCATE 8, 27: INPUT NY$
    IF NY$ = "" THEN GOSUB 1102: GOTO 730
    YN$ = NY$
    FOR I = 1 TO 200: VV$(I) = "": NEXT
    GOSUB 1100
730 IF BO% = 1 THEN BO% = 0: GOTO 725
732 LOCATE 2, 33: COLOR L%(11), L%(12): PRINT T$(0): COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 732
    IF UCASE$(I$) = "Y" THEN 725
    COLOR 15, 0
    IF D3% = 3 THEN GOSUB 775
    REM:HAVE T1 AND YN$
    FOR I = 1 TO AP%(0)
735 IF RIGHT$(YN$(I, 0), 1) = " " THEN YN$(I, 0) = LEFT$(YN$(I, 0), LEN(YN$(I, 0)) - 1): GOTO 735
736 IF RIGHT$(YN$(I, 1), 1) = " " THEN YN$(I, 1) = LEFT$(YN$(I, 1), LEN(YN$(I, 1)) - 1): GOTO 736
    IF T1 = NG%(I, 0) AND YN$ = YN$(I, 0) OR T1 = NG%(I, 1) AND YN$ = YN$(I, 1) THEN GOSUB 780: GOSUB 750
    NEXT
    IF D3% = 3 THEN GOSUB 350
    GOTO 10
750 IF D3% = 3 THEN 755
    LOCATE 1, 1: PRINT NG% + 1; " MATCHES"
    OPEN D$(0) + SF$ + ".SCD" FOR RANDOM AS #1 LEN = 48
    Y = LOF(1)
    GOTO 753
752 Y = 0
753 NG% = Y / 48
    FOR X = 0 TO 18
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 48 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 38 AS X$, 2 AS Q$(22), 2 AS Q$(23), 3 AS Q$(20), 3 AS Q$(21)
    FOR X = 0 TO 18: LSET Q$(X + 1) = MKI$(NG%(I, X)): NEXT
    FOR X = 0 TO 1: LSET Q$(20 + X) = YN$(I, X): NEXT
    LSET Q$(22) = MKI$(VS%(I)): LSET Q$(23) = MKI$(HS%(I))
    PUT #1, NG% + 1
    CLOSE 1
755 RETURN
775 CLS
    COLOR L%(11), L%(12): PRINT T$(0); : COLOR 15, 0: PRINT " IS TO BE:"
    PRINT
    PRINT "(0) MANUALLY CONTROLLED": PRINT
    PRINT "(1) COMPUTER CONTROLLED"
776 I$ = INKEY$: IF I$ = "" THEN 776
    I1 = VAL(I$)
    IF I1 < 0 OR I1 > 1 THEN 776
    INPUT "ENTER DISPLAY VARIABLE FOR THESE GAMES "; Y1
    Y1 = Y1 * 10
    RETURN
780 REM: CHANGE SETTINGS FOR A TEAM
    IF T1 = NG%(I, 0) AND YN$ = YN$(I, 0) THEN NG%(I, 8) = 2
    IF T1 = NG%(I, 1) AND YN$ = YN$(I, 1) THEN NG%(I, 8) = 1
    IF (T1 = NG%(I, 0) AND YN$ = YN$(I, 0) OR T1 = NG%(I, 1) AND YN$ = YN$(I, 1)) AND I1 = 1 AND D3% = 3 THEN NG%(I, 8) = 3
    NG%(I, 4) = Y1
    RETURN

900 REM: GLOBAL SETTINGS FOR A SCHEDULE

    COLOR 15, 0: CLS : PRINT TAB(30); "GLOBAL SETTINGS FOR SCHEDULE"
REM:901 LOCATE 2, 2: COLOR 14: PRINT "(0) "; : COLOR 15: PRINT U5$(U5)
901 LOCATE 3, 2: COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "DESIGNATED HITTER: "; TS$(DH%)
    LOCATE 4, 2: COLOR 14: PRINT "(2) "; : COLOR 15: PRINT USING "DISPLAY PAUSE IN SECONDS: ##.#  "; Y1 / 10
    LOCATE 5, 2: COLOR 14: PRINT "(3) "; : COLOR 15: PRINT "USE WEATHER EFFECTS: "; TS$(1 - WE%)
    LOCATE 6, 2: COLOR 14: PRINT "(4) "; : COLOR 15: PRINT "USE INJURIES "; TS$(1 - IJ%)
    LOCATE 7, 2: COLOR 14: PRINT "(5) "; : COLOR 15: PRINT "USE LEFT/RIGHT SPLIT STATS "; TS$(1 - LR%)
    LOCATE 8, 2: COLOR 14: PRINT "(6) "; : COLOR 15: PRINT "USE DAYS REST DATA FOR PITCHERS "; TS$(1 - DR%)
    LOCATE 9, 2: COLOR 14: PRINT "(7) "; : COLOR 15: PRINT "USE MANAGER PROFILE, IF AVAILABLE "; TS$(1 - MP%)
    LOCATE 10, 2: COLOR 14: PRINT "(8) "; : COLOR 15: PRINT "USE VGA SCREENS, IF ABLE: "; TS$(1 - VG%)
    LOCATE 11, 2: COLOR 14: PRINT "(9) "; : COLOR 15: PRINT "PITCHER ERA: "; PU$(PE%)
    LOCATE 12, 2: COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "ACCEPT GLOBAL SETTINGS"
905 I$ = INKEY$: IF I$ = "" THEN 905
    I = VAL(I$)
    IF I < 0 OR I > 9 THEN 905
    IF I = 1 THEN DH% = 1 - DH%: GOTO 901
    IF I = 3 THEN WE% = 1 - WE%: GOTO 901
    IF I = 4 THEN IJ% = 1 - IJ%: GOTO 901
    IF I = 5 THEN LR% = 1 - LR%: GOTO 901
    IF I = 6 THEN DR% = 1 - DR%: GOTO 901
    IF I = 7 THEN MP% = 1 - MP%: GOTO 901
    IF I = 8 THEN VG% = 1 - VG%: GOTO 901
    IF I = 9 THEN PE% = PE% + 1
    IF I = 9 AND PE% > 8 THEN PE% = 0: GOTO 901
    IF I = 9 THEN 901
    IF I = 2 THEN INPUT "DISPLAY VARIABLE "; Y1: Y1 = Y1 * 10: GOTO 901
REM:    IF I = 0 THEN U5 = U5 + 1
REM:    IF I = 0 AND U5 > 3 THEN U5 = 0
REM:    IF I = 0 THEN 901

    FOR I = 1 TO AP%(0)
REM:    NG%(I, 8) = U5
    NG%(I, 3) = DH%
    NG%(I, 4) = Y1
    NG%(I, 5) = WE%
    NG%(I, 6) = IJ%
    NG%(I, 7) = LR%
    NG%(I, 9) = DR%
    NG%(I, 10) = MP%
    NG%(I, 11) = VG%
    NG%(I, 14) = PE%
    NEXT
    RETURN

1100 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN 2060
     OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
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
CALL ShowCursor
26 MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN BO% = 1: RETURN
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 26
     IF EXITCODE% = -79 THEN Start% = NT: EXITCODE% = 3: SELECTION% = NT: GOTO 26
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > NT THEN Start% = NT
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 26
     T1 = V%(SELECTION%)
     GOTO 9000
1200 FOR I = 1 TO AP%(0)
     HR% = INT(NG%(I, 15) / 100)
     MIN% = NG%(I, 15) - HR% * 100
     TM% = HR% * 60 + MIN%
     NG%(I, 12) = INT(TM% / 30 - 18)
     NEXT
     RETURN
2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT
     PRINT "MAKE SURE PROPER DISK IS IN DRIVE OR CORRECT TEAM NUMBER IS ENTERED"
2061 PRINT : PRINT : PRINT "PRESS ANY KEY TO CONTINUE"
2062 I$ = INKEY$: IF I$ = "" THEN 2062
2063 GOTO 2720
2720 BO% = 1: RETURN
8500 REM *** SORT SCHEDULE ***
     FOR I = 1 TO AP%(0)
     HR% = INT(NG%(I, 15) / 100)
     MIN% = NG%(I, 15) - HR% * 100
     TM% = HR% * 60 + MIN%
     NG%(I, 12) = INT(TM% / 30 - 18)
     D = (NG%(I, 18) - 1): D = D * 365
     M = (NG%(I, 17) - 1): M = M * 31
     Y = NG%(I, 16)
     DY(I) = D + M + Y
     NEXT
     FOR Z = 1 TO AP%(0) - 1: FOR Z1 = Z + 1 TO AP%(0)
     IF NG%(Z, 12) <= NG%(Z1, 12) THEN 8505
     GOSUB 8525
8505 NEXT Z1: NEXT Z
     FOR Z = 1 TO AP%(0) - 1: FOR Z1 = Z + 1 TO AP%(0)
     IF DY(Z) <= DY(Z1) THEN 8510
     GOSUB 8525
8510 NEXT Z1: NEXT Z
     RETURN
8525 FOR I = 0 TO 18: SWAP NG%(Z, I), NG%(Z1, I): NEXT
     FOR I = 0 TO 1: SWAP YN$(Z, I), YN$(Z1, I): NEXT
     SWAP DY(Z), DY(Z1)
     SWAP VS%(Z), VS%(Z1)
     SWAP HS%(Z), HS%(Z1)
     RETURN
9000 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #4 LEN = 4342
     FIELD #4, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
9005 GET #4, T1
     T$(0) = Q$(0)
9010 IF RIGHT$(T$(0), 1) = " " THEN T$(0) = LEFT$(T$(0), LEN(T$(0)) - 1): GOTO 9010
     FOR I1 = 1 TO 13: L%(I1) = CVI(Q$(I1)): NEXT
     CLOSE 4
     RETURN
1300 CLS : COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 33: COLOR 14
   PRINT "AUTO SCHEDULER"
   LOCATE 25, 1: PRINT TAB(80); : LOCATE , 1: PRINT "(C) SHANNON LYNN 1990 V3.0";
   LOCATE 5, 1: COLOR , 0
   COLOR 15: PRINT "(0) "; : COLOR 14: PRINT "8 TEAM, 154 GAME SCHEDULE": PRINT
   COLOR 15: PRINT "(1) "; : COLOR 14: PRINT "10 TEAM, 162 GAME SCHEDULE": PRINT
   COLOR 15: PRINT "(2) "; : COLOR 14: PRINT "12 TEAM, 2 DIVISION, 162 GAME SCHEDULE": PRINT
   COLOR 15: PRINT "(3) "; : COLOR 14: PRINT "14 TEAM, 2 DIVISION, 162 GAME SCHEDULE": PRINT
   COLOR 15: PRINT "(4) "; : COLOR 14: PRINT "RETURN TO FULL COUNT MAIN MENU"
1305 I$ = INKEY$: IF I$ = "" THEN 1305
   D3% = VAL(I$)
   IF I$ = CHR$(27) THEN D3% = 4
   IF D3% < 0 OR D3% > 4 THEN 1305
   IF D3% = 4 THEN 10
   Spec$ = D$(2) + "*.LIB"
   Count = FCount%(Spec$)
   IF Count = 0 THEN 4000
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
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE TO SCHEDULE": COLOR 15
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
    PRINT "-SELECTS LEAGUE";
CALL ShowCursor
1320 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 1300
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 1320
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 1320
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 1320
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
    OPEN D$(2) + F$ FOR INPUT AS #2
    INPUT #2, LG$
    INPUT #2, D1$: INPUT #2, D2$: INPUT #2, D3$: INPUT #2, D4$
    FOR X = 1 TO 40
    INPUT #2, Z1$(X): INPUT #2, Z1%(X): INPUT #2, Z2$(X)
    NEXT
    CLOSE 2
    IF D3% = 0 THEN F$ = "154TEMP.SCX"
    IF D3% = 1 THEN F$ = "162TENTM.SCX"
    IF D3% = 2 THEN F$ = "16212TM.SCX"
    IF D3% = 3 THEN F$ = "16214TM.SCX"
    OPEN F$ FOR RANDOM AS #1 LEN = 48
    Y = LOF(1)
    AP%(0) = Y / 48
    BS% = INT((AP%(0) - 1) / 20)
    FOR X = 0 TO 18
    FIELD #1, X * 2 AS X$, 2 AS Q$(X + 1), 48 - 2 - 2 * X AS X$
    NEXT
    FIELD #1, 38 AS X$, 2 AS Q$(22), 2 AS Q$(23), 3 AS Q$(20), 3 AS Q$(21)
    FOR I = 1 TO AP%(0)
    GET #1, I
    FOR X = 0 TO 18: NG%(I, X) = CVI(Q$(X + 1)): NEXT
    FOR X = 0 TO 1: YN$(I, X) = Q$(20 + X): NEXT
    VS%(I) = CVI(Q$(22)): HS%(I) = CVI(Q$(23))
    IF NG%(I, 14) > 8 THEN NG%(I, 14) = 0
    IF NG%(I, 6) > 1 THEN NG%(I, 6) = 1
    IF NG%(I, 7) > 1 THEN NG%(I, 7) = 1
    IF NG%(I, 9) > 1 THEN NG%(I, 9) = 1
    IF NG%(I, 10) > 1 THEN NG%(I, 10) = 1
    IF NG%(I, 11) > 1 THEN NG%(I, 11) = 1
    NEXT
    CLOSE 1
    GOSUB 900
    GOSUB 4100
    FOR I = 1 TO 20
    PRINT Z1%(I), Z2$(I)
    NEXT
    FOR I = 1 TO AP%(0)
    PRINT "GAME"; I
    PRINT NG%(I, 0), YN$(I, 0)
    PRINT NG%(I, 1), YN$(I, 1)
    IF D3% = 0 OR D3% = 1 THEN X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X)
    IF D3% = 2 AND NG%(I, 0) <= 6 THEN X = NG%(I, 0): PRINT X: NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): GOTO 3915
    IF D3% = 2 AND NG%(I, 0) > 6 THEN X = NG%(I, 0): PRINT X: NG%(I, 0) = Z1%(X + 4): YN$(I, 0) = Z2$(X + 4)
3915 IF D3% = 2 AND NG%(I, 1) <= 6 THEN X = NG%(I, 1): PRINT X: NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X): GOTO 3916
     IF D3% = 2 AND NG%(I, 1) > 6 THEN X = NG%(I, 1): PRINT X: NG%(I, 1) = Z1%(X + 4): YN$(I, 1) = Z2$(X + 4)
3916 IF D3% = 3 AND NG%(I, 0) <= 7 THEN X = NG%(I, 0): NG%(I, 0) = Z1%(X): YN$(I, 0) = Z2$(X): GOTO 3917
    IF D3% = 3 AND NG%(I, 0) > 7 THEN X = NG%(I, 0): NG%(I, 0) = Z1%(X + 3): YN$(I, 0) = Z2$(X + 3)
3917 IF D3% = 3 AND NG%(I, 1) <= 7 THEN X = NG%(I, 1): NG%(I, 1) = Z1%(X): YN$(I, 1) = Z2$(X): GOTO 3918
    IF D3% = 3 AND NG%(I, 1) > 7 THEN X = NG%(I, 1): NG%(I, 1) = Z1%(X + 3): YN$(I, 1) = Z2$(X + 3)

3918 PRINT "GAME"; I
    PRINT NG%(I, 0), YN$(I, 0)
    PRINT NG%(I, 1), YN$(I, 1)
    NEXT
    GOSUB 350
    GOTO 25
4100 COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 1
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER NAME FOR SCHEDULE: ": PRINT
    LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    LOCATE , 12: PRINT "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
    LOCATE , 12: PRINT "WILL AUTOMATICALLY ADD .SCD"
    LOCATE , 12: PRINT "SAMPLE NAMES ARE MONTREAL, STL68, NY76, CIN90, ETC"
    LOCATE 7, 37: INPUT F$
    IF F$ = "" THEN 21
    LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
22 I$ = INKEY$: IF I$ = "" THEN 22
   IF UCASE$(I$) = "Y" THEN 4100
   COLOR 15, 0
   F$ = F$ + ".SCD"
RETURN

'EDIT THE PARAMETERS TO SAVE A SPECIAL EVENTS BOXSCORE
5070 IF NOT EXIST%("EVENTS.BOX") THEN 5075
     OPEN "EVENTS.BOX" FOR INPUT AS #1
     FOR X = 1 TO 13
     INPUT #1, E%(X)
     NEXT
     CLOSE 1
5075 CLS : COLOR 14
     PRINT "PARAMETERS FOR SAVING SPECIAL EVENTS BOXSCORES": X% = 2: COLOR 15
     PRINT " 1. IF INNINGS ARE OVER "; E%(1)
     PRINT " 2. A TEAM'S RUNS LESS THAN "; E%(2)
     PRINT " 3. A TEAM'S RUNS MORE THAN "; E%(3)
     PRINT " 4. A TEAM'S HITS LESS THAN "; E%(4)
     PRINT " 5. A TEAM'S HITS MORE THAN "; E%(5)
     PRINT " 6. BATTER'S HITS MORE THAN "; E%(6)
     PRINT " 7. BATTER'S RUNS MORE THAN "; E%(7)
     PRINT " 8. BATTER'S RBI'S MORE THAN "; E%(8)
     PRINT " 9. BATTER'S 2B'S MORE THAN "; E%(9)
     PRINT "10. BATTER'S 3B'S MORE THAN "; E%(10)
     PRINT "11. BATTER'S HR'S MORE THAN "; E%(11)
     PRINT "12. BATTER'S SB'S MORE THAN "; E%(12)
     PRINT "13. PITCHER'S K'S MORE THAN "; E%(13)
     GOTO 197
147 LOCATE 2: INPUT " 1. IF INNINGS ARE OVER "; E%(1): IF C1 = 1 THEN 197
162 LOCATE 3: INPUT " 2. A TEAM'S RUNS LESS THAN "; E%(2): IF C1 = 1 THEN 197
163 LOCATE 4: INPUT " 3. A TEAM'S RUNS MORE THAN "; E%(3): IF C1 = 1 THEN 197
148 LOCATE 5: INPUT " 4. A TEAM'S HITS LESS THAN "; E%(4): IF C1 = 1 THEN 197
178 LOCATE 6: INPUT " 5. A TEAM'S HITS MORE THAN "; E%(5): IF C1 = 1 THEN 197
150 LOCATE 7: INPUT " 6. BATTER'S HITS MORE THAN "; E%(6): IF C1 = 1 THEN 197
111 LOCATE 8: INPUT " 7. BATTER'S RUNS MORE THAN "; E%(7): IF C1 = 1 THEN 197
112 LOCATE 9: INPUT " 8. BATTER'S RBI'S MORE THAN "; E%(8): IF C1 = 1 THEN 197
153 LOCATE 10: INPUT " 9. BATTER'S 2B'S MORE THAN "; E%(9): IF C1 = 1 THEN 197
114 LOCATE 11: INPUT "10. BATTER'S 3B'S MORE THAN "; E%(10): IF C1 = 1 THEN 197
155 LOCATE 12: INPUT "11. BATTER'S HR'S MORE THAN "; E%(11): IF C1 = 1 THEN 197
156 LOCATE 13: INPUT "12. BATTER'S SB'S MORE THAN "; E%(12): IF C1 = 1 THEN 197
117 LOCATE 14: INPUT "13. PITCHER'S K'S MORE THAN "; E%(13): IF C1 = 1 THEN 197
197 C1 = 1
    LOCATE 23, 1: PRINT "SELECT # TO CHANGE 1-13 (0 TO CONTINUE) ";
    INPUT B9
    ON B9 GOTO 147, 162, 163, 148, 178, 150, 111, 112, 153, 114, 155, 156, 117

     OPEN "EVENTS.BOX" FOR OUTPUT AS #1
     FOR X = 1 TO 13
     PRINT #1, E%(X)
     NEXT
     CLOSE 1
     GOTO 4500

4000 LOCATE 10, 8
     CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 10

4010 LOCATE 10, 8
     CALL CSMsgBox("NO SPECIAL EVENTS BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Cnf)
3806 I$ = INKEY$: IF I$ = "" THEN 3806
     CALL CSMsgBox("", 0, Cnf)
     GOTO 4500

4020 LOCATE 10, 8
     CALL CSMsgBox("NO BOXSCORES WERE FOUND PRESS ANY KEY TO RETURN TO MENU", 26, Cnf)
3808 I$ = INKEY$: IF I$ = "" THEN 3808
     CALL CSMsgBox("", 0, Cnf)
     GOTO 4500

4050 LOCATE 10, 8
     CALL CSMsgBox("NO SCHEDULE FILES WERE FOUND... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3810 I$ = INKEY$: IF I$ = "" THEN 3810
     CALL CSMsgBox("", 0, Cnf)
     GOTO 10

4060 LOCATE 10, 8
     CALL CSMsgBox("BOXSCORE DOES NOT EXIST PRESS ANY KEY TO RETURN TO MAIN MENU", 23, Cnf)
3807 I$ = INKEY$: IF I$ = "" THEN 3807
     CALL CSMsgBox("", 0, Cnf)
     RETURN

4500 'BOXSCORE MAINTENANCE
   CALL TITLE
   ITEM$(1) = "SET PARAMETERS TO SAVE SPECIAL EVENTS BOXSCORES  "
   ITEM$(2) = "ACCESS USER SAVED AND SPECIAL EVENTS BOXSCORES   "
   ITEM$(3) = "DELETE ALL SCHEDULE GENERATED BOXSCORES          "
   ITEM$(4) = "DELETE ALL USER SAVED, & SPECIAL BOXSCORES       "
   ITEM$(5) = "DELETE ALL BOXSCORES (SCHEDULE, USER, &  SPECIAL)"
   ITEM$(6) = "RETURN TO MAIN AUTO SCHEDULER MENU               "

     SELECTION% = 1
     Count% = 6
     Start% = 1
     SCANCODE% = 6
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 6
     Column% = 16
     CALL Box0(5, 15, 12, 65, 2, 31)
     CALL ShowCursor
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN SELECTION% = 6

     ON SELECTION% GOTO 5070, 5060, 5010, 5010, 5010

      GOTO 10

5000 REDIM array$(500)               'holds the main editor string array
     LOCATE 23, 1: PRINT "ENTER GAME # TO GO TO        "
     LOCATE 23, 1: INPUT "ENTER GAME # TO GO TO "; NG%
     IF NG% < 1 OR NG% > AP%(0) THEN 120
     NS% = INT((NG% - 1) / 20)
     BN$ = D$(1) + "box" + MID$(STR$(NG%), 2, LEN(STR$(NG%)))
     GOSUB 5050
     GOTO 25

5010 IF SELECTION% = 3 THEN Spec$ = D$(1) + "BOX*."
     IF SELECTION% = 4 THEN Spec$ = D$(1) + "*.SPC"
     IF SELECTION% = 5 THEN Spec$ = D$(1) + "*.SPC"
     GOSUB 5020
     IF SELECTION% = 5 THEN Spec$ = D$(1) + "BOX*.":GOSUB 5020
     GOTO 4500

5020 Count = FCount%(Spec$)
     IF Count = 0 AND SELECTION% = 4 THEN 4010
     IF Count = 0 AND SELECTION% = 3 THEN 4020
     IF Count = 0 AND SELECTION% = 5 THEN RETURN
     REDIM F$(0 TO Count)
     FOR X = 1 TO Count
     F$(X) = SPACE$(12)
     NEXT
     F$(0) = Spec$
     CALL ReadFile(VARPTR(F$(0)))

     FOR X = 1 TO Count
     CALL KILLFILE(D$(1) + F$(X))
     LOCATE 12,34:PRINT "DELETING " F$(X)
     NEXT
     RETURN


5060 Spec$ = D$(1) + "*.SPC"
   Count = FCount%(Spec$)
   IF Count = 0 THEN 4010
   REDIM F$(0 TO Count)
   FOR X = 1 TO Count
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count, 0)

5065 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT BOXSCORE TO VIEW": COLOR 15
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
    PRINT "-SELECTS BOXSCORE";
CALL ShowCursor
4520 MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, EXITCODE%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HideCursor
     IF EXITCODE% = 27 THEN 4500
     IF EXITCODE% = -71 THEN Start% = 1: EXITCODE% = 3: SELECTION% = 1: GOTO 4520
     IF EXITCODE% = -79 THEN Start% = Count%: EXITCODE% = 3: SELECTION% = Count%: GOTO 4520
     IF EXITCODE% = -73 THEN Start% = Start% - 80: IF Start% < 1 THEN Start% = 1
     IF EXITCODE% = -81 THEN Start% = Start% + 80: IF Start% > Count% THEN Start% = Count%
     IF EXITCODE% = -81 OR EXITCODE% = -73 THEN EXITCODE% = 3: SELECTION% = Start%: GOTO 4520
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3911 I$ = INKEY$: IF I$ = "" THEN 3911
    IF UCASE$(I$) = "Y" THEN 5065
    BN$ = D$(1) + F$
    REDIM array$(500)               'holds the main editor string array
    GOSUB 5050
    GOTO 5060

'EDIT A BOXSCORE LOADED FROM THE FILE NAME BN$

5050 IF NOT EXIST%(BN$) THEN GOSUB 4060: RETURN
     OPEN BN$ FOR INPUT AS #1
     CURLINE = 1
     WHILE NOT EOF(1)
       LINE INPUT #1, array$(CURLINE)
       CURLINE = CURLINE + 1
    WEND
    CLOSE #1


    CLS                             '  show how QEdit saves the screen

    COLOR 15, 0                     'print instructions on bottom of screen
    LOCATE 25, 1
    PRINT " ESC - Quit    F1 - Help    F2 - Print Boxscore   F3 - Save Boxscore"; TAB(79);



    '----- fill in editing parameters
    SELECT CASE Monitor%            'determine the monitor type
       CASE 3, 5, 7, 9, 10, 12      'CGA, EGA, VGA monitors
          Ed.AColor = 23            'white on blue
       CASE ELSE                    'all other monitors
          Ed.AColor = 7             'white on black
    END SELECT

    Ed.Wrap = 0   'word wrap at column 65 (use 0 for to disable word wrap)
    Ed.Frame = 0  'frame code (1 = show ruler line & scroll bars, 0 = don't)
    Ed.Rows = 24   'number of text lines to display on the screen (3 - 25)
    Ed.Wide = 80   'width of the active screen (10 - 80)
    Ed.HTab = 4    'set tab stops to 4 spaces

    LOCATE 1, 1    'locate the upper left corner of the edit window frame


    Action = 1                      'Action 1 to save underlying screen and
                                    '  display text, operate in polled mode
    '----- main loop for editing
    DO
        '----- call QEdit in polled mode
        QEdit array$(), Ky$, Action, Ed

        '----- F2 function key pressed? - change margin
IF Ky$ = F2$ THEN : FOR I = 1 TO CURLINE - 1: LPRINT array$(I): NEXT

IF Ky$ = F3$ THEN
     OPEN BN$ FOR OUTPUT AS #1
     FOR I = 1 TO CURLINE - 1
     PRINT #1, array$(I)
     NEXT
     CLOSE 1
     CALL CSMsgBox("REVISED BOXSCORE HAS BEEN SAVED...PRESS ANY KEY TO CONTINUE", 31, Cnf)
3815 I$ = INKEY$: IF I$ = "" THEN 3815
     CALL CSMsgBox("", 0, Cnf)
END IF

    LOOP UNTIL Ky$ = CHR$(27)       'keep calling QEdit until Esc is pressed

    '----- call QEdit again with an Action flag of 5 to restore the screen
    QEdit array$(), Ky$, 5, Ed


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

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 33: COLOR 14
   PRINT "AUTO SCHEDULER"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1995";
   LOCATE 5, 1: COLOR , 0
END SUB

