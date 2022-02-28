DEFINT A-Z
DECLARE SUB GETKEY ()
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)
DECLARE FUNCTION GetDrive% ()
DECLARE SUB YesNoB (YN$, ExitCode, Colr)
DECLARE SUB ClearEOL (Colr)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE FUNCTION FUsing$ (Number$, Mask$)
DECLARE FUNCTION OneColor% (FG, BG)
DECLARE FUNCTION Peek1% (Segment, Address)
DECLARE SUB QPrint (X$, Colr, Page)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE SUB CapNum ()
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
   DIM Z1$(40), Z2$(40), Z1%(40), D$(2)
   DIM Q$(15), VV$(200), V%(200), T%(34), L%(13)
   DIM Y AS LONG
   '$INCLUDE: 'DEFCNF.BI'
   '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
   DEF FnExt$ (X$, Length) = X$ + SPACE$(Length - LEN(X$))  'for display only
'determine if the monitor is mono or color
   '
      FG = 0
      BG = 7

   Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
   DRIVE = GetDrive%
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 2
   INPUT #2, D$(I)
   NEXT
   CLOSE 2
   CLS
   IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 50
   PRINT "MAKE SURE DISK WITH STATS AND LEADER FILES IS IN DRIVE "; D$(2)
   CALL GETKEY
50 FOR X = 1 TO 40
   Z1$(X) = "XXX": Z2$(X) = "XXX": Z1%(X) = -1
   NEXT
60 CLS
   COLOR 12: PRINT TAB(31); "LEAGUE EDITOR MENU": PRINT
70 COLOR 14: PRINT "(1)"; : COLOR 15: PRINT " EDIT AN EXISTING LEAGUE": PRINT
80 COLOR 14: PRINT "(2)"; : COLOR 15: PRINT " CREATE A NEW LEAGUE": PRINT
90 COLOR 14: PRINT "(3)"; : COLOR 15: PRINT " RETURN TO STANDINGS/LEADERS MENU"
100 I$ = INKEY$: IF I$ = "" THEN 100
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
22  I$ = INKEY$: IF I$ = "" THEN 22
    IF UCASE$(I$) = "Y" THEN 130
25  CLS
    COLOR 15, 1
    LOCATE 5, 1: QPrint "Name for Division 1 ", 30, -1
    LOCATE , 21: QPrint FnExt$(D1$, 8), Colr, -1
    LOCATE 7, 1: QPrint "Name for Division 2 ", 30, -1
    LOCATE , 21: QPrint FnExt$(D2$, 8), Colr, -1
    LOCATE 9, 1: QPrint "Name for Division 3 ", 30, -1
    LOCATE , 21: QPrint FnExt$(D3$, 8), Colr, -1
    LOCATE 11, 1: QPrint "Name for Division 4 ", 30, -1
    LOCATE , 21: QPrint FnExt$(D4$, 8), Colr, -1

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
   CALL YesNoB(X$, XCode, Colr)
   LOCATE , 1
   ClearEOL 30
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
250 I$ = INKEY$: IF I$ = "" THEN 250
260 IF I$ = "3" THEN 950
270 LOCATE 20, 30: PRINT "SLOT # (1-40) ";
280 N = 1: INPUT B
290 IF B < 1 OR B > 40 THEN 270
300 IF I$ = "2" THEN Z1$(B) = "XXX": GOTO 220
310 GOSUB 1440
390 Z1$(B) = T1$: Z2$(B) = YN$: Z1%(B) = D
400 GOTO 220
950 CLS
   IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 970
    PRINT "INSERT COMPILED STATS DISK IN DRIVE "; D$(2)
    CALL GETKEY
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
     IF DRIVE >= 67 THEN RUN "HELLO.EXE"
     PRINT "MAKE SURE GAME DISK IS IN DRIVE"
     CALL GETKEY
     RUN "HELLO.EXE"
     END
1100 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN 2060
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
     D = V%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."

1300 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
FIELD #1, 15 AS Q$(0), 2 AS Q$(1), 2 AS Q$(2), 2 AS Q$(3), 2 AS Q$(4), 2 AS Q$(5), 2 AS Q$(6), 2 AS Q$(7), 2 AS Q$(8), 2 AS Q$(9), 2 AS Q$(10), 2 AS Q$(11), 2 AS Q$(12), 2 AS Q$(13), 4 AS Q$(14), 4297 AS X$
GET #1, D
T1$ = Q$(0)
T1$ = QPRTrim$(T1$)
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
    FOR I = 1 TO 200: VV$(I) = "": NEXT
    IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 1441
    CLS : PRINT "MAKE SURE PROPER TEAMS DISK IS IN DRIVE "; D$(0)
    CALL GETKEY
1441 GOSUB 1100
182 IF BO% = 1 THEN BO% = 0: GOTO 1440
183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T1$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 1440
    RETURN
2060 CLS : PRINT "ERROR:: UNABLE TO READ TEAM FILE": PRINT
     PRINT "MAKE SURE PROPER DISK IS IN DRIVE OR CORRECT TEAM NUMBER IS ENTERED"
2061 PRINT : PRINT : PRINT "PRESS ANY KEY TO CONTINUE"
2062 I$ = INKEY$: IF I$ = "" THEN 2062
2063 GOTO 2720
2720 BO% = 1: RETURN

4000 Spec$ = D$(2) + "*.LIB"
    Count = FCount%(Spec$)
   IF Count = 0 THEN 4500
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
    PRINT "-SELECTS TEAM";
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 60
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 F$ = QPRTrim$(F$)
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
     CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 50

STATIC SUB CapNum ()

   CapStatus = Peek1%(0, &H417) AND &H40        'get BIOS status of Caps Lock
   NumStatus = Peek1%(0, &H417) AND &H20        'get BIOS status of Num Lock
   H = POS(0): V = CSRLIN                       'save cursor position

   IF Cap = CapStatus GOTO QNumCheck            'no change since last check

   Cap = CapStatus                              'save Caps status
   LOCATE 25, 69                                'position to print status msg

   IF CapStatus THEN
      QPrint " CAP ", 112, -1                   'white on black
   ELSE
      QPrint SPACE$(5), 7, -1                   'print or erase message
   END IF



QNumCheck:

   IF Num = NumStatus GOTO QCapNumExit          'no change since last check

   Num = NumStatus                              'save Num status
   LOCATE 25, 75                                'position to print status msg

   IF NumStatus THEN                            'print or erase message
      QPrint " NUM ", 112, -1
   ELSE
      QPrint SPACE$(5), 7, -1
   END IF



QCapNumExit:

   LOCATE V, H                                  'restore cursor position

END SUB

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

STATIC SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)

TInitialize:

   Clr = 0                                      'determine monitor type
   IF Peek1%(0, &H463) <> &HB4 THEN Clr = 1

   X$ = T$                                      'work on a copy of the string
'  X$ = QPTrim$(T$)                             'use this if the incoming
                                                '  string is fixed-length
TC:

   ExitCode = 0: Insrt = 0: Curpo = 1           'initialize flags

   Length = LEN(X$)
   IF Length > Max THEN EXIT SUB                'already to big to edit

   X$ = X$ + SPACE$(Max - Length)               'pad with trailing spaces
   QPrint X$, Colr, -1
   LOCATE , , 1                                 'turn on the cursor

   GOSUB TInsertOff                     'set cursor size according to display



TGetKey:

   'CapNum                               'display Cap/Num if active

   'disallow insert if cursor past end
   IF Curpo > Length AND Insrt <> 0 THEN GOSUB TInsertOff

   IF Curpo > Max GOTO TEnter           'field is filled, handle as Enter key

   A$ = INKEY$
   IF A$ = "" GOTO TGetKey
   IF LEN(A$) = 1 GOTO TRegularKey

   A$ = RIGHT$(A$, 1)                   'it was an extended key, get the code

   ON INSTR(CHR$(15) + ".GHKMOPRSstu" + CHR$(19), A$) GOTO TShiftTab, TClear, THome, TUp, TLeft, TRight, TEndKey, TDown, TIns, TDel, TCtrlLeft, TCtrlRight, TCtrlEnd, TRestore

   GOTO TGetKey                         'none of the above, get again



TShiftTab:

   ExitCode = 1                         'user wants to go back a field
   GOTO TEnter                          'handle as if it were the Enter key



TClear:

   X$ = ""                              'Alt-C, erase the current string
   GOSUB TInsertOff                     'clear insert mode and restore cursor
   LOCATE , POS(0) - (Curpo - 1)
   GOTO TC                              'and start all over again



THome:

   LOCATE , POS(0) - (Curpo - 1)        'put cursor at beginning of line
   Curpo = 1                            'show cursor as being on 1st character
   GOTO TGetKey



TUp:

   ExitCode = 1                         'user wants to go back a field
   GOTO TEnter                          'handle as if it were the Enter key



TLeft:

   IF Curpo = 1 GOTO TShiftTab          'cursor is on the first character,
                                        'handle as if it were a Shift-Tab
   Curpo = Curpo - 1                    'update Curpo
   LOCATE , POS(0) - 1                  'back up the cursor
   GOTO TGetKey



TRight:

   Curpo = Curpo + 1                    'update Curpo
   LOCATE , POS(0) + 1                  'advance the cursor on the screen
   GOTO TGetKey



TEndKey:

   LOCATE , POS(0) + (Length - Curpo) + 1 'put cursor at the end of the line
   Curpo = Length + 1                   'update Curpo
   GOTO TGetKey



TDown:

   GOTO TEnter



TIns:

   IF Insrt THEN                        'insert is already on, turn it off
      GOSUB TInsertOff
      GOTO TGetKey
   END IF

   IF Curpo > Length GOTO TGetKey       'ignore Ins if cursor is past the end
   IF Length = Max GOTO TGetKey         'also ignore if field is full

   Insrt = 1                            'set the insert flag
   IF Clr THEN                          'set cursor size according to display
      LOCATE , , , 0, 7
   ELSE
      LOCATE , , , 0, 13
   END IF

   GOTO TGetKey



TDel:

   IF Curpo > Length GOTO TGetKey       'ignore Del if cursor is past end

   'slide all characters left one position, add a trailing space and re-print
   MID$(X$, Curpo) = MID$(X$, Curpo + 1) + " "
   QPrint MID$(X$, Curpo), Colr, -1

   Length = Length - 1                  'show string as one character shorter
   GOTO TGetKey



TCtrlLeft:

   IF Curpo = 1 GOTO TGetKey            'at the beginning, ignore

   A = Curpo                            'save cursor position

   'we're within a word, find beginning
   IF MID$(X$, Curpo - 1, 1) <> " " GOTO TSeekLeft2

TSeekLeft1:

   IF Curpo = 1 GOTO TCtrlLeftExit      'at the beginning, give up

   IF MID$(X$, Curpo - 1, 1) = " " THEN
      Curpo = Curpo - 1
      GOTO TSeekLeft1                   'seek previous non-blank character
   END IF

TSeekLeft2:

   IF Curpo = 1 GOTO TCtrlLeftExit      'at the beginning, give up
   IF MID$(X$, Curpo - 1, 1) <> " " THEN
      Curpo = Curpo - 1
      GOTO TSeekLeft2                   'seek character preceeded by a blank
   END IF

TCtrlLeftExit:

   LOCATE , POS(0) - (A - Curpo)        'position the cursor
   GOTO TGetKey



TCtrlRight:

   A = Curpo                            'save cursor position

TSeekRight1:

   IF A > Length GOTO TGetKey           'at the end, give up

   IF MID$(X$, A, 1) <> " " THEN
      A = A + 1                         'consider next character
      GOTO TSeekRight1                  'seek next blank space
   END IF

TSeekRight2:

   IF A > Length GOTO TGetKey           'at the end, give up

   IF MID$(X$, A, 1) = " " THEN
      A = A + 1                         'consider next character
      GOTO TSeekRight2                  'seek next non-blank character
   END IF

   LOCATE , POS(0) + (A - Curpo)        'position the cursor

   Curpo = A                            'show cursor as being on the next word
   GOTO TGetKey                         'get another keypress




TCtrlEnd:

   IF Curpo > Length GOTO TGetKey       'cursor is past the end, ignore

   QPrint SPACE$(Length - Curpo + 1), Colr, -1  'blank from cursor to the end
   MID$(X$, Curpo) = SPACE$(Length - Curpo + 1) 'clear that part of the string
   Length = Curpo - 1                   'show the length being at the cursor
   GOTO TGetKey                         'get another keypress



TRestore:

   LOCATE , POS(0) - (Curpo - 1)        'locate cursor at beginning of line,
   GOTO TInitialize                     '  and start all over again



TRegularKey:

   IF A$ < " " THEN                     'a control key
      ON INSTR(CHR$(8) + CHR$(9) + CHR$(13) + CHR$(27), A$) GOTO TBackspace, TTabKey, TEnter, TEscape
      GOTO TGetKey                      'none of the above
   END IF

   IF CapsOn THEN                       'convert to upper case if requested
      IF A$ >= "a" AND A$ <= "z" THEN A$ = CHR$(ASC(A$) AND 95)
   END IF

   IF NumOnly THEN                      'disallow non-numeric if requested
      IF A$ < "0" OR A$ > "9" THEN
         PLAY "L16O3EC"
         GOTO TGetKey
      END IF
   END IF

   QPrint A$, Colr, -1                  'print character
   LOCATE , POS(0) + 1
   Curpo = Curpo + 1                    'show cursor being ahead

   IF Insrt GOTO THandleInsert

   MID$(X$, Curpo - 1, 1) = A$          'assign the character

   'cursor is past end, increase length
   IF Curpo > Length + 1 THEN Length = Curpo - 1

   'field complete, handle as Enter key
   IF Length = Max AND Curpo > Length GOTO TEnter

   GOTO TGetKey



THandleInsert:

   Length = Length + 1                  'show string being 1 character longer
   Temp$ = MID$(X$, Curpo - 1)          'using Temp$ avoids a BASIC 7 bug
   MID$(X$, Curpo) = Temp$              'move characters one position ahead

   MID$(X$, Curpo - 1, 1) = A$          'assign the current character
   QPrint MID$(X$, Curpo, Length - Curpo + 1), Colr, -1 're-print X$

   IF Length = Max GOTO TEnter          'field complete, handle as Enter key
   GOTO TGetKey



TBackspace:

   IF Curpo = 1 GOTO TGetKey            'can't back up any more, ignore
   Curpo = Curpo - 1                    'show cursor being 1 character before
   LOCATE , POS(0) - 1                  'back up the cursor
   GOTO TDel                            'handle as if it were the Delete key



TTabKey:                                'reserved for your Tab routine if you
                                        '  want to handle it differently
TEnter:

   GOSUB TInsertOff                     'clear insert, restore cursor size
   X$ = LEFT$(X$, Length)               'retain only the current length

   T$ = X$                              'assign the string
   LOCATE , , 0                         'turn off the cursor
   EXIT SUB



TEscape:

   ExitCode = 2                         'show that the user pressed Escape
   GOTO TEnter                          'handle as if it were the Enter Key



TInsertOff:

   'clear Insert mode and restore cursor, depending on monitor type

   Insrt = 0
   IF Clr THEN
      LOCATE , , , 6, 7
   ELSE
      LOCATE , , , 12, 13
   END IF
   RETURN

END SUB

'----- entry parameters
'
'YN$ = string to be input or edited (use the name of your choice)
'Colr is the combined foreground and background colors that will be used
'
'----- exit parameters
'
'YN$  = final edited amount
'ExitCode indicates how editing was terminated -
'   0 = Enter, Tab, Down-Arrow, Right-Arrow past end, or field filled
'   1 = Shift-Tab, Up-Arrow, or Left-Arrow past beginning
'   2 = Escape key pressed
'
'----- local variables
'
'X$ is a string version of the number while it is being input or edited
'
STATIC SUB YesNoB (YN$, ExitCode, Colr)

   ExitCode = 0                                 'initialize flag

   X$ = YN$
   IF X$ = "" THEN X$ = " "
   QPrint X$, Colr, -1
   LOCATE , , 1



YGetKey:

   CapNum                                       'print Cap/Num if active
   X$ = INKEY$
   IF X$ = "" GOTO YGetKey

   IF UCASE$(X$) = "Y" OR UCASE$(X$) = "N" GOTO YDone

   IF X$ = CHR$(9) OR X$ = CHR$(13) GOTO YD2    'Tab or Enter
   IF X$ = CHR$(27) THEN                        'Escape
      ExitCode = 2
      GOTO YD2
   END IF

   IF LEN(X$) = 2 THEN                          'extended key
      X$ = RIGHT$(X$, 1)
      IF X$ = "P" OR X$ = "M" THEN GOTO YD2     'Down arrow, Right arrow
      IF X$ = "H" OR X$ = "K" OR X$ = CHR$(15) THEN
         ExitCode = 1                           'Up, Left, Shift-Tab
         GOTO YD2
      END IF
   END IF

   GOTO YGetKey



YDone:

   IF X$ > "Y" THEN X$ = CHR$(ASC(X$) - 32)     'capitalize if necessary
   YN$ = X$
   QPrint YN$, Colr, -1

YD2:

   LOCATE , , 0

END SUB

