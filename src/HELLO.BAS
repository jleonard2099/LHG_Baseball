DECLARE SUB GETKEY ()
DECLARE SUB TITLE ()
   DEFINT A-Z
   DECLARE SUB MMenuVert (BYVAL Address%, Choice%, Start%, SCANCODE%, NormClr%, HiLite%, NumRows%, Row%, Column%)
   DECLARE SUB Box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, Colr%)
   DECLARE FUNCTION GetDrive% ()
   DECLARE SUB TextIn (T$, Max, NumOnly, CapsOn, ExitCode, Colr)
   DECLARE SUB YesNoB (YN$, ExitCode, Colr)
   DECLARE SUB ClearEOL (Colr)
   DECLARE FUNCTION FUsing$ (Number$, Mask$)
   DECLARE FUNCTION OneColor% (FG, BG)
   DECLARE FUNCTION Peek1% (Segment, Address)
   DECLARE SUB QPrint (X$, Colr, Page)
   DECLARE FUNCTION QPTRIM$ (X$)
   DECLARE SUB CapNum ()
   DIM D$(3), ITEM$(15)
   CALL INITMOUSE(X)
   CALL showcursor
   DRIVE = GetDrive%
   DEF FnExt$ (X$, Length) = X$ + SPACE$(Length - LEN(X$))  'for display only
   OPEN "DEV.BAS" FOR INPUT AS #1
   FOR I = 0 TO 3
   INPUT #1, D$(I)
   NEXT
   CLOSE 1
1  CALL TITLE
   ITEM$(1) = "PLAY A GAME                     "
   ITEM$(2) = "ENTER NEW/CHANGE EXISTING TEAMS "
   ITEM$(3) = "PROGRAMMABLE MANAGER            "
   ITEM$(4) = "AUTO SCHEDULER/BOXSCORE OPTIONS "
   ITEM$(5) = "DRAFT A TEAM                    "
   ITEM$(6) = "TRADE PLAYERS                   "
   ITEM$(7) = "VIEW/PRINT COMPILED STATS       "
   ITEM$(8) = "MODIFY REPLAY STATS             "
   ITEM$(9) = "LEAGUE LEADERS AND STANDINGS    "
   ITEM$(10) = "LEAGUE UTILITIES                "
   ITEM$(11) = "ACTIVATE/DEACTIVATE PLAYERS     "
   ITEM$(12) = "GROUNDBALL/FLYOUT CALCS         "
   ITEM$(13) = "HARD DRIVE INSTALLATION         "
   ITEM$(14) = "CONFIGURE DRIVES FOR TEAMS,STATS"
   ITEM$(15) = "EXIT TO DOS                     "
     SELECTION% = 1
     Count% = 15
     Start% = 1
     SCANCODE% = 15
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 7
     Column% = 25
     CALL Box0(6, 24, 22, 57, 2, 31)
     CALL showcursor
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN END

    IF SELECTION% = 1 THEN GOSUB 275: RUN "LOADER.EXE"
    IF SELECTION% = 2 THEN GOSUB 251: RUN "BINPUT.EXE"
    IF SELECTION% = 3 THEN GOSUB 251: RUN "PROGMAN.EXE"
    IF SELECTION% = 4 THEN GOSUB 251: RUN "SCHEDULE.EXE"
    IF SELECTION% = 5 THEN GOSUB 252: RUN "DRAFT.EXE"
    IF SELECTION% = 6 THEN GOSUB 252: RUN "TRADE.EXE"
    IF SELECTION% = 7 THEN GOSUB 252: RUN "LOOKY.EXE"
    IF SELECTION% = 8 THEN GOSUB 252: RUN "SINPUT.EXE"
    IF SELECTION% = 9 THEN 700
    IF SELECTION% = 10 THEN 800
    IF SELECTION% = 11 THEN GOSUB 251: RUN "ACTIVATE.EXE"
    IF SELECTION% = 12 THEN GOSUB 251: RUN "GROUND.EXE"
    IF SELECTION% = 13 THEN GOSUB 252: RUN "HDCOPY.EXE"
    IF SELECTION% = 14 THEN 505
    IF SELECTION% = 15 THEN CLS : END

250 IF DRIVE >= 67 THEN 257
    CLS : PRINT "INSERT PROGRAM DISK 3"
    CALL GETKEY
    RETURN

251 IF DRIVE >= 67 THEN 257
    CLS : PRINT "INSERT PROGRAM DISK 4"
    CALL GETKEY
    RETURN

252 IF DRIVE >= 67 THEN 257
    CLS : PRINT "INSERT PROGRAM DISK 5"
    CALL GETKEY
257 RETURN

253 IF DRIVE >= 67 THEN 257
    CLS : PRINT "INSERT PROGRAM DISK 6"
    CALL GETKEY
    RETURN

275 IF DRIVE >= 67 THEN 278
    CLS : PRINT "INSERT PROGRAM DISK # 2"
276 PRINT "PRESS ANY KEY TO CONTINUE"
277 I$ = INKEY$: IF I$ = "" THEN 277
278 RETURN

300 IF DRIVE >= 67 THEN 307
    CLS : PRINT "INSERT LEAGUE UTILITIES DISK"
305 PRINT : PRINT : PRINT "PRESS ANY KEY TO CONTINUE"
306 I$ = INKEY$: IF I$ = "" THEN 306
307 RETURN
350 IF DRIVE >= 67 THEN 307
    CLS : PRINT "INSERT CURRENT TEAMS DISK"
    GOTO 305
505 COLOR 15, 1
      FG = 0
      BG = 7
      FOR I = 0 TO 3
      D$(I) = LEFT$(D$(I), LEN(D$(I)) - 1)
      NEXT
   Colr = OneColor%(FG, BG)             'pack FG and BG into a single byte
    CLS : LOCATE , 16: PRINT "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": PRINT
510 LOCATE , 31: PRINT "PATHNAME EXAMPLES:": PRINT
515 LOCATE , 31: PRINT "A:"
520 LOCATE , 31: PRINT "C:\FULL\TEAMS"

    LOCATE 7, 1: QPrint "Path for Teams ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(0), 30), Colr, -1
    LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
    LOCATE , 16: QPrint FnExt$(D$(1), 30), Colr, -1
    LOCATE 11, 1: QPrint "Path for Leaders ", 30, -1
    LOCATE , 18: QPrint FnExt$(D$(2), 30), Colr, -1
    LOCATE 13, 1: QPrint "Path for Manager Profiles ", 30, -1
    LOCATE , 27: QPrint FnExt$(D$(3), 30), Colr, -1

GetDiv1:

   LOCATE 7, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
   CALL TextIn(D$(0), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
   IF XCode = 2 GOTO Done               'all done entering data
   D$(0) = QPTRIM$(D$(0))
GetDiv2:

   LOCATE 9, 16                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
   CALL TextIn(D$(1), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv1:           'can't back up any more, stay here
   IF XCode = 2 GOTO Done               'all done entering data
   D$(1) = QPTRIM$(D$(1))
GetDiv3:

   LOCATE 11, 18                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30    'specify field parameters
   CALL TextIn(D$(2), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv2:           'can't back up any more, stay here
   IF XCode = 2 GOTO Done               'all done entering data
   D$(2) = QPTRIM$(D$(2))
GetDiv4:

   LOCATE 13, 27                         'location of beginning of field
   CapsOn = 0: NumOnly = 0: Max = 30     'specify field parameters
   CALL TextIn(D$(3), Max, NumOnly, CapsOn, XCode, Colr)        'get input
   IF XCode = 1 GOTO GetDiv3:           'can't back up any more, stay here
   D$(3) = QPTRIM$(D$(3))
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

580 OPEN "DEV.BAS" FOR OUTPUT AS #1
585 FOR R = 0 TO 3: D$(R) = D$(R) + "\": NEXT R
590 PRINT #1, D$(0)
595 PRINT #1, D$(1)
    PRINT #1, D$(2)
    PRINT #1, D$(3)
600 CLOSE #1

610 IF DRIVE >= 67 THEN 620
    FOR I = 2 TO 6
    CLS
    PRINT "INSERT PROGRAM DISK "; STR$(I); " AND HIT ANY KEY"
    CALL GETKEY
    OPEN "DEV.BAS" FOR OUTPUT AS #1
    PRINT #1, D$(0)
    PRINT #1, D$(1)
    PRINT #1, D$(2)
    PRINT #1, D$(3)
    CLOSE #1
    NEXT
    CLS : PRINT "INSERT GAME DISK AND HIT ANY KEY"
    CALL GETKEY
620 GOTO 1

700 CALL TITLE
   ITEM$(1) = "CREATE/EDIT A LEAGUE                 "
   ITEM$(2) = "UPDATE STANDINGS/LEADERS FOR A LEAGUE"
   ITEM$(3) = "EXPANDED LEADERS AND STANDINGS       "
   ITEM$(4) = "VIEW RESULTS FOR A LEAGUE            "
   ITEM$(5) = "PRINT RESULTS FOR A LEAGUE           "
   ITEM$(6) = "DELETE ALL STATS FOR A LEAGUE        "
   ITEM$(7) = "RETURN TO FULL COUNT MAIN MENU       "
   ITEM$(8) = "EXIT TO DOS                          "
     SELECTION% = 1
     Count% = 8
     Start% = 1
     SCANCODE% = 8
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 23
     CALL Box0(7, 22, 16, 60, 2, 31)
     CALL showcursor
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN 1
    IF SELECTION% = 1 THEN GOSUB 250: RUN "CREATE.EXE"
    IF SELECTION% = 2 THEN GOSUB 250: RUN "LEAGCMPL.EXE"
    IF SELECTION% = 3 THEN GOSUB 250: RUN "NEWLDR.EXE"
    IF SELECTION% = 4 THEN GOSUB 250: RUN "SEELEAG.EXE"
    IF SELECTION% = 5 THEN GOSUB 250: RUN "PRTLEAG.EXE"
    IF SELECTION% = 6 THEN GOSUB 250: RUN "BLANK.EXE"
    IF SELECTION% = 7 THEN 1
    IF SELECTION% = 8 THEN CLS : END

800 CALL TITLE
   ITEM$(1) = "COMPARE COMPILED STATS VS. ACTUAL"
   ITEM$(2) = "MERGE TWO STAT FILES             "
   ITEM$(3) = "VIEW TEAM HEAD TO HEAD RESULTS   "
   ITEM$(4) = "CAREER FILE OPTIONS              "
   ITEM$(5) = "SORT PLAYERS BY POSITION         "
   ITEM$(6) = "RETURN TO FULL COUNT MAIN MENU   "
     SELECTION% = 1
     Count% = 6
     Start% = 1
     SCANCODE% = 6
     FileColr% = 31
     HiLiteColr% = 112
     Spacing% = 1
     Row% = 8
     Column% = 23
     CALL Box0(7, 22, 14, 56, 2, 31)
     CALL showcursor
     MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileColr%, HiLiteColr%, Count%, Row%, Column%
     CALL HideCursor
     COLOR , 0
     IF SCANCODE% = 27 THEN 1
    IF SELECTION% = 1 THEN GOSUB 252: RUN "COMPARE.EXE"
    IF SELECTION% = 2 THEN GOSUB 253: RUN "MERGE.EXE"
    IF SELECTION% = 3 THEN GOSUB 252: RUN "REC.EXE"
    IF SELECTION% = 4 THEN GOSUB 253: RUN "CAREER.EXE"
    IF SELECTION% = 5 THEN GOSUB 253: RUN "POS.EXE"
    IF SELECTION% = 6 THEN 1

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

SUB GETKEY ()
LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
20 I$ = INKEY$: IF I$ = "" THEN 20
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

SUB TITLE ()
   COLOR , 0: KEY OFF: CLS
   COLOR , 1: LOCATE 1, 1: PRINT TAB(80); : LOCATE , 30: COLOR 12
   PRINT "FULL COUNT BASEBALL 6.51"
   LOCATE 24, 1: PRINT TAB(80); : LOCATE , 1
   PRINT "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; TAB(70); LEFT$(TIME$, 5);
   LOCATE 3, 1: COLOR 2, 0
   COLOR 15: PRINT "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
             PRINT "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR ecciLR@AOL.COM"
             PRINT "      LANCE HAFFNER GAMES ON THE INTERNET AT  HTTP://WWW.LHGAMES.COM"

   LOCATE 12, 1
   PRINT "LANCE HAFFNER GAMES"
   PRINT "P.O. BOX 100594"
   PRINT "NASHVILLE, TN 37214"
   COLOR 10
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

