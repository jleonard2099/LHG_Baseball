DEFINT A-Z
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB CSMsgBox (Message$, Wdth, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB KILLFILE (FILENAME$)
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION EXIST (FILENAME$)
  DIM O1(40), TM$(40), JS(46, 42) AS SINGLE, ZM%(46), NM$(46)
  DIM H1$(40), PS$(40), D$(2)
  DIM BA$(40), DFT(40, 20) AS SINGLE, ER$(40), OFT(40, 20) AS SINGLE
  DIM O2(40), Z2$(255), Z3$(255), Z1(255, 2) AS SINGLE, BAT(24), PIT(23)
  DIM J AS SINGLE
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SetCnf.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%

4 OPEN "DEV.BAS" FOR INPUT AS #2
  FOR I = 0 TO 2
  INPUT #2, D$(I)
  NEXT
  CLOSE 2
5 CLS
  APP = 0
  IF UCASE$(LEFT$(D$(2), 1)) <> "A" AND UCASE$(LEFT$(D$(2), 1)) <> "B" THEN 4000
  PRINT "REMOVE STANDINGS AND LEAGUE LEADERS PROGRAM DISK"
  PRINT
  PRINT "INSERT DISK CONTAINIG COMPILED STATS!!": LOCATE 23, 1
  PRINT "PRESS ANY KEY TO CONTINUE"
6 A$ = INKEY$: IF A$ = "" THEN 6

4000 Spec$ = D$(2) + "*.LIB"
    Count% = FCount%(Spec$)
   IF Count% = 0 THEN 4500
   REDIM F$(0 TO Count%)
   FOR X = 1 TO Count%
       F$(X) = SPACE$(12)
   NEXT
   F$(0) = Spec$
   CALL ReadFile(VARPTR(F$(0)))
   CALL SortStr(VARPTR(F$(1)), Count%, 0)

1310 CLS
    COLOR , 1
    CLS
    LOCATE , 28
    COLOR 14
    PRINT "SELECT LEAGUE TO ERASE": COLOR 15
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
    PRINT "-SELECTS LEAGUE   ";
    COLOR 14
    PRINT "HIT ESCAPE KEY TO EXIT";
    COLOR 15
CALL SHOWCURSOR
MAMenu VARPTR(F$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN 99
     F$ = F$(SELECTION%)
   LOCATE 2, 33: PRINT F$: LOCATE , 33: PRINT "ANY CHANGE (YN)";
     COLOR 15, 0
3910 I$ = INKEY$: IF I$ = "" THEN 3910
    IF UCASE$(I$) = "Y" THEN 1310
3911 F$ = QPRTrim$(F$)
     WE$ = LEFT$(F$, LEN(F$) - 4)
     LH$ = LEFT$(WE$, 8)
15 CLS


   LOCATE 10, 8
   CALL CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Cnf)
20 I$ = INKEY$: IF I$ = "" THEN 20
     CALL CSMsgBox("", 0, Cnf)
     IF I$ <> "Y" THEN 1310

30 OPEN D$(2) + LH$ + ".TMS" FOR INPUT AS #2
   INPUT #2, LG$, D1$, D2$, D3$, D4$
   FOR XW = 1 TO 40
35 INPUT #2, H1$(XW)
   NEXT
   CLOSE 2
71 FOR XW = 1 TO 40
    IF H1$(XW) = CHR$(42) THEN 74
801 PRINT "DELETING "; H1$(XW); " STATS"
    CALL KILLFILE(D$(1) + H1$(XW))
    CALL KILLFILE(D$(1) + H1$(XW) + "R")
    CALL KILLFILE(D$(1) + H1$(XW) + "S")
    CALL KILLFILE(D$(1) + H1$(XW) + "D")
74 NEXT
    CLS
    GOTO 4000
99 IF DRIVE >= 67 THEN 103
    CLS : PRINT : PRINT : PRINT
    PRINT "MAKE SURE GAME DISK IS IN DRIVE"
100 PRINT : PRINT "PRESS ANY KEY TO CONTINUE!!"
101 A$ = INKEY$
102 IF A$ = "" THEN 101
103 RUN "HELLO.EXE"

4500 LOCATE 10, 8
     CALL CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 99

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

