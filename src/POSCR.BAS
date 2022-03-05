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
DIM K9(1) AS LONG, D$(4), Q$(1844), FS%(22, 4, 4)
DIM B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
DIM P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21)
DIM TM(40), T$(40), TYN$(40), VV$(200), V%(200)
DIM Y AS LONG
   FOR I = 1 TO 40
   TM(I) = 0
   T$(I) = ""
   TYN$(I) = ""
   NEXT
   OPEN "DEV.BAS" FOR INPUT AS #2
   FOR I = 0 TO 3
   INPUT #2, D$(I)
   NEXT
16 CLOSE #2
21 CLS
30  COLOR 15, 0: CLS
    JJ = 7
    LOCATE 6, 8: COLOR 15, 1
    PRINT " Ö"; STRING$(61, 196); "· "; : FOR II = 1 TO JJ: LOCATE 6 + II, 8
    PRINT " º"; STRING$(61, 32); "º "; : NEXT
    LOCATE 7 + JJ, 8
    PRINT " Ó"; STRING$(61, 196); "½ "; : LOCATE 7, 32
    LOCATE , 12: PRINT "ENTER NAME FOR POSITION FILE: ": PRINT
    LOCATE , 12: PRINT "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
    LOCATE , 12: PRINT "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
    LOCATE , 12: PRINT "WILL AUTOMATICALLY ADD .POS"
    LOCATE , 12: PRINT "SAMPLE NAMES ARE AL-1965, DRAFT, ETC"
    LOCATE 7, 41: INPUT LGE$
    IF LGE$ = "" THEN 21
    LOCATE 13, 12: PRINT "ANY CHANGE (YN) "
22 I$ = INKEY$: IF I$ = "" THEN 22
   IF UCASE$(I$) = "Y" THEN 30
   COLOR 15, 0
36 CLS
50 COLOR 15, 0: CLS
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
1441 GOSUB 1105
182 IF BO% = 1 THEN BO% = 0: GOTO 50
183 LOCATE 2, 33: COLOR 15, 0: COLOR L%(11), L%(12): PRINT T$: COLOR 15, 4
    LOCATE , 33: PRINT "ANY CHANGE (YN)"; : I$ = INKEY$: IF I$ = "" THEN 183
    IF UCASE$(I$) = "Y" THEN 50
112 J = J + 1
200 TM(J) = T: T$(J) = T$: TYN$(J) = YN$
300 COLOR 15, 0
    CLS
    LOCATE 7: COLOR 12: PRINT "1.  "; : COLOR 7: PRINT T$(1);
    LOCATE , 27: COLOR 12: PRINT "2.  "; : COLOR 7: PRINT T$(2);
    LOCATE , 54: COLOR 12: PRINT "3.  "; : COLOR 7: PRINT T$(3)
    LOCATE 8: COLOR 12: PRINT "4.  "; : COLOR 7: PRINT T$(4);
    LOCATE , 27: COLOR 12: PRINT "5.  "; : COLOR 7: PRINT T$(5);
    LOCATE , 54: COLOR 12: PRINT "6.  "; : COLOR 7: PRINT T$(6)
    LOCATE 9: COLOR 12: PRINT "7.  "; : COLOR 7: PRINT T$(7);
    LOCATE , 27: COLOR 12: PRINT "8.  "; : COLOR 7: PRINT T$(8);
    LOCATE , 54: COLOR 12: PRINT "9.  "; : COLOR 7: PRINT T$(9)
    LOCATE 10: COLOR 12: PRINT "10. "; : COLOR 7: PRINT T$(10);
    LOCATE , 27: COLOR 12: PRINT "11. "; : COLOR 7: PRINT T$(11);
    LOCATE , 54: COLOR 12: PRINT "12. "; : COLOR 7: PRINT T$(12)
    LOCATE 11: COLOR 12: PRINT "13. "; : COLOR 7: PRINT T$(13);
    LOCATE , 27: COLOR 12: PRINT "14. "; : COLOR 7: PRINT T$(14);
    LOCATE , 54: COLOR 12: PRINT "15. "; : COLOR 7: PRINT T$(15)
    LOCATE 12: COLOR 12: PRINT "16. "; : COLOR 7: PRINT T$(16);
    LOCATE , 27: COLOR 12: PRINT "17. "; : COLOR 7: PRINT T$(17);
    LOCATE , 54: COLOR 12: PRINT "18. "; : COLOR 7: PRINT T$(18)
    LOCATE 13: COLOR 12: PRINT "19. "; : COLOR 7: PRINT T$(19);
    LOCATE , 27: COLOR 12: PRINT "20. "; : COLOR 7: PRINT T$(20);
    LOCATE , 54: COLOR 12: PRINT "21. "; : COLOR 7: PRINT T$(21)
    LOCATE 14: COLOR 12: PRINT "22. "; : COLOR 7: PRINT T$(22);
    LOCATE , 27: COLOR 12: PRINT "23. "; : COLOR 7: PRINT T$(23);
    LOCATE , 54: COLOR 12: PRINT "24. "; : COLOR 7: PRINT T$(24)
    LOCATE 15: COLOR 12: PRINT "25. "; : COLOR 7: PRINT T$(25);
    LOCATE , 27: COLOR 12: PRINT "26. "; : COLOR 7: PRINT T$(26);
    LOCATE , 54: COLOR 12: PRINT "27. "; : COLOR 7: PRINT T$(27)
    LOCATE 16: COLOR 12: PRINT "28. "; : COLOR 7: PRINT T$(28);
    LOCATE , 27: COLOR 12: PRINT "29. "; : COLOR 7: PRINT T$(29);
    LOCATE , 54: COLOR 12: PRINT "30. "; : COLOR 7: PRINT T$(30)
    LOCATE 17: COLOR 12: PRINT "31. "; : COLOR 7: PRINT T$(31);
    LOCATE , 27: COLOR 12: PRINT "32. "; : COLOR 7: PRINT T$(32);
    LOCATE , 54: COLOR 12: PRINT "33. "; : COLOR 7: PRINT T$(33)
    LOCATE 18: COLOR 12: PRINT "34. "; : COLOR 7: PRINT T$(34);
    LOCATE , 27: COLOR 12: PRINT "35. "; : COLOR 7: PRINT T$(35);
    LOCATE , 54: COLOR 12: PRINT "36. "; : COLOR 7: PRINT T$(36)
    LOCATE 19: COLOR 12: PRINT "37. "; : COLOR 7: PRINT T$(37);
    LOCATE , 27: COLOR 12: PRINT "38. "; : COLOR 7: PRINT T$(38);
    LOCATE , 54: COLOR 12: PRINT "39. "; : COLOR 7: PRINT T$(39)
    LOCATE 20: COLOR 12: PRINT "40. "; : COLOR 7: PRINT T$(40)
500 LOCATE 22: PRINT "ENTER ANOTHER TEAM (Y/N)"
501 I$ = INKEY$: IF I$ = "" THEN 501
502 IF UCASE$(I$) = "Y" THEN 50
540 GOSUB 1100
550 OPEN D$(0) + LGE$ + ".POS" FOR OUTPUT AS #2
551 FOR J = 1 TO 40: WRITE #2, TM(J), T$(J), TYN$(J): NEXT
552 CLOSE #2
600 IF DRIVE >=67 THEN RUN "POS.EXE"
    CLS : PRINT "MAKE SURE PROGRAM DISK #6 IS IN DRIVE"
    LOCATE 23, 1: PRINT "PRESS ANY KEY TO CONTINUE"
605 I$ = INKEY$: IF I$ = "" THEN 605
    RUN "POS.EXE"
1100 CLS
     COLOR 15
     IF UCASE$(LEFT$(D$(0), 1)) <> "A" AND UCASE$(LEFT$(D$(0), 1)) <> "B" THEN 1110
     PRINT "MAKE SURE DISK WITH STAT FILES IS IN DRIVE "; : COLOR 12: PRINT D$(0)
1101 COLOR 15
     CALL GETKEY
1110 RETURN

4500 LOCATE 10, 8
     CALL CSMsgBox("NO TEAM FILE FOUND MATCHING ID... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Cnf)
3805 I$ = INKEY$: IF I$ = "" THEN 3805
     CALL CSMsgBox("", 0, Cnf)
     GOTO 600
1105 NT = 0
     IF NOT EXIST%(D$(0) + "FCTEAMS." + YN$) THEN BO% = 1: RETURN
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
CALL SHOWCURSOR
MAMenu VARPTR(VV$(1)), SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%
CALL HIDECURSOR
     IF ExitCode% = 27 THEN BO% = 1: RETURN
     T = V%(SELECTION%)
     LOCATE 2, 33: COLOR 31: PRINT "LOADING..."
5500 OPEN D$(0) + "FCTEAMS." + YN$ FOR RANDOM AS #1 LEN = 4342
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
FOR I = 1 TO 13: L%(I) = CVI(Q$(I)): NEXT: K9(0) = CVS(Q$(14))
FOR I = 0 TO 22: B$(I) = QPRTrim$(Q$(I * 43 + 15))
FOR I1 = 0 TO 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
NEXT: NEXT
FOR I = 0 TO 21: P$(I) = QPRTrim$(Q$(I * 38 + 1004))
FOR I1 = 0 TO 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
NEXT: NEXT
S$ = QPRTrim$(Q$(1840)): M$ = QPRTrim$(Q$(1841)): NN$ = QPRTrim$(Q$(1842)): CK = CVI(Q$(1843))
     CLOSE 1
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

