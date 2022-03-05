DEFINT A-Z
DECLARE SUB CSMsgBox (Message$, Wdth%, Cnf AS ANY)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB QPrint (X$, Colr, Page)
DECLARE SUB AddInt (SEG AElement, Value, NumEls)
DECLARE FUNCTION EXIST (FILENAME$)
DECLARE FUNCTION QPRTrim$ (Work$)
DECLARE SUB InitInt (SEG AElement, StartValue, NumEls)
DECLARE SUB MPaintBox (ULRow, ULCol, LRRow, LRCol, Colr)
DECLARE SUB MScrnSave (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MScrnRest (ULRow, ULCol, LRRow, LRCol, SEG Address)
DECLARE SUB MAMenu (BYVAL Address%, SELECTION%, Start%, Count%, ExitCode%, FileColr%, HiLiteColr%, Rows%, Columns%, Spacing%, Row%, Column%)
DECLARE FUNCTION GetDrive% ()
DECLARE FUNCTION FCount% (FSpec$)
DECLARE SUB ReadFile (BYVAL Address%)
DECLARE SUB ISortI (SEG Element%, SEG IndexElement%, NumElements%, Direction%)
DECLARE SUB SortStr (BYVAL Address%, Size%, Direction%)
  '$INCLUDE: 'DEFCNF.BI'
  '$INCLUDE: 'SETCNF.BI'          'queries the system info for MsgBox
  DRIVE = GetDrive%
        DIM H(4), DC(3), SC%(3400, 1), SC$(3400, 1), SCE(3400), X%(40, 1), GAME(3400)
        DIM DX(10000)
        DIM X$(40, 1), Z1%(40), Z1$(40), D$(2), Z2$(40)
RANDOMIZE TIMER
  OPEN "DEV.BAS" FOR INPUT AS #2: FOR I = 0 TO 2: INPUT #2, D$(I): NEXT: CLOSE 2
     REM:DISPLAY LEAGUE ON THE SCREEN
4000 Spec$ = D$(2) + "*.LIB"
     Count = FCount%(Spec$)
     IF Count = 0 THEN 4500
     REDIM F$(0 TO Count)
     FOR X = 1 TO Count
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
     IF ExitCode% = 27 THEN END
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
     END
     REM:GOTO 50


25      FOR I = 1 TO 40
        IF I <= 10 AND Z1$(I) <> "XXX" THEN DC(0) = DC(0) + 1
        IF I > 10 AND I <= 20 AND Z1$(I) <> "XXX" THEN DC(1) = DC(1) + 1
        IF I > 20 AND I <= 30 AND Z1$(I) <> "XXX" THEN DC(2) = DC(2) + 1
        IF I > 30 AND I <= 40 AND Z1$(I) <> "XXX" THEN DC(3) = DC(3) + 1
        NEXT

        A = 4
        IF DC(3) = 0 THEN A = 3
        IF DC(2) = 0 THEN A = 2
        IF DC(1) = 0 THEN A = 1
        IF A = 1 THEN B = DC(0)
        IF A = 2 AND DC(0) = DC(1) THEN B = DC(0)
        IF A = 3 AND DC(0) = DC(1) AND DC(1) = DC(2) THEN B = DC(0)
        IF A = 4 AND DC(0) = DC(1) AND DC(1) = DC(2) AND DC(2) = DC(3) THEN B = DC(0)
        IF B < 2 THEN PRINT "NOT ENOUGH TMS IN EACH DIV. MUST BE AT LEAST 2": END
        IF A = 1 OR A = 2 AND DC(0) = DC(1) OR A = 3 AND DC(0) = DC(1) AND DC(1) = DC(2) OR A = 4 AND DC(0) = DC(1) AND DC(1) = DC(2) AND DC(2) = DC(3) THEN 100
        PRINT "ALL FOUR DIV NOT EQUAL SIZE": END
100     CLS
        CALL CSMsgBox("NOW YOU WILL DETERMINE THE NUMBER OF GAMES FOR EACH TEAM IN EACH DIVISION. THE MAXIMUM NUMBER OF GAMES YOU CAN SCHEDULE IS 170 GAMES", 26, Cnf)
105     I$ = INKEY$: IF I$ = "" THEN 105
        CALL CSMsgBox("", 0, Cnf)


        LOCATE 12, 1: PRINT "# OF TIMES PLAY TEAMS IN OWN DIV : "; : INPUT C
        LOCATE 13, 1: PRINT "# OF TIMES PLAY TEAMS IN OTHER DIV : "; : INPUT D
        LOCATE 15, 1: PRINT "THE # OF GAMES FOR EACH TEAM IS : "; :
        PRINT C * (B - 1) + (A - 1) * B * D
        IF C * (B - 1) + (A - 1) * B * D > 170 THEN 100
        CLS : PRINT "NOW YOU WILL DETERMINE THE NUMBER OF DIVISION HOME "
        PRINT "GAMES AGAINST EACH TEAM IN THE DIVISION"
        LOCATE 5, 1: PRINT "0 - # OF 1 GAME SERIES              : "; : INPUT E
        LOCATE 6, 1: PRINT "1 - # IF 2 GAME SERIES              : "; : INPUT F
        LOCATE 7, 1: PRINT "2 - # OF 3 GAME SERIES              : "; : INPUT G
        LOCATE 8, 1: PRINT "3 - # OF 4 GAME SERIES (NO DBL HDR) : "; : INPUT H
        LOCATE 9, 1: PRINT "4 - # OF 4 GAME SERIES (DBL HDR)    : "; : INPUT I
        PRINT "TOTAL # OF DIVISION HOME GAMES "; ((C * (B - 1)) / 2)
        PRINT "TOTAL YOU HAVE SCHEULED "; (E + (F * 2) + (G * 3) + ((H + I) * 4)) * (B - 1)
        PRINT "YOU STILL NEED TO SCHEDULE ";
        PRINT "ANY CHANGE ROUTINE"
110 I$ = INKEY$: IF I$ = "" THEN 110
        REM:  "NON - DIV HOME GAMES"

        CLS : PRINT "NOW YOU WILL DETERMINE THE NUMBER OF NON-DIV HOME GAMES"
        PRINT "AGAINST EACH TEAM IN EACH DIVISION"
        LOCATE 5, 1: PRINT "0 - # OF 1 GAME SERIES              : "; : INPUT J
        LOCATE 5, 1: PRINT "1 - # OF 2 GAME SERIES              : "; : INPUT K
        LOCATE 7, 1: PRINT "2 - # OF 3 GAME SERIES              : "; : INPUT L
        LOCATE 8, 1: PRINT "3 - # OF 4 GAME SERIES (NO DBL HDR) : "; : INPUT M
        LOCATE 9, 1: PRINT "4 - # OF 4 GAME SERIES (DBL HDR)    : "; : INPUT N
        LOCATE 12, 1: PRINT "TOTAL # OF NON-DIV GAMES "; : PRINT ((A - 1) * B * D) / 2
        LOCATE 13, 1: PRINT "TOTAL YOU HAVE SCHEDULES "; (J + (K * 2) + (L * 3) + ((M + N) * 4)) * B
        LOCATE 14, 1: PRINT "YOU STILL NEED TO SCHEDULE ";
        PRINT ";ANY CHANGE ROUTINE"

        CLS : PRINT "ENTER DATE SEASON TO BEGIN ON :"
        INPUT "XX/XX/XX"; F$
        REM:SELECT C VS C , DH NO DH , RULES ETC.
        REM:;CALC # OF DAYS IN SN.
        IF (B / 2) - INT(B / 2) = 0 THEN SX = C * (B - 1) - ((N + I) * 2): E0 = 0: REM:EVEN # OF TMS/DIV
        IF (B / 2) - INT(B / 2) > 0 THEN SX = C * (B - 1) + C - ((N + I) * 2): E0 = 1: REM;ODD # TMS/DIV
        GD = INT(B / 2): REM ; GAMES/DAY
        SD = 10000
        IF E0 = 0 THEN XX = B - 1: REM ; EVEN # OF TMS FOR ROTATING
        IF E0 = 1 THEN XX = B: REM   ; ODD # OF TMS FOR ROTATING

DODIV:
        DV = 0: GOSUB DIV
        IF A > 1 THEN DV = 10: GOSUB DIV
        IF A > 2 THEN DV = 20: GOSUB DIV
        IF A > 3 THEN DV = 30: GOSUB DIV
        GOTO NONDIV
DIV:
        GOSUB 9000
        PRINT E, F, G, H, I, YY, NN, GM
22 I$ = INKEY$: IF I$ = "" THEN 22
        YY = E: NN = 1: IF YY > 0 THEN GOSUB 9005
        PRINT E, F, G, H, I, YY, NN, GM
23 I$ = INKEY$: IF I$ = "" THEN 23
        YY = F: NN = 2: IF YY > 0 THEN GOSUB 9005
        PRINT E, F, G, H, I, YY, NN, GM
24 I$ = INKEY$: IF I$ = "" THEN 24
        YY = G: NN = 3: IF YY > 0 THEN GOSUB 9005
        PRINT E, F, G, H, I, YY, NN, GM
28 I$ = INKEY$: IF I$ = "" THEN 28
        YY = H: NN = 4: IF YY > 0 THEN GOSUB 9005
        PRINT E, F, G, H, I, YY, NN, GM
26 I$ = INKEY$: IF I$ = "" THEN 26
        YY = I: NN = 4: DH = 1: IF YY > 0 THEN GOSUB 9005
        PRINT E, F, G, H, I, YY, NN, GM
27 I$ = INKEY$: IF I$ = "" THEN 27
        DH = 0
        RETURN
9005 :
        REM:T1 = 1
        FOR QQ = 1 TO YY
        T1 = 1
        PRINT "QQ,A="; QQ, A
        GOSUB 9000
        GOSUB 9010
        NEXT QQ
        RETURN
9010 :
        MM = INT(RND(1) * B) + 1
        IF X%(MM, 0) <> -1 THEN 9010
        X%(MM, 0) = Z1%(T1 + DV): REM ; RANDOMLY LOAD TM# FROM LL FILE
        X$(MM, 0) = Z2$(T1 + DV): REM: ; RANDOMLY LOAD ID# FROM LL FILE
        T1 = T1 + 1: IF T1 > B THEN 500
        GOTO 9010
500  
        PRINT "@500"
        FOR RR = 1 TO B - 1
        IF E0 = 0 THEN X%(B - 1, 0) = X%(1, 0): X$(B - 1, 0) = X$(1, 0)
        IF E0 = 1 THEN X%(B, 0) = X%(1, 0): X$(B, 0) = X$(1, 0): REM ; ODD ROTATE
        FOR DD = 2 TO XX
        X%(DD - 1, 0) = X%(DD, 0): X$(DD - 1, 0) = X$(DD, 0)
        NEXT DD
        
505     PRINT "@505"
        DY = INT(RND(1) * SD) + 1
        PRINT "DY="; DY
        IF DX(DY) > 0 THEN 505
        DX(DY) = 1
        REM:GET HOME DAY #

510     PRINT "@510"
        DY1 = INT(RND(1) * SD) + 1
        PRINT DY1
        IF DX(DY1) > 0 THEN 510
        DX(DY1) = 1

        FOR PP = 1 TO NN
        IF N = 4 AND DH = 1 THEN 520
        DY = DY + 1
        DY1 = DY1 + 1
520
        GM = GM + 1
        PRINT "GM="; GM, B
        SC%(GM, 0) = X%(1, 0): SC$(GM, 0) = X$(1, 0): SC%(GM, 1) = X%(2, 0): SC$(GM, 1) = X$(2, 0)
        SCE(GM) = DY
        GM = GM + 1
        SC%(GM, 0) = X%(2, 0): SC$(GM, 0) = X$(2, 0): SC%(GM, 1) = X%(1, 0): SC$(GM, 1) = X$(1, 0)
        SCE(GM) = DY1
        IF B >= 4 THEN GM = GM + 1: SC%(GM, 0) = X%(3, 0): SC$(GM, 0) = X$(3, 0): SC%(GM, 1) = X%(4, 0): SC$(GM, 1) = X$(4, 0): SCE(GM) = DY
        IF B >= 4 THEN GM = GM + 1: SC%(GM, 0) = X%(4, 0): SC$(GM, 0) = X$(4, 0): SC%(GM, 1) = X%(3, 0): SC$(GM, 1) = X$(3, 0): SCE(GM) = DY1
        IF B >= 6 THEN GM = GM + 1: SC%(GM, 0) = X%(5, 0): SC$(GM, 0) = X$(5, 0): SC%(GM, 1) = X%(6, 0): SC$(GM, 1) = X$(6, 0): SCE(GM) = DY
        IF B >= 6 THEN GM = GM + 1: SC%(GM, 0) = X%(6, 0): SC$(GM, 0) = X$(6, 0): SC%(GM, 1) = X%(5, 0): SC$(GM, 1) = X$(5, 0): SCE(GM) = DY1
        IF B >= 8 THEN GM = GM + 1: SC%(GM, 0) = X%(7, 0): SC$(GM, 0) = X$(7, 0): SC%(GM, 1) = X%(8, 0): SC$(GM, 1) = X$(8, 0): SCE(GM) = DY
        IF B >= 8 THEN GM = GM + 1: SC%(GM, 0) = X%(8, 0): SC$(GM, 0) = X$(8, 0): SC%(GM, 1) = X%(7, 0): SC$(GM, 1) = X$(7, 0): SCE(GM) = DY1
        IF B = 10 THEN GM = GM + 1: SC%(GM, 0) = X%(9, 0): SC$(GM, 0) = X$(9, 0): SC%(GM, 1) = X%(10, 0): SC$(GM, 1) = X$(10, 0): SCE(GM) = DY
        IF B = 10 THEN GM = GM + 1: SC%(GM, 0) = X%(10, 0): SC$(GM, 0) = X$(10, 0): SC%(GM, 1) = X%(9, 0): SC$(GM, 1) = X$(9, 0): SCE(GM) = DY1
        NEXT PP
        NEXT RR
        RETURN
NONDIV:
        IF A = 1 THEN 7000
        GOSUB 9000
        DV = 0: DV1 = 10: GOSUB DOYY
        GOTO DIV3
DO1:
        T1 = 1
        FOR QQ = 1 TO YY
        GOSUB 7010
        NEXT QQ
        RETURN
7010 :
        MM = INT(RND(1) * B) + 1
        IF X%(MM + DV, 0) <> -1 THEN 7010
        X%(MM + DV, 0) = Z1%(T1 + DV)
        X$(MM + DV, 0) = Z2$(T1 + DV)
        T1 = T1 + 1: IF T1 > B THEN T1 = 1: GOTO 9015
        GOTO 7010
9015 :
        MM = INT(RND(1) * B) + 1
        IF X%(MM + DV1, 1) <> -1 THEN 9015
        X%(MM + DV1, 1) = Z1%(T1 + DV1)
        X$(MM + DV1, 1) = Z2$(T1 + DV1)
        T1 = T1 + 1: IF T1 > B THEN T1 = 1: GOTO ROTAT1
        GOTO 7010
ROTAT1:
        XX = B - 1
        FOR RR = 1 TO B
        X%(B, 1) = X%(1, 1): X$(B, 1) = X$(1, 1)
        FOR DD = 2 TO XX
        X%(DD - 1, 1) = X%(DD, 1): X$(DD - 1, 1) = X$(DD, 1): NEXT DD
7020
        DY = INT(RND(1) * SD) + 1
        IF DX(DY) > 0 THEN 7020
        DX(DY) = 1
7030
        DY1 = INT(RND(1) * SD) + 1
        IF DX(DY1) > 0 THEN 7030
        DX(DY1) = 1
        FOR PP = 1 TO NN
        IF NN = 4 AND DH = 1 THEN 7040
        DY = DY + .1
        DY1 = DY1 + .1
7040
        FOR TT = 1 TO B: GM = GM + 1
        SC%(GM, 0) = X%(TT + DV, 0): SC$(GM, 0) = X$(TT + DV, 0)
        SC%(GM, 1) = X%(TT + DV1, 1): SC$(GM, 1) = X$(TT + DV1, 1)
        SCE(GM) = DY
        GM = GM + 1
        SC%(GM, 0) = X%(TT + DV1, 1): SC$(GM, 0) = X$(TT + DV1, 1)
        SC%(GM, 1) = X%(TT + DV, 0): SC$(GM, 1) = X$(TT + DV, 0)
        SCE(GM) = DY1
        NEXT TT
        NEXT PP
        NEXT RR
        RETURN
DIV3:
        IF A <= 2 THEN 7000
        GOSUB 9000
        DV = 0: DV1 = 20: GOSUB DOYY
        GOSUB 9000
        DV = 10: DV1 = 20: GOSUB DOYY
DIV4:
        IF A <= 3 THEN 7000
        GOSUB 9000
        DV = 0: DV1 = 30: GOSUB DOYY
        GOSUB 9000
        DV = 10: DV1 = 30: GOSUB DOYY
        GOSUB 9000
        DV = 20: DV1 = 30: GOSUB DOYY
7000 :
        CLS : PRINT "SORTING SCHEDULE..."
        M = GM: L = GM - 1
        PRINT M, L
SORT1:
        S6 = 0: S7 = 0
SORT2:
        InitInt GAME(1), 0, GM           'initialize the array (base zero)
        ISortI SCE(1), GAME(1), GM, Direction
        AddInt GAME(1), 1, GM            'add one to every element (needed
					'  unless starting at element zero)
        OPEN "SCHED" FOR OUTPUT AS #2
        FOR I = 1 TO GM
REM:        PRINT #2, SCE(I), SC%(I, 0), SC%(I, 1), SC$(I, 0), SC$(I, 1)
        PRINT #2, GAME(I), SCE(GAME(I)), SCE(I)
        NEXT
        CLOSE 2

        END
8020
        FOR I = 1 TO GM
        D = D + 100: X = INT(D / 100)
        IF X = 431 THEN D = D + 7000
        IF X = 532 THEN D = D + 6900
        IF X = 631 THEN D = D + 7000
        IF X = 732 THEN D = D + 6900
        IF X = 832 THEN D = D + 6900
        IF X = 931 THEN D = D + 7000
        IF X = 1032 THEN D = D + 6900
        IF X = 1131 THEN D = D + 7000
        IF X = 1232 THEN D = D - 113099
        IF X = 132 THEN D = D + 6900
        IF X = 229 THEN D = D + 7200
        IF X = 332 THEN D = D + 6900
        REM:PUT IN C VS C
        REM:DAY NIGHT ETC
        REM:WRITE IT OUT

        NEXT I
        STOP
        END





DOYY:
        YY = J: NN = 1: IF YY > 0 THEN GOSUB DO1
        YY = K: NN = 2: IF YY > 0 THEN GOSUB DO1
        YY = L: NN = 3: IF YY > 0 THEN GOSUB DO1
        YY = M: NN = 4: IF YY > 0 THEN GOSUB DO1
        YY = N: NN = 4: DH = 1: IF YY > 0 THEN GOSUB DO1
        DH = 0
        RETURN
9000 :
        FOR Q2 = 1 TO 40: FOR Q1 = 0 TO 1: X%(Q2, Q1) = -1: X$(Q2, Q1) = "": NEXT: NEXT
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

