DEFINT A-Z
  '$INCLUDE: 'GWDECL.BAS'
DECLARE SUB HITBYPITCH (S2%,D)
DECLARE SUB WILDPITCH (S2%,D)
DECLARE SUB PBP (F$)
DECLARE SUB BASEONBALLS (S2%, D2, D, P)
DECLARE SUB PINCHRUNNER (MS%,A1,S4%,I2,P,PR%,BC%,U%,D,MO)
DECLARE FUNCTION FUsing$ (Number$, Image$)
DECLARE FUNCTION GetDrive% ()
DECLARE SUB SETUP ()
DECLARE SUB ADDPIT (D,I5,I6)
DECLARE SUB ADVANCEMENT (P,D,S2%,F%,I3)
DECLARE SUB BATDISP (PG,P9)
DECLARE SUB DELAY ()
DECLARE SUB FLDDISP (BC%,P,U%,D)
DECLARE SUB FLDERR (F%,W%,D,I3,SB%,S2%,P)
DECLARE SUB HITTINGLINES (L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
DECLARE SUB HOLDSTATUS (H0%)
DECLARE SUB INCBATDF (D,I5,I6)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB INCPITOFF (P,I5,I6)
DECLARE SUB INNDISP (I2,I)
DECLARE SUB LDRIVE ()
DECLARE SUB LET2NUM (I$)
DECLARE SUB MAINSCREEN (D,P,BC%,U%,MO)
DECLARE SUB PITDISP (P9)
DECLARE SUB PITSTAM (P9)
DECLARE SUB RANDNUM (RN)
DECLARE SUB SCOREBOARD ()
DECLARE SUB POPUP ()
DECLARE SUB CHECKSAVE (D,P,S4%)
DECLARE SUB POPUPSTATS ()
DECLARE SUB VIEWTEAM ()
DECLARE SUB REPLACEMENTS (I7%,I8%,I9%)
DECLARE SUB WHOSON1ST (S4%)
DECLARE SUB GETINFIELDER (W%,P,I1,D,S2%,P2)
DECLARE SUB GETFIELDNAME (W%,D)
DECLARE SUB GETOUTFIELDER (W%,P,I1,D)
DECLARE SUB OUTFLDNAME (W%,D)
DECLARE SUB HITS2GAP (W%,P,I1,D)
DECLARE SUB CREDITASSIST (W%,D)
DECLARE SUB CREDITPUTOUT (I3,FB2P%,AST%,D)
DECLARE SUB CREDITREGPO (W%,D)
DECLARE SUB NEWLINES ()
DECLARE SUB PICKEDOFF (D,I)
DECLARE SUB LONGFLY (D,W%)
DECLARE SUB DELIVERY (D)
DECLARE SUB TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
DECLARE SUB WALLDOUBLE (HW%,W%)
DECLARE SUB OUTFLDLOCATION (W%)
DECLARE SUB STRIKEOUT (U%,D)
DECLARE SUB GROUNDERPBP (W%,D)
DECLARE SUB DPPBP ()
DECLARE SUB DPRELAY ()
DECLARE SUB INCREMENTHIT (P,D,MO)
DECLARE SUB POPFLY (W%,FO%)
DECLARE SUB PITCHERNAME (W%,D)
DECLARE SUB OUTATFIRST (W%,D,FBU%,FB2P%)
DECLARE SUB GROUNDERS (W%,D)
DECLARE SUB SECOND2FIRST (D)
DECLARE SUB THROW2FIRST (D)
DECLARE SUB DPPIVOT (W%,D)
DECLARE SUB CAUGHTFLY (I,S2%,D)
DECLARE SUB BATOFF (P,I5,I6)
DECLARE SUB PITOFF (P,I5,I6)
DECLARE SUB SQUEEZECHOICE (SH)
DECLARE SUB ADDSTTS (P,D)
DECLARE SUB CHANGELINEUP (P9,P)
DECLARE SUB DISPFLDG (PG,P9,TG)
DECLARE SUB STEALCHANCE (AA,I2,I,CHANCE%)
DECLARE SUB STEAL3RD (I,AA,Q0,H0%,D)
DECLARE SUB PITCHOUT (S4%,I,Q0)
DECLARE SUB TRIPLEPLAY (D)
DECLARE SUB TIRING (P9)
DECLARE SUB PITSWAP (I)
DECLARE SUB PUTOUT (I,D,P)
DECLARE SUB WHICHRUNNER (I,S4%,D5)
DECLARE SUB NOJUMP (D)
DECLARE SUB STRIKEOUTCS (SO)
DECLARE SUB GETAKEY ()
DECLARE SUB PLAYERNAME (P)
DECLARE SUB OFFPIT (P9,I5,I6)
DECLARE SUB SINGLEROUTINE (W%,P,I1,D,S2%,P2)
DECLARE SUB RAREPLAYS (H6%, H7%, IP%)
TYPE PBP
YY AS STRING * 30
END TYPE
  DIM K9(1) AS SINGLE
  DIM S$(1), A$(1), L%(1, 13), T1(1), B%(1, 22, 80), P%(1, 21, 89), B$(1, 22), P$(1, 21), P8%(1, 21, 41), h$(4)
  DIM B8%(1, 22, 21), P6%(1), P1%(1), S8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
  DIM S%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), C$(11), F$(10)
  DIM VV(1), VV1(1), D3%(1), M%(9), P2%(1), A5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
  DIM X0%(1, 2, 23), D0%(1), P5%(1, 21, 41), B5%(1, 22, 21), TP%(11, 2), L0%(1), T3%(22)
  DIM T%(22), TS%(1,11),SA%(1,24)
  DIM PH%(22, 1),MG%(1,999),F1$(10)
  DIM CSS%(1,22,6),SU%(1,1)
  DIM NG%(18), YN$(1), Q$(23), H0$(1), TM$(4), SC$(4), PC$(4), N$(1), D$(3), M$(1)
  DIM H0%(1), CZ%(1), BP(1), BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2)
  DIM PR%(24), GK%(1),AP%(2),VA%(26)
COMMON SHARED TS%(),SA%(),P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), h$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED S$, P, U6, DH%, U9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, CC%, AP%, INNING%,BUCK AS PBP
COMMON SHARED MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%
'$INCLUDE: 'COMMON.GW'
'$INCLUDE: 'GETVIDEO.BAS'
  DRIVE = GetDrive%
   STACK 0
   CALL SETUP
   CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
   CALL LOADSCREEN

SUB LOADSCREEN
   CALL SCREEN12(0)
   CALL DisplayPCXFile (S$, VideoPage%)
   REM:i=12*16*4
   VA%=&HAA00                    'PLAY BY PLAY BOX WINDOW
   MEMNEED%=44*128*4+4
   REDIM VB%(MEMNEED%\2)
   CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),0)
   CALL ClearVE
   REM:VA%(0)=VA%+12800\16 + 1       'SAVE THE HOLD RUNNERS SECTION
   va%(0)=va%
   call GMove2VE (53,400,12,16,VA%(0),0)
   VA%(1)=VA%(0)+768\16 + 1      'INFIELD POSITIONING
   CALL GMove2VE (52,416,14,16,VA%(1),0)
   VA%(2)=VA%(1)+896\16 + 1     'GET CENTER FIELDER
   CALL GMove2VE (38,224,10,32,VA%(2),0)
   VA%(3)=VA%(2)+1280\16 + 1     'GET LEFT FIELDER
   CALL GMove2VE (7,256,10,32,VA%(3),0)
   VA%(4)=VA%(3)+1280\16 + 1     'GET RIGHT FIELDER
   CALL GMove2VE (65,256,10,32,VA%(4),0)
   VA%(5)=VA%(4)+1280\16 + 1     'GET SHORTSTOP
   CALL GMove2VE (30,272,10,32,VA%(5),0)
   VA%(6)=VA%(5)+1280\16 + 1     'GET SECOND BASE
   CALL GMove2VE (45,272,10,32,VA%(6),0)
   VA%(7)=VA%(6)+1280\16 + 1     'GET THIRD BASE
   CALL GMove2VE (15,304,10,32,VA%(7),0)
   VA%(8)=VA%(7)+1280\16 + 1     'GET FIRST BASE
   CALL GMove2VE (61,304,10,32,VA%(8),0)
   VA%(9)=VA%(8)+1280\16 + 1     'GET PITCHER
   CALL GMove2VE (35,336,13,32,VA%(9),0)
   VA%(10)=VA%(9)+1664\16 + 1     'GET CATCHER
   CALL GMove2VE (35,416,15,16,VA%(10),0)
   VA%(11)=VA%(10)+960\16 + 1     'RUNNER ON 2ND
   CALL GMove2VE (37,304,10,32,VA%(11),0)
   VA%(12)=VA%(11)+1280\16 + 1     'RUNNER ON 3RD
   CALL GMove2VE (15,336,10,32,VA%(12),0)
   VA%(13)=VA%(12)+1280\16 + 1     'RUNNER ON 1ST
   CALL GMove2VE (56,336,10,32,VA%(13),0)
   VA%(16)=VA%(13)+1280\16 + 1     'LEFT HANDED BATTER
   MEMNEED%=22*16*4+4
   REDIM VC%(MEMNEED%\2)
   CALL GMove4VE (30,400,22,16,VARSEG(VC%(0)),0)
   CALL ClearVE
   CALL GMove2VE (1,304,14,176,VA%(16),0)
END SUB
   GOTO 5000

2000 REM: BRANCH FOR RARE PLAYS
     CALL RAREPLAYS (H6%, H7%, IP%)
     IF IP%=1 THEN IP% = 0:CALL RELIEVERS
     IF H6% <= 4 OR H6% > 5 AND H6% < 9 OR H6% >= 10 AND H6% <= 24 THEN gosub 2002:goto 5052
     IF H6% = 5 OR H6% >= 25 AND H6% <= 29 THEN gosub 2002:goto 3900
     IF H6% = 9 THEN gosub 2002:goto 3240
     IF H6% = 45 THEN gosub 2002:goto 3201
     gosub 2002:GOTO 5052
2002 CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     CALL FLDDISP(BC%, p, U%, D)
     CALL MAINSCREEN(D, p, BC%, U%, MO)
     return

3050 CALL RANDNUM(RN)
     IF (((PC% = 1 OR PC% = 2 OR PC% = 3) AND RN = 2) OR (RD = 1 AND RN <= 5)) AND INNING% >= 2 AND RD% = 0 THEN H7% = 20:H6%=21:GOTO 2000
     H6% = INT(RND(1) * 1000) + 1
     IF H6% <= 3 AND IJ% = 0 THEN 2000
     SN = 0: H6% = INT(RND(1) * 1000) + 1
IF O%(0)=0 AND S4%=4 AND INFPOS%<2 AND H6%=768 OR O%(0)=0 AND S4%=4 AND INFPOS% < 2 AND H6% = 123 OR O%(0) = 0 AND S4% = 7 AND INFPOS% < 2 AND H6% = 245 OR O%(0)=0 AND S4%=7 AND INFPOS%<2 AND H6%=381 THEN CALL TRIPLEPLAY (D): GOTO 3900
     H7% = INT(RND(1) * 7500) + 1
     CALL NEWLINES
     IF H6% <= 4 THEN 3200
     IF H6% <= 6 THEN 3210
     IF H6% <= 11 OR H6% <= 200 AND B%(D, B9%(D, 2), 22) <> 2 AND B%(D, B9%(D, 2), 23) <> 2 AND B%(D, B9%(D, 2), 24) <> 2 AND B%(D, B9%(D, 2), 25) <> 2 THEN 3220
     IF H6% <= 19 THEN 3230
     IF H7% = 27 THEN 3244
     IF H7% = 28 THEN 3246
     IF H6% <= B4%(P, 5, B1!(P)) + 25 THEN S2% = 2: GOTO 3240
     IF P2 = 4 THEN 4100
3056 H6% = INT(RND(1) * 1000) + 1: H7% = INT(RND(1) * 1000) + 1
     I4 = 0: IF P2 = 2 THEN I4 = -5
     IF INFPOS% = 2 THEN SN = 70: I4 = I4 + 15
     IF U% = 2 THEN I4 = I4 - 15
     IF INFPOS% = 1 THEN SN = 20
     IF INNING% > 5 AND S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) THEN I4 = I4 + 40
     IF P = 0 THEN I4 = I4 - 2
     IF P = 1 THEN I4 = I4 + 2
     IF H7% <= SN THEN CALL PBP ("It gets through the drawn-in infield!"): CALL DELAY:GOTO 3063
     IF H6% > B4%(P, 0, B1!(P)) + I4 THEN 3600
     H6% = INT(RND(1) * 1000) + 1
     IF H6% <= B4%(P, 1, B1!(P)) AND P2 <> 2 OR H6% <= B4%(P, 1, B1!(P)) * .5 AND P2 = 2 THEN S2% = 8: GOTO 3500
     IF H6% <= B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P)) AND P2 <> 2 OR H6% <= B4%(P, 1, B1!(P)) * .5 + B4%(P, 2, B1!(P)) * .5 AND P2 = 2 THEN S2% = 9: GOTO 3520

     CALL HITS2GAP (W%,P,I1,D)
     I3 = B4%(P, 1, B1!(P)) + B4%(P, 2, B1!(P))
     E2% = I3 + B4%(P, 3, B1!(P)): IF W% = 8 AND H6% <= E2% AND P2 <> 2 OR W% = 8 AND H6% <= E2% * .5 AND P2 = 2 THEN S2% = 10: GOTO 3530
     E2% = I3 + B4%(P, 6, B1!(P)): IF W% = 7 AND H6% <= E2% AND P2 <> 2 OR W% = 7 AND H6% <= E2% * .5 AND P2 = 2 THEN S2% = 10: GOTO 3530
     E2% = I3 + B4%(P, 7, B1!(P)): IF W% = 9 AND H6% <= E2% AND P2 <> 2 OR W% = 9 AND H6% <= E2% * .5 AND P2 = 2 THEN S2% = 10: GOTO 3530
3063 H6% = INT(RND(1) * 100) + 1: S2% = 7: IF B7%(P, B1!(P)) = 1 OR H7% <= SN THEN 3065
     IF H6% <= (B%(P, B3%(P, B1!(1)), 16) * 2.5) THEN S2% = 12: CALL GETINFIELDER (W%,P,I1,D,S2%,P2): CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900


3065 REM:*** single ***
     CALL SINGLEROUTINE (W%,P,I1,D,S2%,P2)
     IF S2% = 12 THEN CALL SCOREBOARD: GOTO 5050
     F% = 0: PQ = 0: CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 0 THEN CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
     CALL SCOREBOARD
     GOTO 3900


     
3200 REM:*** hit by pitch ***
     CALL HITBYPITCH (S2%,D)
3201 A5%(0) = 1: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
     IF B0%(1, 0) > 0 THEN A5%(1) = 1
     IF B0%(1, 0) > 0 AND B0%(2, 0) > 0 THEN A5%(2) = 1
     IF B0%(1, 0) > 0 AND B0%(2, 0) > 0 AND B0%(3, 0) > 0 THEN A5%(3) = 1: IF W% = 2 AND S2% > 2 THEN A5%(3) = 11
     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     GOTO 5050


3210 REM:*** balk ***
     IF S4% = 0 THEN 3100
     S2% = 4: CALL NEWLINES
     CALL RANDNUM(RN)
     UN = INT(RND(1) * 4) + 1
     IF RN <= 33 THEN F$="a balk has been called"
     IF RN > 33 AND RN <= 66 THEN CALL PBP ("and a BALK is called"):F$=P$(D, P1%(D))+" dropped the ball"
     IF RN > 66 THEN F$=U$(UN)+" calls a balk on "+P$(D, P1%(D))
     CALL PBP (F$): GOTO 3223

3100 rem:*** fouled out of play ***
     CALL FOULBALL
     GOTO 5052


3220 REM:*** passed ball ***
     IF S4% = 0 AND SO <> 1 THEN 3100
     S2% = 5: CALL NEWLINES
     CALL DELIVERY(D)
     CALL RANDNUM(RN)
     IF RN <= 50 THEN CALL PBP ("pitch gets by "+B$(D, B9%(D, 2)))
     IF RN > 50 THEN CALL PBP ("off "+B$(D, B9%(D, 2))+"'s glove"):CALL PBP ("it gets away from him")
     CALL PBP ("It's ruled a passed ball")
     IF B0%(1, 0) = 0 AND I = 9 AND SO = 1 OR O%(0) > 1 AND I = 9 AND SO = 1 THEN O%(1) = O%(1) + 1
3222 I8 = INT(RND(1) * 10) + 1
     IF S4% = 1 AND P2 = 2 AND I8 < B%(P, B3%(P, B0%(1, 0)), 16) OR S4% = 5 AND P2 = 2 AND I8 < B%(P, B3%(P, B0%(1, 0)), 16) THEN A5%(1) = 2: A5%(3) = 1: A5%(0) = 0: A5%(2) = 2: CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 5051
3223 IF B0%(1, 0) = 0 AND I = 9 AND SO = 1 OR O%(0) > 1 AND I = 9 AND SO = 1 THEN A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 3900
     A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 0: CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 5051
3230 REM:*** wild pitch ***
     IF S4% = 0 AND SO <> 1 THEN 3100
CALL WILDPITCH (S2%,D)
3236 GOTO 3222

3240 REM:*** base on balls ***
     CALL BASEONBALLS(S2%, D2, D, P)
     GOTO 3201


3244 S2% = 2: CALL NEWLINES: CALL PBP (B$(D, B9%(D, 2))+" is called for interference!!")
     CALL PBP (Q$(0)+" trots to first base")
     GOTO 3201


3246 CALL NEWLINES: CALL PBP (Q$(0)+" is out for obstruction!")
     W% = 2: CALL CREDITREGPO (W%,D): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6): GOTO 3248
     I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6)
3248 I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     GOTO 3900

3500 REM:*** double s2%=8 ***
     CALL GETOUTFIELDER (W%,P,I1,D)
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): CALL NEWLINES
     CALL DELIVERY(D)
     IF B7%(P, B1!(P)) <> 1 THEN
        I5 = B3%(P, B1!(P)): I6 = 0
        CALL INCBATOFF (P,I5,I6)
        I6 = 1
        CALL INCBATOFF (P,I5,I6)
        I6 = 4
        CALL INCBATOFF (P,I5,I6)
     ELSE
        I5 = P1%(P): I6 = 23
        CALL INCPITOFF (P,I5,I6)
        I6 = 24
        CALL INCPITOFF (P,I5,I6)
        I6 = 27
        CALL INCPITOFF (P,I5,I6)
     END IF
     CALL RANDNUM(RN)
     if rn<=40 and w% = 7 then
        Y% = INT(RND(1) * 40) + 2900
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 30) + 3020
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3502
     end if

     if rn<=40 and w% = 9 then
        Y% = INT(RND(1) * 40) + 2940
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 30) + 3020
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3502
     end if

     if rn<=40 and w% = 8 then
        Y% = INT(RND(1) * 40) + 2980
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 30) + 3020
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3502
     end if


     CALL RANDNUM(RN)
     IF RN <= 25 THEN
        CALL LONGFLY (D,W%)
        HW% = 1
        CALL WALLDOUBLE (HW%,W%)
        GOTO 3502
     END IF
     IF RN > 25 AND RN <= 50 THEN
        F$=Q$(0): CALL LDRIVE
        F$=F$+"one to the gap in "
        CALL OUTFLDLOCATION (W%)
        CALL PBP (F$)
        GOTO 3502
     END IF
     IF RN > 50 AND RN <= 60 THEN
        F$=Q$(0)+" hits a sinking liner into "
        CALL OUTFLDLOCATION (W%)
        CALL PBP (F$): CALL PBP ("It goes to the wall")
        GOTO 3502
     END IF
     IF RN > 60 AND RN <= 75 THEN
        F$=Q$(0)+" lines one to the alley in "
        CALL OUTFLDLOCATION (W%)
        CALL PBP (F$)
        GOTO 3502
     END IF
     IF RN > 75 AND RN <= 85 AND (W% = 7 OR W% = 9) THEN
        F$=Q$(0)
        CALL LDRIVE
        F$=F$+"one down the "+C$(W%)+" line": CALL PBP (F$)
        CALL PBP ("It rolls into the corner")
        GOTO 3502
     END IF
     IF RN > 75 AND RN <= 85 AND W% = 8 THEN
        F$=Q$(0): CALL LDRIVE
        F$=F$+"one between the fielders": CALL PBP (F$)
        CALL PBP (Q$(4)+" cuts it off...")
        GOTO 3502
     END IF
     IF RN > 85 AND RN <= 92 THEN
        F$=Q$(0): CALL LDRIVE
        F$=F$+"a blue darter into "+C$(W%)
        CALL PBP (F$)
        GOTO 3502
     END IF
     IF RN > 92 THEN
        F$=Q$(0)+" lines one to "
        CALL OUTFLDLOCATION (W%)
        CALL PBP (F$)
        CALL PBP ("It's down and to the wall!")
     END IF
3502 HW% = 0
     IF INT(RND(1) * 100) + 1 <= 80 THEN F$="Double!!" ELSE F$="2-Bagger!!"
     CALL PBP (F$)

     FOR I = 0 TO 3: A5%(I) = 2: NEXT I: A5%(3) = 1
     IF B0%(1, 0) = 0 THEN 3511
     H6% = INT(RND(1) * 100) + 1
     BR% = B%(P, B3%(P, B0%(1, 0)), 16)
     IF B7%(P, B0%(1, 0)) = 1 THEN BR% = P%(P, P1%(P), 31)
     I3 = 0
     IF O%(0) = 0 AND W% = 7 THEN I3= 18 + BR% * 3
     IF O%(0) = 1 AND W% = 7 THEN I3= 16 + BR% * 3
     IF O%(0) = 2 AND W% = 7 THEN I3= 34 + BR% * 3
     IF O%(0) = 0 AND W% = 8 THEN I3= 37 + BR% * 3
     IF O%(0) = 1 AND W% = 8 THEN I3= 44 + BR% * 3
     IF O%(0) = 2 AND W% = 8 THEN I3= 65 + BR% * 3
     IF O%(0) = 0 AND W% = 9 THEN I3= 12 + BR% * 3
     IF O%(0) = 1 AND W% = 9 THEN I3= 18 + BR% * 3
     IF O%(0) = 2 AND W% = 9 THEN I3= 40 + BR% * 3
     IF H6% <= I3 OR P2 = 2 THEN A5%(1) = 3
3511 CALL ADVANCEMENT (P,D,S2%,F%,I3)
     F% = 0
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 0 THEN
        CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
     END IF
     CALL SCOREBOARD
     GOTO 3900

3520 REM:*** TRIPLE S2%=9 ***
     CALL OUTFLDNAME (W%,D)
     CALL DELIVERY(D)
     CALL NEWLINES
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 5: CALL INCBATOFF (P,I5,I6): GOTO 3522
     I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 28: CALL INCPITOFF (P,I5,I6)
3522 I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     CALL RANDNUM(RN)
     if rn<=30 and w% = 7 then
        Y% = INT(RND(1) * 40) + 2900
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 40) + 3050
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3524
     end if

     if rn<=30 and w% = 9 then
        Y% = INT(RND(1) * 40) + 2940
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 40) + 3050
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3524
     end if

     if rn<=30 and w% = 8 then
        Y% = INT(RND(1) * 40) + 2980
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        Y% = INT(RND(1) * 40) + 3050
        CALL GETRPBP(Y%, Y$, D)
        CALL STRIPRPBP (Y$,Y%,D)
        CALL PBP (Y$)
        goto 3524
     end if


     CALL RANDNUM(RN)

     IF RN <= 10 AND W% <> 8 THEN 3570
     CALL LONGFLY (D,W%)
     ad$ = "wall": IF W% = 7 AND L%(1, 13) = 27 THEN ad$ = "screen"
     bv = INT(RND(1) * 100) + 1: CALL RANDNUM(RN)
     IF RN <= 20 THEN F$="It one-hops the "+ad$+"..."
     IF RN <= 40 AND RN > 20 THEN F$="It's off the top of the "+ad$+"..."
     IF RN <= 60 AND RN > 40 THEN CALL PBP ("It bounces fair"):F$="down into the corner..."
     IF RN <= 80 AND RN > 60 THEN CALL PBP ("It's in the gap"):F$="It rolls to the "+ad$
     IF RN <= 99 AND RN > 80 THEN F$="It hits at the base of the "+ad$+"..."
     IF RN > 99 THEN F$="The outfielders cross"
     CALL PBP (F$)
3524 CALL RANDNUM(RN)
     IF RN <= 90 THEN F$="Triple!!"
     IF RN > 90 THEN F$="3-Bagger!"
     CALL PBP (F$)
3525 A5%(3) = 1: A5%(0) = 3: A5%(1) = 3: A5%(2) = 2
     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     F% = 0
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 0 THEN CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
     CALL SCOREBOARD: GOTO 3900

3530 REM:*** HOME RUN S2%=10 ***
     CALL NEWLINES
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 6: CALL INCBATOFF (P,I5,I6)
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 29: CALL INCPITOFF (P,I5,I6)
     CALL RANDNUM(RN)
     IF RN <= 80 THEN CALL LONGFLY (D,W%): GOTO 3535
     CALL RANDNUM(RN)
     IF RN <= 20 AND (W% = 7 OR W% = 9) THEN
         F$=Q$(0)+" hits it down the "+C$(W%)+" line"
         CALL PBP (F$)
         CALL PBP ("It's long enough if it's fair...")
         GOTO 3536
     end if
     CALL PBP (Q$(0)+" swings...")
     IF RN > 20 AND RN <= 40 THEN CALL PBP ("NO DOUBT ABOUT THAT ONE!!"):F$="IT'S WAY OUT OF HERE!!"
     IF RN > 40 AND RN <= 60 THEN F$=Q$(4)+" CAN ONLY LOOK UP!!"
     IF RN > 60 AND RN <= 80 THEN F$=P$(D, P1%(D))+" WATCHES IT SAIL OUT!!"
     IF RN > 80 AND RN <= 90 THEN F$="GONE!!!"
     IF RN > 90 THEN F$="SAINTS AND HAYRAKES, LOOK AT 'ER SAIL!!"
     call pbp (F$)
3536 CALL PBP ("HOME RUN!!"): CALL DELAY:GOTO 3544
3535 CALL RANDNUM(RN)
     IF RN <= 5 AND L%(1, 13) = 8 AND W% = 7 THEN CALL PBP ("THAT'S OUT OF THE PARK"): CALL PBP ("ON WAVELAND AVENUE!!!"):F$="HOME RUN...HOLY COW!!!": GOTO 3543
     IF RN <= 15 AND L%(1, 13) = 8 AND W% = 7 THEN CALL PBP ("IT'S GONE"):F$="LAST SEEN BOUNCING DOWN WAVELAND AVENUE!!!": GOTO 3543
     IF RN <= 25 THEN 3580
3545 CALL RANDNUM(RN)
     IF RN <= 7 THEN F$="GOODBYE!! IT'S A HOME RUN!!"
     IF RN > 7 AND RN <= 14 THEN F$="ADIOS!! IT'S A HOME RUN !!!"
     IF RN > 14 AND RN <= 21 THEN F$="SAY GOOD-BYE TO THAT ONE!!!"
     IF RN > 21 AND RN <= 28 THEN F$="TOUCH 'EM ALL!!"
     IF RN > 28 AND RN <= 35 THEN F$="SEE YOU LATER!!"
     IF RN > 35 AND RN <= 42 THEN F$="GOOD-BYE MR. SPALDING!! HOME RUN!!!"
     IF RN > 42 AND RN <= 49 THEN F$="HOME RUN!! MY OH MY!!"
     IF RN > 49 AND RN <= 56 THEN F$="IT'S OVER THE WALL...OH DOCTOR!!!"
     IF RN > 56 AND RN <= 63 THEN F$="LONG, LONG, AND GONE!!"
     IF RN > 63 AND RN <= 70 THEN F$="HEY! HEY!...IT'S A HOME RUN !!!"
     IF RN > 70 AND RN <= 77 THEN F$="IT'S LONG, IT'S LONG, IT'S GONE!!!"
     IF RN > 77 AND RN <= 84 THEN F$="HOLY COW!! IT'S A HOME RUN!!!"
     IF RN > 84 AND RN <= 89 THEN F$="OPEN THE WINDOW AUNT MINNIE...HERE SHE COMES"
     IF RN > 89 AND RN <= 95 THEN CALL PBP ("WAAYYY BACK..."):F$="YOU CAN PUT IT ON THE BOARD, YYESSS!!"
     IF RN > 95 THEN CALL PBP ("IT'S WAY OUT OF HERE!! HOME RUN!!!"):F$="HOW 'BOUT THAT!!!"
3543 CALL PBP (F$):CALL DELAY
3544 A5%(3) = 1: A5%(2) = 2: A5%(1) = 3: A5%(0) = 4
     I5 = P1%(D): I6 = 14: CALL ADDPIT (D,I5,I6)
     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     CALL SCOREBOARD
     GOTO 3900

3570 CALL RANDNUM(RN): X$ = "1st"
     IF W% = 7 THEN X$ = "3rd"
     IF RN <= 50 THEN CALL PBP (Q$(0)+" hits a hot smash")
     IF RN > 50 THEN CALL PBP (Q$(0)+" lines one")
     CALL PBP ("Fair past "+X$+"!!"): CALL PBP ("It rolls into the "+C$(W%)+" corner")
     CALL PBP ("It's a triple!!"): GOTO 3525
3580 IF L%(1, 13) = 21 AND W% = 7 THEN CALL PBP ("IT CLEARS THE GREEN MONSTER!!"):F$="HOME RUN!!!": GOTO 3543
     IF L%(1, 13) = 27 AND W% = 7 THEN CALL PBP ("IT CLEARS THE SCREEN!!"):F$="HOME RUN!!!": GOTO 3543
     IF L%(1, 13) = 33 AND P = 1 THEN CALL PBP ("ADIOS!! THAT'S A HOME RUN!!"):F$="THE BUSCH EAGLE FLAPS ITS WINGS": GOTO 3543
     IF L%(1, 13) = 25 AND P = 1 THEN CALL PBP ("IT'S OUT OF HERE FOR A HOMER!!!"): F$="THERE GOES THE EXPLODING SCOREBOARD!": GOTO 3543
     IF L%(1, 13) = 7 AND W% = 9 THEN CALL PBP ("IT CLEARS THE WAGON GATE!!"): F$="THAT'S A HOME RUN!!": GOTO 3543
     IF L%(1, 13) = 7 AND P = 1 THEN CALL PBP ("ADIOS!! THAT'S A HOME RUN!!"): F$="THERE GO THE CANNONS!!": GOTO 3543
     IF L%(1, 13) = 8 OR L%(1, 13) = 40 THEN F$="IT CLEARS THE IVY FOR A HOME RUN!!": GOTO 3543
     IF L%(1, 13) = 23 AND (W% = 7 OR W% = 9) THEN CALL PBP ("IT'S LONG, IT'S LONG, IT'S GONE!!"):F$="THAT ONE'S IN THE UPPER DECK": GOTO 3543
     GOTO 3545


3600 REM:*** strikeout ***
     H6% = INT(RND(1) * 1000) + 1
     IF H6% > B4%(P, 4, B1!(P)) THEN 3610


3601 CALL NEWLINES
     CALL STRIKEOUT (U%,D)
3602 W% = 2: CALL CREDITREGPO (W%,D)
     O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1
     SO = 1
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 8: CALL INCBATOFF (P,I5,I6): IF P2 = 2 THEN U% = 3
     I5 = P1%(D): I6 = 5: CALL ADDPIT (D,I5,I6): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 31: CALL INCPITOFF (P,I5,I6): IF P2 = 2 THEN U% = 3
     IF P2 = 2 AND O%(0) < 3 THEN SO = 7: GOTO 4800

     REM:*** WILD PITCH OR PASSED BALL ON A STRIKEOUT
     I = INT(RND(1) * 600) + 1
     IF B0%(1, 0) = 0 AND I = 9 AND SO = 1 OR O%(0) = 3 AND I = 9 THEN
        O%(0) = O%(0) - 1: O%(1) = O%(1) - 1
        I5 = P1%(D): P8%(D, I5, 0) = P8%(D, I5, 0) - 1
        P5%(D, I5, 0) = P5%(D, I5, 0) - 1
        B8%(D, B9%(D, 2), 16) = B8%(D, B9%(D, 2), 16) - 1
        B5%(D, B9%(D, 2), 16) = B5%(D, B9%(D, 2), 16) - 1
        I1 = INT(RND(1) * 100) + 1: IF I1 < 77 THEN 3230 ELSE 3220
     END IF
     GOTO 3900


3610 REM:*** ground ball for out ***
     H6% = INT(RND(1) * 100) + 1
     E2% = B%(P, B3%(P, B1!(P)), 18) + P%(D, P1%(D), 12) - 52
     E3% = 23 + P%(D, P1%(D), 12)
     E4% = 0: IF H6% > E2% AND B7%(P, B1!(P)) <> 1 THEN E4% = 1
     IF H6% > E3% AND B7%(P, B1!(P)) = 1 THEN E4% = 1
     IF E4% = 1 THEN 4000
     S2% = 13
     CALL GETINFIELDER (W%,P,I1,D,S2%,P2): CALL NEWLINES: S2% = 13
     CALL DELIVERY(D)
     CALL GROUNDERPBP (W%,D)
     IF W% = 2 THEN 3612
     IF W% = 1 THEN F$=F$+"the pitcher":CALL PBP (F$):GOTO 3612
     F$=F$+F$(W%):CALL PBP (F$)
3612 IF (S4% = 1 OR S4% = 4 OR ((S4% = 7 OR S4% = 5) AND INFPOS% < 2)) AND O%(0) <= 1 and RN<=50 THEN
        CALL DPPBP
        DPC% = 1
     END IF
     IF B7%(P, B1!(P)) <> 1 THEN
        I5 = B3%(P, B1!(P)): I6 = 0
        CALL INCBATOFF (P,I5,I6)
     ELSE
        I5 = P1%(P): I6 = 23
        CALL INCPITOFF (P,I5,I6)
     END IF
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN 3900
     REM:*** 2 outs, bases empty, runner 2nd, runner 3rd, or 2nd&3rd ***
     IF O%(0) = 2 OR S4% = 0 OR S4% = 2 OR S4% = 3 OR S4% = 6 THEN 3650
     H6% = INT(RND(1) * 1000) + 1: E2% = ((L%(D, 5) - 80) * 4)
     E3% = ((5 - B%(P, B3%(P, B1!(P)), 16)) * 9)
     IF W% = 2 AND H6% > 250 + E2% + E3% THEN 3650
     IF H6% > 490 + E2% + E3% THEN 3650
     IF P2 = 2 THEN 3650
     S2% = 13
     IF S4% = 4 AND W% = 5 AND INT(RND(1) * 3) + 1 = 1 THEN 3633
     IF S4% = 4 AND W% = 2 THEN CALL CREDITASSIST (W%,D): GOTO 3633
     IF S4% = 7 AND W% = 1 THEN CALL CREDITASSIST (W%,D): TB = 9: W% = 2: CALL PBP ("He throws home"): GOTO 3631
     IF S4% = 5 AND S6%(P, 0) - S6%(D, 0) >= 0 AND INNING% > 7 AND O%(0) = 0 AND INFPOS% > 0 AND W% = 5 OR S4% = 5 AND S6%(P, 0) - S6%(D, 0) >= 0 AND INNING% > 7 AND O%(0) = 0 AND INFPOS% > 0 AND W% = 3 THEN 3650
     IF S4% = 7 AND INFPOS% > 0 AND (W% = 5 OR W% = 3) THEN CALL CREDITASSIST (W%,D): TB = 9: W% = 2: CALL PBP ("He fires it home"): GOTO 3631
     IF S4% = 7 AND INFPOS% = 2 AND W% = 4 THEN CALL CREDITASSIST (W%,D): TB = 9: W% = 2: CALL PBP ("He throws home for the force"): GOTO 3631
     IF S4% = 7 AND INFPOS% = 2 AND W% = 6 THEN CALL CREDITASSIST (W%,D): TB = 9: W% = 2: CALL PBP ("He goes home with it"): GOTO 3631
     IF W% <= 4 THEN
        CALL CREDITASSIST (W%,D)
        W% = 6
        CALL PBP ("He throws to "+B$(D, B9%(D, 6)))
     ELSE
        CALL CREDITASSIST (W%,D)
        W% = 4
        CALL PBP ("He throws to "+B$(D, B9%(D, 4)))
     END IF

     A5%(1) = 11: A5%(2) = 0: A5%(3) = 0: A5%(0) = 0

     IF S4% = 7 AND (W% = 2 OR W% = 1) THEN
        A5%(3) = 11: A5%(2) = 1: A5%(1) = 1: A5%(0) = 1
     END IF
     GOTO 3634

3631 A5%(3) = 11: A5%(1) = 0: A5%(2) = 0: A5%(0) = 0: GOTO 3634
3633 A5%(3) = 0: A5%(2) = 11: A5%(1) = 0: A5%(0) = 0
3634 I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     Q$(4) = B$(D, B9%(D, W%))


     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     CALL CREDITREGPO (W%,D)


     S2% = 15
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)


     IF F% = 1 THEN 3900
     CALL CREDITASSIST (W%,D)
     IF W% = 5 AND O%(0) < 2 AND INT(RND(1) * 10) + 1 <= B%(P, B3%(P, B0%(1, 0)), 16) THEN 3643
     A5%(0) = 11: A5%(1) = 1: A5%(2) = 0: A5%(3) = 0
     IF O%(0) < 2 THEN A5%(2) = 1: A5%(3) = 1
     GOTO 3644
3643 A5%(0) = 1: A5%(1) = 11: A5%(2) = 0: A5%(3) = 0
     IF O%(0) < 2 THEN A5%(2) = 1: A5%(3) = 1
3644 O%(0) = O%(0) + 1: O%(1) = 2: I5 = P1%(D): I6 = 0
     CALL DPRELAY
     CALL ADDPIT (D,I5,I6)

     CALL ADVANCEMENT (P,D,S2%,F%,I3)


     CALL CREDITPUTOUT (I3,FB2P%,AST%,D)


     CALL RANDNUM(RN)
     DPC% = 0
     IF RN <= 80 THEN F$=" --- DOUBLE PLAY ---"
     IF RN > 80 THEN F$=" --- TWIN KILLING ---"
     CALL PBP (F$)
     DP%(D) = DP%(D) + 1: D3%(D) = D3%(D) + 1
     TB = 0: CALL INCREMENTHIT (P,D,MO): GOTO 3900
3650 IF O%(0) < 2 THEN 3690
     I = INT(RND(1) * 10) + 1: S2% = 13
     IF W% = 5 AND I <= 5 AND (S4% = 4 OR S4% = 7) THEN CALL PITCHERNAME (W%,D): CALL PBP (Q$(4)+" gloves it and heads for 3rd"): GOTO 3670
     IF W% = 5 THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL THROW2FIRST (D): GOTO 3672
     IF W% = 6 AND I <= 8 AND (S4% = 1 OR S4% = 4 OR S4% = 5 OR S4% = 7) THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL DPPIVOT (W%,D): ast% = 6: GOTO 3671
     IF W% = 6 THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL THROW2FIRST (D): GOTO 3672
     IF W% = 4 AND I <= 5 AND (S4% = 1 OR S4% = 4 OR S4% = 5 OR S4% = 7) THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL DPPIVOT (W%,D): ast% = 4: GOTO 3671
     CALL RANDNUM(RN)
     IF W% = 3 AND RN <= 94 THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL PBP ("He takes it to the bag unassisted"): fbu% = 1: GOTO 3672
     IF W% = 3 THEN CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): W% = 1: CALL PITCHERNAME (W%,D): CALL PBP ("He flips it to "+Q$(4)+" covering"): fb2p% = 1: W% = 3: GOTO 3672
     CALL PITCHERNAME (W%,D): CALL GROUNDERS (W%,D): CALL SECOND2FIRST (D): GOTO 3672
3670 A5%(0) = 0: A5%(1) = 0: A5%(2) = 11: A5%(3) = 0: GOTO 3684
3671 A5%(0) = 0: A5%(1) = 11: A5%(2) = 0: A5%(3) = 0: GOTO 3684
3672 A5%(0) = 11: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0

3684 IF fbu% = 0 THEN CALL CREDITASSIST (W%,D)
     fbu% = 0
     if dpc% = 1 and a5%(0) = 1 then dpc% = 2
     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     IF DPC% = 2 THEN
        DPC% = 0
        CALL DPSAFE
     END IF
     IF I3>4 THEN I3=1
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D)


     O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     GOTO 3900

3690 I = INT(RND(1) * 100) + 1
     IF I <= 90 AND P2 <> 2 OR S4% = 0 THEN 3720
     IF I <= 10 AND P2 = 2 THEN 3720
     S2% = 13: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: A5%(0) = 11
     I = INT(RND(1) * 100) + 1
     IF S4% = 1 OR S4% = 4 OR S4% = 5 OR S4% = 7 THEN 3713
3693 I = INT(RND(1) * 100) + 1
     A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
     IF W% = 5 AND I <= 25 AND INFPOS% < 2 THEN A5%(2) = 1
     IF W% = 5 AND I <= 83 AND INFPOS% = 0 THEN A5%(3) = 1:REM WAS 50
     IF W% = 5 AND I <= 10 AND INFPOS% > 0 THEN A5%(3) = 1
     IF W% = 5 AND I < 5 AND INFPOS% < 2 AND S4% = 2 AND S6%(P, 0) - S6%(D, 0) > -2 OR W% = 5 AND I < 5 AND INFPOS% < 2 AND S4% = 6 AND A5%(3) = 1 AND S6%(P, 0) - S6%(D, 0) > -2 THEN A5%(2) = 11: A5%(0) = 1: CALL PBP ("NO, he's going to third")
     IF W% = 6 AND I <= 20 AND INFPOS% < 2 THEN A5%(2) = 1
     IF W% = 6 AND I <= 92 AND INFPOS% < 2 THEN A5%(3) = 1
     IF W% = 6 AND I <= 5 AND INFPOS% = 2 THEN A5%(3) = 1
     IF W% = 6 AND I < 6 AND INFPOS% < 2 AND S4% = 2 AND S6%(P, 0) - S6%(D, 0) > -2 OR W% = 6 AND I < 6 AND INFPOS% < 2 AND S4% = 6 AND A5%(3) = 1 AND S6%(P, 0) - S6%(D, 0) > -2 THEN A5%(2) = 11: A5%(0) = 1: CALL PBP ("NO, he's going to third")
     IF W% = 4 AND I <= 90 AND INFPOS% < 2 THEN A5%(2) = 1
     IF W% = 4 AND I <= 90 AND INFPOS% < 2 THEN A5%(3) = 1
     IF W% = 4 AND I <= 65 AND INFPOS% = 2 THEN A5%(2) = 1
     IF W% = 4 AND I <= 5 AND INFPOS% = 2 THEN A5%(3) = 1
     IF W% = 3 AND I <= 95 THEN A5%(2) = 1
     IF W% = 3 AND I <= 88 AND INFPOS% = 0 THEN A5%(3) = 1:REM: WAS 60
     IF W% = 3 AND I <= 15 AND INFPOS% > 0 THEN A5%(3) = 1
     IF W% = 1 AND I <= 30 THEN A5%(2) = 1
     IF W% = 1 AND I <= 50 AND INFPOS% = 0 THEN A5%(3) = 1
     IF W% = 1 AND I <= 10 AND INFPOS% > 0 THEN A5%(3) = 1
     IF W% = 2 AND I <= 45 THEN A5%(2) = 1
3713 GOTO 3684
3720 S2% = 14
     IF S4% = 0 OR S4% = 2 OR S4% = 3 OR S4% = 6 THEN A5%(0) = 11: CALL OUTATFIRST (W%,D,FBU%,FB2P%): GOTO 3693
     I = INT(RND(1) * 100) + 1
     A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: A5%(3) = 1: IF INFPOS% = 0 AND W% = 2 THEN A5%(3) = 0
     IF S4% = 5 OR S4% = 7 OR S4% = 4 THEN 3725
     A5%(1) = 11: GOTO 3743
3725 IF S4% = 4 OR S4% = 7 THEN 3732
     IF S6%(P, 0) - S6%(D, 0) >= 0 AND INNING% > 6 AND O%(0) = 0 AND INFPOS% > 0 AND W% = 5 OR S6%(P, 0) - S6%(D, 0) >= 0 AND INNING% > 6 AND O%(0) = 0 AND INFPOS% > 0 AND W% = 3 THEN A5%(3) = 11: GOTO 3743
     IF INFPOS% = 0 AND W% <> 2 THEN A5%(1) = 11: GOTO 3743
     IF INFPOS% > 0 AND W% = 5 OR INFPOS% > 0 AND W% = 3 THEN A5%(3) = 0: A5%(1) = 11: GOTO 3743
     IF INFPOS% > 0 AND (W% = 1 OR W% = 3) THEN A5%(3) = 0: A5%(1) = 1: A5%(0) = 11: GOTO 3743
     IF INFPOS% = 2 AND W% = 4 OR INFPOS% = 2 AND W% = 6 AND I <= 90 THEN A5%(3) = 0: A5%(1) = 11: GOTO 3743
     A5%(1) = 11: GOTO 3743
3732 IF W% = 6 AND I < 6 AND INFPOS% < 2 AND INNING% < 9 OR W% = 5 AND I < 6 AND INFPOS% < 2 AND INNING% < 9 THEN A5%(2) = 11: GOTO 3684
     IF S4% = 7 THEN 3740
     I = INT(RND(1) * 10) + 1: A5%(0) = 1: A5%(1) = 11: A5%(2) = 1: A5%(3) = 0
     IF W% = 5 AND I <= 5 THEN A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
     IF W% = 1 AND I <= 3 THEN A5%(1) = 1: A5%(2) = 11: A5%(3) = 0
     IF W% = 2 AND B%(P, B3%(P, B0%(2, 0)), 16) < 7 AND B%(P, B3%(P, B0%(2, 0)), 16) > 0 THEN A5%(1) = 1: A5%(2) = 11: A5%(3) = 0: GOTO 3743
     IF W% = 2 AND B%(P, B3%(P, B0%(2, 0)), 16) > 6 THEN A5%(1) = 1: A5%(2) = 1: A5%(0) = 11
     GOTO 3743
3740 I = INT(RND(1) * 100) + 1: A5%(1) = 11: A5%(2) = 1: A5%(3) = 1: A5%(0) = 1
     IF INFPOS% > 0 AND W% = 5 OR INFPOS% > 0 AND W% = 3 OR INFPOS% = 2 AND W% = 4 OR INFPOS% = 2 AND W% = 6 OR W% = 2 OR W% = 1 THEN A5%(3) = 11: A5%(0) = 1: A5%(1) = 1: A5%(2) = 1: GOTO 3743
     IF W% = 5 AND I <= 5 OR W% = 1 OR W% = 2 THEN A5%(3) = 11: A5%(2) = 1: A5%(1) = 1: A5%(0) = 1
3743 GOTO 3684


3900 CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
     CALL ClearVE
     I$=INKEY$
     IF UCASE$(I$) = "Q" THEN call SCREEN0(0): END

     REM:CALL GMove2VE (17,128,46,80,VA%(17),0)
     DPC% = 0:H0% = 0: INFPOS% = 0: F% = 0: PQ = 0: H0%(0) = 0
     IF O%(0) = 3 THEN 5000
     GOTO 5050

3955 CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
     CALL ClearVE
     RETURN

4000 rem:balls hit to the air
     I = INT(RND(1) * 100) + 1
     A5%(0) = 0: A5%(1) = 0: A5%(2) = 0: A5%(3) = 0
     IF I <= 73 THEN 4020:rem flyout
     IF I <= 89 THEN 4010:rem popup
     S2% = 21
     CALL GETINFIELDER (W%,P,I1,D,S2%,P2)
     CALL NEWLINES
     CALL DELIVERY(D)

     Y% = INT(RND(1) * 42) + 70
     CALL GETRPBP(Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$)

     Y% = INT(RND(1) * 34) + 120
     CALL GETRPBP(Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$)

     IF B7%(P, B1!(P)) = 1 THEN
        I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6)
     ELSE
        I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6)
     END IF

     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN 3900

     O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1
     CALL CREDITREGPO (W%,D)
     I5 = P1%(D): I6 = 0
     CALL ADDPIT (D,I5,I6)
     IF O%(0) = 3 THEN 3900

     I = INT(RND(1) * 100) + 1
     IF (S4% = 1 OR S4% = 5) AND I >= 80 OR (S4% = 1 OR S4% = 5) AND P2 = 2 THEN 4012
     IF (S4% = 4 OR S4% = 2) AND I >= 80 OR S4% = 4 AND P2 = 2 THEN 4014
4009 CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 3900

4010 rem: infield popup
     CALL GETINFIELDER (W%,P,I1,D,S2%,P2): S2% = 20: CALL NEWLINES
     IF W% = 1 THEN W% = 2: Q$(4) = B$(D, B9%(D, W%))
     IF (W% = 2 OR W% = 5 OR W% = 3) AND INT(RND(1) * 100) + 1 <= 30 THEN fo% = 1
     if fo%=0 and (s4%=4 or s4%=7) and o%(0)<2 then call pbp("INFIELD FLY RULE CALLED")
      if fo%=1 and w%=2 then y% = INT(RND(1) * 24) + 180
      if fo%=1 and (w%=3 or w%=4) then y% = INT(RND(1) * 31) + 210
      if fo%=1 and (w%=5 or w%=6) then y% = INT(RND(1) * 31) + 250
      if fo%<>1 and w%=2 then y% = INT(RND(1) * 50) + 400
      if fo%<>1 and w%=3 then y% = INT(RND(1) * 60) + 450
      if fo%<>1 and w%=5 then y% = INT(RND(1) * 60) + 510
      if fo%<>1 and w%=6 then y% = INT(RND(1) * 60) + 570
      if fo%<>1 and w%=4 then y% = INT(RND(1) * 60) + 630
      CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
      CALL PBP (Y$)

      if fo%=1 and w%=2 then y% = INT(RND(1) * 12) + 321
      if fo%=1 and (w%=3 or w%=4) then y% = INT(RND(1) * 25) + 340
      if fo%=1 and (w%=5 or w%=6) then y% = INT(RND(1) * 25) + 370
      if fo%<>1 then y% = INT(RND(1) * 25) + 690
      CALL GETRPBP(Y%, Y$, D): CALL STRIPRPBP (Y$,Y%,D)
      CALL PBP (Y$)


4011 IF B7%(P, B1!(P)) = 1 THEN
        I5 = P1%(P): I6 = 23
        CALL INCPITOFF (P,I5,I6)
     ELSE
        I5 = B3%(P, B1!(P)): I6 = 0
        CALL INCBATOFF (P,I5,I6)
     END IF

     IF S4% = 4 THEN 4017
     IF INT(RND(1) * 100) + 1 <= 25 AND (L%(1, 13) = 11 OR L%(1, 13) = 8) THEN CALL PBP ("fights the wind...")
     IF INT(RND(1) * 100) + 1 <= 15 AND (L%(1, 13) = 2 OR L%(1, 13) = 4 OR L%(1, 13) = 13 OR L%(1, 13) = 24) THEN CALL PBP ("fights the lights...")
     IF fo% <> 1 THEN CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900

4017 fo% = 0
     CALL PBP (Q$(4)+" makes the catch")
     CALL CREDITREGPO (W%,D)
     O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1
     I5 = P1%(D): I6 = 0
     CALL ADDPIT (D,I5,I6)
     GOTO 3900

4012 rem:runner doubled off of 1st
     A5%(1) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     CALL ADDPIT (D,I5,I6)
     D3%(D) = D3%(D) + 1: DP%(D) = DP%(D) + 1
     if w%<>3 then
        Y% = INT(RND(1) * 10) + 160
     else
        y% = int(rnd(1) * 10) + 170
     end if
     CALL GETRPBP(Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     call pbp(Y$)
     I3 = 1
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     call pbp(" --- DOUBLE PLAY ---")
     IF W%<> 3 THEN CALL CREDITASSIST (W%,D)
     GOTO 4009

4014 rem:runner doubled off of 2nd
     A5%(2) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     CALL ADDPIT (D,I5,I6)
     D3%(D) = D3%(D) + 1:DP%(D) = DP%(D) + 1
     I3 = 2
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     call pbp(" --- DOUBLE PLAY ---")
     CALL CREDITASSIST (W%,D)
     GOTO 4009




4020 REM:*** FLY  OUTS ***
     CALL GETOUTFIELDER (W%,P,I1,D)
     IF B7%(P, B1!(P)) <> 1 THEN
        I5 = B3%(P, B1!(P)): I6 = 0
        CALL INCBATOFF (P,I5,I6)
     ELSE
        I5 = P1%(P): I6 = 23
        CALL INCPITOFF (P,I5,I6)
     END IF
     I = INT(RND(1) * 100) + 1
     IF I <= 17 THEN 4050
     IF I <= 33 THEN 4060
     REM:*** regular fly out ***
     S2% = 18: CALL NEWLINES
     X$ = C$(W%)
     IF INT(RND(1) * 100) <= 25 AND W% = 7 THEN X$ = "lcf"
     IF INT(RND(1) * 100) <= 25 AND W% = 9 THEN X$ = "rcf"
     CALL DELIVERY (D)
     CALL RANDNUM(RN)
     Y% = INT(RND(1) * 90) + 1070
     IF (W% = 7 or W% = 9) AND RN<=10 THEN X$="":Y% = INT(RND(1) * 10) + 1160
     CALL GETRPBP (Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$+ " "+ X$)
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN 3900
4025 CALL CREDITREGPO (W%,D)
     CALL CAUGHTFLY (I,S2%,D)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) = 3 THEN 3900
     I = INT(RND(1) * 100) + 1
     IF S2%=19 AND S4%=1 AND I<=33 AND P2=2 OR S2%=19 AND S4%=5 AND I<=33 THEN
        A5%(1)=10
        O%(0)=O%(0)+1:O%(1)=O%(1)+1
        CALL ADDPIT (D,I5,I6)
        D3%(D)=D3%(D)+1:DP%(D)=DP%(D)+1
        CALL CREDITASSIST (W%,D)
        GOTO 4009
     END IF
     IF B0%(3,0)>0 THEN I8=4:LN%=3308:CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
     IF I9=1 THEN I9=0:GOTO 4074
     GOTO 4066

4050 REM:*** shallow fly ball ***
     S2% = 19
     CALL NEWLINES
     X$ = C$(W%)
     IF INT(RND(1) * 100) <= 25 AND W% = 7 THEN X$ = "lcf"
     IF INT(RND(1) * 100) <= 25 AND W% = 9 THEN X$ = "rcf"
     CALL DELIVERY(D)
     CALL RANDNUM(RN)
     Y% = INT(RND(1) * 60) + 1170
     CALL GETRPBP (Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$+ " "+ X$)
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN 3900
     GOTO 4025

4060 REM:*** deep fly out ***
     S2% = 17
     CALL NEWLINES
     CALL LONGFLY (D,W%)
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN 3900
     CALL CREDITREGPO (W%,D)
     CALL CAUGHTFLY (I,S2%,D)
     O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1
     I5 = P1%(D): I6 = 0
     CALL ADDPIT (D,I5,I6)
     IF O%(0) = 3 THEN 3900
     IF B0%(3, 0) = 0 THEN 4066
     A5%(3) = 1: I5 = P1%(D): I6 = 16: CALL ADDPIT (D,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 23: CALL PITOFF (P,I5,I6): I5 = P1%(P): I6 = 41: CALL INCPITOFF (P,I5,I6): GOTO 4066
     I5 = B3%(P, B1!(P)): I6 = 0: CALL BATOFF (P,I5,I6): I5 = B3%(P, B1!(P)): I6 = 19: CALL INCBATOFF (P,I5,I6)

4066 IF B0%(2, 0) = 0 THEN 4072
     I4 = (B%(P, B3%(P, B0%(2, 0)), 16) - 5) * 25: ZM = (B%(D, B9%(D, W%), 15) - 5) * 25
     I4 = I4 - ZM
     I = INT(RND(1) * 1000) + 1
     IF W%=9 AND S2%=17 AND I<=800+I4 AND P2<>2 OR W%=9 AND S2%=18 AND I<=400+I4 AND P2<>2 OR W%=8 AND S2%=17 AND I<=700 + I4 AND P2 <> 2 OR W% = 8 AND S2% = 18 AND I <= 350 + I4 AND P2<>2 OR W%=7 AND S2%=17 AND I<=250+I4 AND P2<>2 THEN A5%(2)=1
     IF P2 = 2 AND B%(P, B3%(P, B0%(2, 0)), 16) > 6 AND W% = 9 AND S2% = 17 AND I <= 400 + I4 OR P2 = 2 AND B%(P, B3%(P, B0%(2, 0)), 16) > 6 AND W% = 8 AND S2% = 17 AND I <= 400 + I4 THEN A5%(2) = 1
     IF A5%(2) <> 1 AND S2% = 18 AND W% = 9 AND P2 <> 2 AND S4% < 6 AND I > 800 THEN I8 = 3: LN% = 3308: CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9):GOTO 4072
     IF A5%(2) <> 1 AND S2% = 18 AND W% = 8 AND P2 <> 2 AND S4% < 6 AND I > 840 THEN I8 = 3: LN% = 3308: CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9):GOTO 4072
     IF A5%(2) <> 1 AND W% = 7 AND P2 <> 2 AND S4% < 6 AND I > 880 THEN I8 = 3: LN% = 3308: CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
4072 IF B0%(1, 0) = 0 THEN 4074
     I = INT(RND(1) * 1000) + 1: A5%(1) = 0
     I3 = (B%(P, B3%(P, B0%(1, 0)), 16) - 5) * 25
     I4 = (B%(D, B9%(D, W%), 15) - 5) * 25: J5 = I3 - I4
     IF S2% = 17 AND P2 <> 2 AND S4% = 1 AND I <= 100 + J5 THEN I8 = 2: LN% = 3308: CALL TRYFOREXTRABASE (LN%,S2%,P2,Z2%,D,W%,SAH%,SB%,I8,I9)
4074 CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 3900
4095 I$=INKEY$:IF I$="" THEN 4095
     RETURN
4100 REM:*** BUNT ROUTINE ***
     CALL NEWLINES: FOR I = 0 TO 3: A5%(I) = 0: NEXT: SH = 2: S2% = 23
     IF (S4% = 3 OR S4% > 4) AND O%(0) < 2 THEN CALL SQUEEZECHOICE (SH)
     CALL RANDNUM(RN)
     IF B7%(P, B1!(P)) = 1 THEN BR = P%(P, P1%(P), 32)
     IF B7%(P, B1!(P)) <> 1 THEN BR = B%(P, B3%(P, B1!(P)), 30)
     IF INFPOS% > 0 THEN BR = BR - 2
     IF BR <= 1 THEN BR = 1
     FOR I = 1 TO 9: IF RN <= BT%(L%(1, 10), BR, I) THEN 4107
     NEXT
4107 WW% = INT(RND(1) * 10)
     IF WW% <= 2 THEN W% = 5
     IF WW% > 2 AND WW% <= 4 THEN W% = 1
     IF WW% > 4 AND WW% <= 7 THEN W% = 3
     IF WW% > 7 THEN W% = 2
     CALL GETFIELDNAME (W%,D)
     CALL RANDNUM(RN)
     IF RN <= 52 THEN 4175
     ON I GOTO 4110, 4120, 4130, 4140, 4150, 4160, 4170, 4180, 4175
4110 REM:PU
     CALL PBP (Q$(0)+ " bunts it in the air...")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+ " makes the catch")
     CALL ADDSTTS (P,D)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL CREDITREGPO (W%,D)
     IF O%(0) = 3 OR SH = 2 THEN 3900
4111 IF W% <> 5 THEN CALL CREDITASSIST (W%,D): W% = 5: CALL CREDITREGPO (W%,D): Q$(5) = Q$(4): CALL GETFIELDNAME (W%,D): GOTO 4115
     CALL CREDITASSIST (W%,D): W% = 6: CALL CREDITREGPO (W%,D): Q$(5) = Q$(4): CALL GETFIELDNAME (W%,D)
4115 CALL PBP (Q$(5)+" throws to "+Q$(4)+" at 3rd")
     A5%(3) = 10: CALL ADVANCEMENT (P,D,S2%,F%,I3)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) < 3 AND SH = 1 THEN 5051
     IF O%(0) >= 3 AND SH = 1 THEN B1!(P) = B1!(P) - 1
     GOTO 3900
4120 REM:DP
     IF S4% = 1 OR S4% = 4 OR S4% = 5 OR S4% = 7 THEN 4125
     CALL PBP (Q$(0)+" lays down the bunt...")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+" makes a nice fielding play")
     CALL CREDITASSIST (W%,D): A5%(0) = 11
     IF SH = 1 AND (S4% = 3 OR S4% = 6) THEN A5%(3) = 11: FOR I = 0 TO 2: A5%(I) = 1: NEXT
4122 CALL ADDSTTS (P,D)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     GOTO 3900
4125 CALL PBP (Q$(0)+" lays down a good bunt")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+" fields it cleanly")
     IF SH = 1 AND (S4% = 5 OR S4% = 7) THEN 4127
     IF S4% = 7 AND O%(0) < 2 THEN 4128
     CALL PBP ("He throws to 2nd"):CALL CREDITASSIST (W%,D): A5%(1) = 11
     CALL ADDSTTS (P,D)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     IF O%(0) >= 3 THEN 3900
     A5%(0) = 11: CALL CREDITASSIST (W%,D): A5%(2) = 1
     CALL PBP ("over to first..."): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL PBP ("DOUBLE PLAY!!")
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D): O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     GOTO 3900
4127 IF W% = 2 THEN CALL PBP ("and catches "+Q$(3)+" heading home!"): CALL CREDITREGPO (W%,D)
     IF W% <> 2 THEN CALL PBP ("and throws home to nail "+Q$(3)+"!"): CALL CREDITASSIST (W%,D): W% = 2: CALL CREDITREGPO (W%,D)
     A5%(3) = 11: FOR I = 0 TO 2: A5%(0) = 1: NEXT: GOTO 4122
4128 A5%(3) = 11
     IF W% = 2 THEN CALL PBP ("He steps on home for one..."): CALL CREDITREGPO (W%,D): CALL CREDITASSIST (W%,D)
     IF W% <> 2 THEN CALL PBP ("He throws home for the force"): CALL CREDITASSIST (W%,D): W% = 2: CALL CREDITREGPO (W%,D): CALL CREDITASSIST (W%,D)
     CALL ADVANCEMENT (P,D,S2%,F%,I3)
     CALL PBP ("Back to first..."): A5%(0) = 11:CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D): CALL PBP ("DOUBLE PLAY!!")
     O%(0) = O%(0) + 2: O%(1) = O%(1) + 2
     CALL ADDSTTS (P,D)
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) >= 3 THEN 3900
     A5%(2) = 1: A5%(1) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 3900
4130 REM:LDRNRCTDN
     IF S4% = 7 OR S4% = 6 OR S4% = 5 OR S4% = 3 THEN A5%(3) = 11: FOR I = 0 TO 2: A5%(I) = 1: NEXT
     IF S4% = 2 OR S4% = 4 THEN A5%(2) = 11: A5%(0) = 1: A5%(1) = 1
     IF S4% = 1 THEN A5%(1) = 11: A5%(0) = 1
     IF S4% = 0 THEN A5%(0) = 11
     IF PLB = 0 THEN CALL PBP (Q$(0)+" bunts it hard!")
     IF PLB = 0 THEN CALL PBP (Q$(4)+" makes an easy play")
     PLB = 0: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     CALL ADDSTTS (P,D)
     CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D): GOTO 3900
4140 REM:SHGOOD
     CALL PBP (Q$(0)+" lays down a good bunt...")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+" makes a nice play")
     IF S4% = 0 THEN
        S2% = -12
        CALL PBP (Q$(0)+" beats out an infield hit!")
        GOTO 3065
     end if
4141 I5 = P1%(D): I6 = 15: CALL ADDPIT (D,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 40: CALL INCPITOFF (P,I5,I6)
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 18: CALL INCBATOFF (P,I5,I6)
     I = INT(RND(1) * 100) + 1
     IF S4% = 5 AND SH = 2 AND I > 98 OR I > 97 THEN
        S2% = -12
        CALL PBP (Q$(0)+" beats it out!")
        GOTO 3065
     END IF
     A5%(0) = 11
     CALL PBP ("The only play is to first")
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     IF O%(0) >= 3 THEN 3900
     FOR I = 1 TO 3: A5%(I) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3)
     GOTO 3900
4150 REM:C@3
     IF SH = 1 THEN 4160
     CALL RANDNUM(RN)
     IF B7%(P, B1!(P)) = 1 THEN RR = 2
     IF B7%(P, B1!(P)) <> 1 THEN RR = B%(P, B3%(P, B1!(P)), 16)
     IF INFPOS% > 0 THEN RR = RR - 2
     IF RR <= 2 THEN RR = 2
     FOR I = 0 TO 2
     IF RN <= CF%(RR, I) THEN 4155
     NEXT
4155 ON I + 1 GOTO 4156, 4157, 4158
4156 IF S4% > 0 THEN 4157
     CALL PBP (Q$(0)+" drops it right down the line")
     CALL PBP (Q$(4)+" lets it roll...")
     CALL PBP (Q$(0)+" is trying for 2nd!!")
     CALL PBP (Q$(4)+" fields and throws too late!!")
     CALL PBP ("You won't see that play very often")
     A5%(0) = 2: I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6): I6 = 0: CALL INCBATOFF (P,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
     CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: GOTO 3900
4157 S2% = 12
     CALL PBP (Q$(0)+" lays down a great bunt...")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+" fields... but too late!")
     s2% = -12
     GOTO 3065
4158 GOTO 4140
4160 REM:PLYLDBS
     IF B7%(P, B1!(P)) = 1 THEN RR = 2
     IF B7%(P, B1!(P)) <> 1 THEN RR = B%(P, B3%(P, B1!(P)), 16)
     IF INFPOS% > 0 THEN RR = RR - 2
     IF RR <= 2 THEN RR = 2
     IF S4% = 0 THEN 4150
     IF S4% = 1 THEN I3 = 1: J3$ = "2nd"
     IF S4% = 2 OR S4% = 4 THEN I3 = 2: J3$ = "3rd"
     IF S4% = 3 OR S4% > 4 THEN I3 = 3: J3$ = "home"
     CALL PBP (Q$(0)+" gets the bunt down...")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     CALL PBP (Q$(4)+" fields it quickly...")
     CALL PBP ("Looks like a close play at "+J3$+"!!")
     CALL PBP ("1- Throw to "+J3$+" 2- Throw to 1st")
     IF U6 = 2 OR U6 = 1 AND P <> U9 THEN I$ = "1": GOTO 4162
4161 I$ = INKEY$: IF I$ = "" THEN 4161
     I5 = VAL(I$): IF I5 < 1 OR I5 > 2 THEN 4161
4162 IF I5 = 2 THEN 4141
     CALL RANDNUM(RN)
     FOR I = 0 TO 1
     IF RN <= LB%(RR, I) THEN 4163
     NEXT
4163 IF I = 0 THEN PLB = 1: GOTO 4130
     CALL PBP (Q$(4)+" throws to "+J3$+"... SAFE!!")
     IF B7%(P, B1!(P)) <> 1 THEN I5 = B3%(P, B1!(P)): I6 = 0: CALL INCBATOFF (P,I5,I6)
     IF B7%(P, B1!(P)) = 1 THEN I5 = P1%(P): I6 = 23: CALL INCPITOFF (P,I5,I6)
     FOR I = 0 TO 3: A5%(I) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): GOTO 3900
4170 REM:FLKSO
     CALL RANDNUM(RN)
     IF P%(D, P1%(D), 6) = 0 THEN BR=0:GOTO 4173
     BR = P%(D, P1%(D), 9) / P%(D, P1%(D), 6)
4173 IF BR <= 4 THEN PR = 5
     IF BR > 4 AND BR <= 5.4 THEN PR = 4
     IF BR > 5.4 AND BR <= 6.9 THEN PR = 3
     IF BR > 6.9 AND BR <= 8.4 THEN PR = 2
     IF BR > 8.4 AND BR <= 11.9 THEN PR = 1
     IF BR > 11.9 THEN PR = 0
     FOR I = 0 TO 2
     IF RN <= SO%(PR, I) THEN 4171
     NEXT
4171 ON I + 1 GOTO 4172, 4175, 4140
4172 GOTO 3601
4175 U% = U% + 1
     REM:IF U% = 3 THEN 3601
     GPRINT0VE 29,64,STR$(U%),15
     CALL NEWLINES
     CALL RANDNUM(RN)
     F$=Q$(0)+ " "
     IF RN <= 13 OR SH = 1 AND RN <= 42 THEN F$=F$+"misses the pitch"
     IF RN > 13 AND RN <= 42 AND SH <> 1 THEN F$=F$+"takes a called strike"
     IF RN > 42 THEN F$=F$+"bunts it foul"
     CALL PBP (F$)
     IF U% = 3 THEN 3601
     IF SH = 1 AND RN <= 42 THEN W% = 2: CALL GETFIELDNAME (W%,D): GOTO 4111
     F$="He has"+STR$(U%)+" strike"
     if u%>1 then F$="He has"+STR$(U%)+" strikes": CALL PBP (F$)
     IF S4% = 4 OR S4% = 2 THEN INFPOS% = 0
     CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
     CALL ClearVE
     GOTO 5080
4180 REM:TFPLY
     CALL PBP (Q$(0)+" lays down the bunt...")
     CALL PBP (Q$(4)+" has a tough play")
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P): IF F% = 1 THEN 3900
     IF W% = 1 THEN RR = 3
     IF W% <> 1 THEN RR = B%(D, B9%(D, W%), 17)
     IF INFPOS% > 0 AND S4% <> 0 THEN RR = RR + 1 + L%(1, 10)
     IF INFPOS% > 0 AND S4% = 0 THEN RR = RR + L%(1, 10)
     IF INFPOS% = 0 AND S4% = 0 THEN RR = RR - 1
     IF RR > 11 THEN RR = 11
     IF RR < 0 THEN RR = 0
     CALL RANDNUM(RN)
     FOR I = 0 TO 2
     IF RN <= TP%(RR, I) THEN 4182
     NEXT
4182 ON I + 1 GOTO 4183, 4184, 4185
4183 S2% = 12
     CALL PBP ("but too late...base hit!")
     GOTO 3065
4184 PLB = 1: GOTO 4130
4185 GOTO 4141

4795 IF P2 <> 2 THEN
        CALL NEWLINES
        SB% = 1
        CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
        SB% = 0
        IF F% = 1 THEN
           B1!(P) = B1!(P) - 1
           CALL WHOSON1ST (S4%)
           GOTO 3900
        END IF
     END IF
     GOTO 4804

4800 SH% = 0: S2% = 16: W% = 1: Q$(4) = P$(D, P1%(D))
     IF S4% = 0 THEN 5051
     IF S4% = 1 OR S4% = 5 THEN TGTBASE% = 2: GOTO 4795
     IF S4% = 2 OR S4% = 4 THEN TGTBASE% = 3: GOTO 4795
     IF S4% = 3 OR S4% >= 6 THEN TGTBASE% = 4
4804 I2 = B3%(P, B0%(TGTBASE% - 1, 0))
     FOR I3 = 1 TO 2
     A5%(I3) = 1
     A5%(3) = 0
     NEXT I3
     CALL STEALCHANCE (AA,I2,TGTBASE%,CHANCE%)
     IF B7%(P, B0%(TGTBASE% - 1, 0)) = 1 THEN CHANCE% = 0
     IF TGTBASE% = 4 THEN CHANCE% = CHANCE% * .05: REM *** REDUCTION FOR STEALING HOME ***
4809 IF B%(P, I2, 14)>0 THEN Q0 = INT((B%(P, I2, 13) / (B%(P, I2, 13) + B%(P, I2, 14))) * 1000): GOTO 4813
     IF B%(P, I2, 13) > 31 THEN Q0 = 700 + ((B%(P, I2, 13) - 30) * 2): GOTO 4813
     IF B%(P, I2, 13) > 10 THEN Q0 = 600 + ((B%(P, I2, 13) - 10) * 10): GOTO 4813
     IF B%(P, I2, 13) > 0 THEN Q0 = 400 + ((B%(P, I2, 13) * 20))
     Q0 = 100
4813 IF B7%(P, B0%(TGTBASE% - 1, 0)) = 1 THEN Q0 = 100
     CALL STEAL3RD (TGTBASE%,AA,Q0,H0%,D): REM*** SUCCESS OF 3B STEAL ***
     IF P%(D, P1%(D), 0) = 1 AND TGTBASE% = 2 THEN CHANCE% = CHANCE% + 25: Q0 = Q0 + 10
     IF P%(D, P1%(D), 0) = -1 AND TGTBASE% = 2 THEN CHANCE% = CHANCE% - 50: Q0 = Q0 - 30
     IF P2 = 2 THEN 4838
     B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 1
     I4 = INT(RND(1) * 100) + 1
     IF I4 > B0%(TGTBASE% - 1, 2) THEN 4825

     S2% = 22: W% = 1: Q$(4) = P$(D, P1%(D))
4820 CALL NEWLINES
     CALL PBP (Q$(TGTBASE% - 1)+" caught off base in a rundown!")
     A5%(1) = 0: A5%(2) = 0: A5%(3) = 0: A5%(TGTBASE% - 1) = 11
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL PUTOUT (TGTBASE%,D,P)
     CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) >= 3 THEN B1!(P) = B1!(P) - 1: GOTO 3900
     INFPOS% = 0
     GOTO 5051

4825 REM:RUNNER HAS A CHANCE TO STEAL
     B0%(TGTBASE% - 1, 2) = B0%(TGTBASE% - 1, 2) + 17
     I9 = INT(RND(1) * 1000) + 1

     IF I9 > CHANCE% * 1.388 AND D5 <> 3 THEN
        CALL NEWLINES
        CALL NOJUMP (D)
        CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
        CALL ClearVE
        GOTO 5060
     END IF

4826 CALL NEWLINES
     TGBASE%=B3%(P, B0%(TGTBASE% - 1, 0))
     Y% = INT(RND(1) * 20) + 3090
     CALL GETRPBP(Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$)
     Y% = INT(RND(1) * 13) + 3110
     CALL GETRPBP(Y%, Y$, D)
     CALL STRIPRPBP (Y$,Y%,D)
     CALL PBP (Y$)
     CALL PITCHOUT (S4%,TGTBASE%,Q0)
     IF S4% <> 5 THEN 4888: REM*** DOUBLE STEAL ***1st&3rd
     IF SH% = 1 THEN I9 = INT(RND(1) * 1000) + 1: GOTO 4828
     IF U6 = 2 OR U6 = 1 AND P <> U9 THEN I$ = "Y": GOTO 4887
     CALL PBP ("Throw to 2B (YN) ")
4886 I$ = INKEY$: IF I$ = "" THEN 4886
4887 IF UCASE$(I$) <> "Y" THEN PJ = 7: GOTO 4829
4888 W% = 2: Q$(4) = B$(D, B9%(D, 2))
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN I5 = TGBASE%: I6 = 9: CALL INCBATOFF (P,I5,I6): CALL PITSWAP(TGTBASE%): B1!(P) = B1!(P) - 1: GOTO 3900
4827 CALL WHICHRUNNER (TGTBASE%,S4%,D5)
     I9 = INT(RND(1) * 1000) + 1
     Q0 = Q0 - ((B%(D, B9%(D, 2), 15) - 5) * 25)
4828 RN = INT(RND(1) * 11) + 1
     IF TGTBASE% = 4 THEN 4837
     IF RN = 1 THEN f$ = B$(D, B9%(D, 2))+ " rifles it to "+ h$(TGTBASE%)
     IF RN = 2 THEN f$ = B$(D, B9%(D, 2))+ " fires it to "+ h$(TGTBASE%)
     IF RN = 3 THEN f$ = B$(D, B9%(D, 2))+ " unloads it"
     IF RN = 4 THEN f$ = B$(D, B9%(D, 2))+ " is up and firing"
     IF RN = 5 THEN f$ = B$(D, B9%(D, 2))+ " comes up firing"
     IF RN = 6 THEN f$ = B$(D, B9%(D, 2))+ " lets it fly"
     IF RN = 7 THEN f$ = B$(D, B9%(D, 2))+ " throws down"
     IF RN = 8 THEN f$ = B$(D, B9%(D, 2))+ " gets it off"
     IF RN = 9 THEN f$ = B$(D, B9%(D, 2))+ " fires a missile"
     IF RN =10 THEN f$ = B$(D, B9%(D, 2))+ " throws quickly"
     IF RN =11 THEN f$ = B$(D, B9%(D, 2))+ " throws"
     CALL PBP (F$)
4837 IF B0%(TGTBASE% - 1, 2) = 18 AND I9 > Q0 OR B0%(TGTBASE% - 1, 2) > 18 AND I9 > Q0 - 200 THEN 4830
4829 RN = INT(RND(1) * 100) + 1
     IF RN <= 20 THEN F$="He beats the throw!"
     IF RN > 20 AND RN <= 40 THEN F$="He's in there!"
     IF RN > 40 AND RN <= 50 THEN F$="Throw is off-line..."
     IF RN > 50 AND RN <= 60 THEN F$="Throw is high..."
     IF RN > 60 AND RN <= 80 THEN F$="He slides under the tag!"
     IF RN > 80 AND TGTBASE% <> 4 THEN F$=U$(TGTBASE% + 1)+" with the call"
     IF RN > 80 AND TGTBASE% = 4 THEN F$=U$(1)+" with the call"
     CALL PBP (F$): CALL PBP ("safe @ "+h$(TGTBASE%)+ "!")
     B0%(TGTBASE% - 1, 2) = 0
     INFPOS% = 0
     CALL PITSWAP(TGTBASE%): CALL ADVANCEMENT (P,D,S2%,F%,I3)
     I5 = B9%(D, 2): I6 = 20: CALL INCBATDF (D,I5,I6)
     IF SH% <> 1 THEN 4892
     SH% = 0: GOTO 5051
4830 RN = INT(RND(1) * 100) + 1
     IF RN <= 20 THEN F$=B$(D, B9%(D, 2))+" nails "+Q$(TGTBASE% - 1)
     IF RN > 20 AND RN <= 40 THEN F$=Q$(TGTBASE% - 1)+" is pegged at "+h$(TGTBASE%)
     IF RN > 40 AND RN <= 60 THEN F$="Great throw by "+B$(D, B9%(D, 2))+ "!!"
     IF RN > 60 AND RN <= 80 THEN F$=Q$(TGTBASE% - 1)+" slides into the tag"
     IF RN > 80 AND TGTBASE% <> 4 THEN F$=U$(TGTBASE% + 1)+" with the call"
     IF RN > 80 AND TGTBASE% = 4 THEN F$=U$(1)+ " with the call"
     CALL PBP (F$)
     A5%(TGTBASE% - 1) = 11: INFPOS% = 0
     I5 = B3%(P, B0%(TGTBASE% - 1, 0)): I6 = 10: CALL INCBATOFF (P,I5,I6)
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 2
     CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3)
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     I5 = B9%(D, 2): I6 = 21: CALL INCBATDF (D,I5,I6)
     IF O%(0) >= 3 THEN B1!(P) = B1!(P) - 1: CALL STRIKEOUTCS (SO): GOTO 3900
4831 IF O%(0) >= 3 OR P2 = 2 AND U% = 3 THEN 3900
     IF SH% = 1 THEN SH% = 0: GOTO 5051
     IF AA < 20 OR S4% <> 5 OR B7%(P, B0%(3, 0)) = 1 THEN 5051
     CALL NEWLINES
     IF U6 = 2 OR U6 = 1 AND P = U9 THEN I$ = "N": GOTO 4835
     CALL PBP ("TRY TO STEAL HOME (YN)")
4834 I$ = INKEY$: IF I$ = "" THEN 4834
4835 IF UCASE$(I$) <> "Y" THEN 5051
     SH% = 1: GOTO 4890
4838 CHANCE% = 100: Q0 = Q0 - 200: W% = 2: Q$(4) = B$(D, B9%(D, 2))
     TGBASE%=B3%(P, B0%(TGTBASE% - 1, 0))
     B0%(TGTBASE% - 1, 2) = 18
     CALL FLDERR (F%,W%,D,I3,SB%,S2%,P)
     IF F% = 1 THEN I5 = TGBASE%: I6 = 9: CALL INCBATOFF (P,I5,I6): B1!(P) = B1!(P) - 1: CALL STRIKEOUTCS (SO): GOTO 3900
     GOTO 4827




4890 TGTBASE% = 4: A5%(1) = 0: A5%(2) = 0: A5%(3) = 1
     I2 = B3%(P, B0%(TGTBASE% - 1, 0)): CHANCE% = 100
     Q0 = 700
     GOTO 4826: REM *** STEAL HOME ***
4892 IF S4% <> 5 OR S4% = 5 AND PJ = 7 THEN 4831: REM*** PICK OFF IF NOT STEALING HOME ***
     PJ = 0: TGTBASE% = 4: SS% = INT(RND(1) * 20) + 1: IF SS% = 6 THEN 4820
     GOTO 4831


4900 call SCREEN0(0)
     P9 = D: B1!(P) = B1!(P) + 1: U% = 0: IF B1!(P) = 10 THEN B1!(P) = 1
     CLS : COLOR 15: PRINT "DOES "; B$(D, B3%(D, B9%(D, 0))); " STAY IN THE GAME (YN)"
4902 I$ = INKEY$: IF I$ = "" THEN 4902
     IF UCASE$(I$) = "N" THEN I5 = 77: GOTO 5504
     I5 = 66
     LOCATE 5, 1
     FOR I = 1 TO 9: COLOR 3, 0
     IF BP(P9) = I THEN COLOR 9
     LOCATE I + 4, 5: PRINT USING "# "; I;
     IF B7%(P9, I) = 1 AND I5 = 66 THEN PRINT B$(P9, B3%(P9, I));
     IF B7%(P9, I) = 1 AND I5 <> 66 THEN PRINT P$(P9, B3%(P9, I));
     IF B7%(P9, I) <> 1 THEN PRINT B$(P9, B3%(P9, I));
     PRINT " "; C$(B7%(P9, I));
     COLOR 3
     NEXT I
     I5 = 0
     COLOR 15: LOCATE 3, 1: PRINT "WHAT LINEUP SLOT WILL THE PITCHER BAT IN (1-9)"
4905 I$ = INKEY$: IF I$ = "" THEN 4905
     I2 = VAL(I$)
     IF I2 < 1 OR I2 > 9 THEN 4905
     IF B7%(D, I2) = 10 THEN B7%(D, I2) = 1
     INFPOS% = 0
     B7%(D, B9%(D, 0)) = B7%(D, I2): B9%(D, B7%(D, B9%(D, 0))) = B3%(D, B9%(D, 0)): B7%(D, I2) = 1
     I7% = D: I8% = B9%(D, 0): I9% = B3%(D, B9%(D, 0))
     CALL REPLACEMENTS (I7%,I8%,I9%)
     I5 = 66: GOTO 5504
4950 I1 = IJ0%
     I2 = B%(D,IJ0%,21)-40
4952 b%(D, I1, 21) = 1
     B3%(D, I2) = I1
     B9%(D, B7%(D, I2)) = I1

     D0%(D) = D0%(D) + 1: X0%(D, 0, D0%(D)) = B3%(D, I2)
     X0%(D, 1, D0%(D)) = B7%(D, I2): X0%(D, 2, D0%(D)) = (I2 * 10) + (D0%(D) - 9)
     W5 = D: W6 = D: VV(D) = I2: VV1(D) = I2
     FOR xx = 22 TO 25
     IF B7%(D, I2) = b%(D, I1, xx) THEN b%(D, I1, 15) = b%(D, I1, 33 + (xx - 22) * 2): b%(D, I1, 17) = b%(D, I1, 34 + (xx - 22) * 2): b%(D, I1, 19) = b%(D, I1, xx + 4): GOTO 4955
     NEXT
4955 CALL HITTINGLINES(l%(), p%(), P1%(), B7%(), b%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     CALL FLDDISP(BC%, p, U%, D): CALL MAINSCREEN(D, p, BC%, U%, MO)
     REM:CALL NEWLINES
     REM:LOCATE 15, 1:COLOR 15,0
     REM:PRINT M$(D); " is sending in a defensive sub"
     REM:CALL DELAY: CALL DELAY
     VV(D)=1:VV1(D)=9
     RETURN
4956 FOR I2=1 TO 9
     IF B7%(D,I2)=MG%(D,576+IJ0%) THEN 4957
     NEXT
4957 RETURN
4998 S8%(I,0)=S8%(I,0)+6:S8%(I,1)=6
     RETURN
5000 F% = 0: PQ = 0
     FOR I = 0 TO 1: O%(I) = 0
     NEXT I
     L0%(P) = L0%(P) + MO
     MO = 0
     H0% = 0
     FOR I = 1 TO 3
     FOR I1 = 0 TO 1
     B0%(I, I1) = 0: B1%(I, I1) = 0
     NEXT I1
     B0%(I, 2) = 0
     NEXT I
     S4% = 0
     IF P = 1 THEN INNING% = INNING% + 1: CZ%(1) = 0: CZ%(0) = 0
     P = 1 - P: D = 1 - P
     IF INNING% = 7 AND L%(1, 13) = 8 AND P = 1 AND VAL(LEFT$(A$(1), 2)) > 75 THEN CALL NEWLINES: CALL PBP ("ALL RIGHT..."): CALL PBP ("LEMME HEAR YA..."):CALL PBP ("A ONE, A TWO...")

     GOTO 5004

     IF CGERA%>0 AND INNING%=8 AND P=0 AND S6%(1,0)>S6%(0,0) AND P%(1,P1%(1),1)>0 AND P6%(1)=1 AND INT(P%(1,P1%(1),5)/P%(1,P1%(1),1)*100+.5)<=75 AND INT(RND(1)*100)+1<=INT(P%(1,P1%(1),16)/P%(1,P1%(1),1)*100+.5) THEN I=1:GOSUB 4998
     IF CGERA%>0 AND INNING%=8 AND P=0 AND S6%(0,0)>S6%(1,0) AND P%(0,P1%(0),1)>0 AND P6%(0)=1 AND INT(P%(0,P1%(0),5)/P%(0,P1%(0),1)*100+.5)<=75 AND INT(RND(1)*100)+1<=INT(P%(0,P1%(0),16)/P%(0,P1%(0),1)*100+.5) THEN I=0:GOSUB 4998

     IF P%(1,P1%(1),1)=0 OR P6%(1)<>1 THEN 5002
     CALL RANDNUM(RN)
     IF CGERA%>0 AND INNING%=8 AND P=0 AND S6%(1,0)>=S6%(0,0) AND INT(P%(1,P1%(1),5)/P%(1,P1%(1),1)*100+.5)>75 AND INT(P%(1,P1%(1),5)/P%(1,P1%(1),1)*100+.5)<=100 AND RN<=INT(P%(1,P1%(1),16)/P%(1,P1%(1),1)*100+.5) THEN I=1:GOSUB 4998
5002 IF P%(0,P1%(0),1)=0 OR P6%(0)<>1 THEN 5003
     CALL RANDNUM(RN)
     IF CGERA%>0 AND INNING%=8 AND P=0 AND S6%(0,0)>=S6%(1,0) AND INT(P%(0,P1%(0),5)/P%(0,P1%(0),1)*100+.5)>75 AND INT(P%(0,P1%(0),5)/P%(0,P1%(0),1)*100+.5)<=100 AND RN<=INT(P%(0,P1%(0),16)/P%(0,P1%(0),1)*100+.5) THEN I=0:GOSUB 4998

5003 IF CGERA%>0 AND INNING%=8 AND P=0 AND (S6%(1,0)>=S6%(0,0) OR S6%(0,0)-S6%(1,0)<=2) AND P%(1, P1%(1), 1) > 0 AND P6%(1)=1 AND INT(P%(1, P1%(1), 16) / P%(1, P1%(1), 1) * 100 + .5)>100 THEN I=1:GOSUB 4998
     IF CGERA%>0 AND INNING%=8 AND P=0 AND (S6%(0,0)>=S6%(1,0) OR S6%(1,0)-S6%(0,0)<=2) AND P%(0, P1%(0), 1) > 0 AND P6%(0)=1 AND INT(P%(0, P1%(0), 16) / P%(0, P1%(0), 1) * 100 + .5)>100 THEN I=0:GOSUB 4998


5004 IF INNING% > 9 AND D = 1 AND S6%(1, 0) <> S6%(0, 0) OR INNING% = 28 OR INNING% >= 9 AND P = 1 AND S6%(P, 0) - S6%(D, 0) > 0 THEN 8500
     IF RD = 1 AND INNING% >= RE AND RE >= 6 AND (S6%(1, 0) > S6%(0, 0) AND P = 1 OR S6%(0, 0) > S6%(1, 0) AND P = 0) THEN call SCREEN0(0):PRINT "THIS GAME HAS BEEN CALLED ON ACCOUNT OF RAIN!!!":PRINT "ALL RECORDS ARE OFFICIAL!!":CALL GETAKEY:GOTO 8500
     IF RD = 1 AND P = 0 AND INNING% = 4 THEN CALL PBP ("THE RAIN IS FALLING A LITTLE MORE STEADILY"): CALL PBP ("THIS GAME MAY NOT GO 9!"): CALL GETAKEY
     CALL FLDDISP (BC%,P,U%,D)
     FOR IJ0%=0 TO 22
     IF B%(D,IJ0%,21)>39 AND B%(D,IJ0%,21)<=49 THEN GOSUB 4950
     NEXT
     A1 = S6%(D, 0) - S6%(P, 0)
     IF MG%(D,560)<>999 OR INNING%<8 OR U6 = 0 OR U6 = 1 AND P = U9 THEN 5025
     FOR IJ0%=1 TO 4
     IF MG%(D,560+IJ0%)=-1 OR B%(D,MG%(D,564+IJ0%),21)<>0 THEN 5010
     CALL RANDNUM (RN)
     IF B9%(D,MG%(D,576+IJ0%))=MG%(D,560+IJ0%) AND RN<=MG%(D,568+IJ0%) AND A1>=MG%(D,572+IJ0%) THEN I1=MG%(D,564+IJ0%):GOSUB 4956:GOSUB 4952
5010 NEXT
5025 IF B9%(D, 1) = 99 AND U6 = 0 OR B9%(D, 1) = 99 AND U6 = 1 AND U9 = 1 - D THEN 4900
5050 SH = 0: F% = 0: PQ = 0: SO = 0
     B1!(P) = B1!(P) + 1: INFPOS% = 0: U% = 0
     IF B1!(P) = 10 THEN B1!(P) = 1
5051 CALL WHOSON1ST (S4%)
     CALL PLAYERNAME (P)
     CALL MAINSCREEN (D,P,BC%,U%,MO)

     P2 = 0
     D2 = 0
     IF INNING% >= 9 AND P = 1 AND S6%(P, 0) - S6%(D, 0) > 0 THEN 8500
5052 CALL GMove4VE (20,48,44,128,VARSEG(VB%(0)),-1)
     CALL ClearVE
     I2 = INNING%: I = P
     IF S%(P, INNING%) > 0 THEN CALL INNDISP (I2,I)
     FORMATTED$=FUSING$(STR$(S6%(I,0)),"##")
     GPRINT0VE I+29,53,FORMATTED$,15
     FORMATTED$=FUSING$(STR$(S6%(D,2)),"##")
     GPRINT0VE 1-I+29,59,FORMATTED$,15
     Z2% = B0%(2, 0): F% = 0: PQ = 0: GOTO 5080
5054 D2 = VAL(I$): IF D2 < 1 OR D2 > 9 THEN 5052
5055 IF D2 = 4 THEN INFPOS% = 1:F$="INF:CORNERS IN"
     IF D2 = 3 THEN INFPOS% = 2:F$="INF:INFIELD IN"
     IF D2 = 5 THEN INFPOS% = 0:F$="INF:NORMAL    "
     IF D2=3 OR D2=4 OR D2=5 THEN CALL GMove2VE (52,416,14,16,VA%(1),-1):GPRINT1VE 27,52,F$,0: GOTO 5052
5058 IF D2 = 2 THEN CALL NEWLINES: GOTO 3240
     IF D2 = 6 THEN P9 = D: W5 = -1: W6 = -1: GOTO 5500
5060 GOTO 5085
5061 I$ = INKEY$: IF I$ = "" THEN 5061
     CALL GMove2VE (1,304,14,176,VA%(16),-1)
     IF UCASE$(I$) = "Q" THEN call SCREEN0(0): END
     IF UCASE$(I$) = "D" THEN CALL VIEWTEAM: GOTO 5580
     IF UCASE$(I$) = "W" THEN CALL WEATHER: GOTO 5580
     IF UCASE$(I$) = "S" THEN CALL POPUPSTATS: GOTO 5580
     IF UCASE$(I$) = "L" THEN CALL POPUP: GOTO 5580
     IF UCASE$(I$) = "O" THEN CALL OPTIONS:GOTO 5580
     P2 = VAL(I$)
     IF P2 < 1 OR P2 > 5 THEN 5060
5064 IF P2 = 5 THEN P9 = P: W5 = -1: W6 = -1: GOTO 5500
     IF P2 = 3 THEN 4800
     IF P2 = 2 AND O%(0) = 2 OR P2 = 2 AND S4% <> 1 AND S4% <> 4 AND S4% <> 5 THEN 5051
5067 IF P2 <> 2 THEN 5100
     IF S4% = 4 THEN I = 3 ELSE I = 2
     I3 = INT(RND(1) * 100) + 1
     IF I3>=6 THEN 5068
     CALL PICKEDOFF (D,I): PO = 1: A5%(I - 1) = 10: O%(0) = O%(0) + 1
     O%(1) = O%(1) + 1: W% = 1: CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3)
     CALL CREDITPUTOUT (I3,FB2P%,AST%,D): I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) >= 3 THEN B1!(P) = B1!(P) - 1: PO = 0: GOTO 3900
5068 IF PO = 1 THEN PO = 0: GOTO 5051
     CALL NEWLINES: I3 = INT(RND(1) * 1000) + 1: I9 = INT(RND(1) * 1000) + 1
     IF I3 > 350 THEN CALL PBP ("Runner(s) going")
     IF I3<=350 THEN CALL PBP ("Count not favorable...hit away"): P2 = 1
     CALL DELAY
     IF I3 <= 350 THEN 3050
     IF I9 <= (B4%(P, 4, B1!(P)) * 2 * .33) THEN U% = U% + 1: CALL PBP (Q$(0)+" swings and misses"): CALL DELAY:IF U% = 3 THEN GOTO 3601 ELSE GOTO 4800
     IF I9 <= B4%(P, 4, B1!(P)) * 2 THEN CALL PBP (Q$(0)+" fouls off the pitch...hit away"): CALL DELAY:P2 = 1
     GOTO 3050
5080 IF U6 = 2 OR U6 = 1 AND P <> U9 THEN 8700
     IF S4% = 1 OR S4% = 5 THEN H0% = 1: CALL HOLDSTATUS (H0%)
5081 F$=LEFT$(A$(D), 3)+NN$(D)
     CALL SPA (F$,8)
     GPRINT1VE 20,1,F$,0
     F1$(1)="1-PITCH       "
     F1$(2)="2-INT WALK    "
     F1$(3)="3-INF IN      "
     F1$(4)="4-CORNERS IN  "
     F1$(5)="5-INF NORMAL  "
     F1$(6)="6-PERSONNEL   "
     F1$(7)="8-PITCH AROUND"
     F1$(8)="G-GUARD LINES "
     F1$(9)="H-HOLD RUNNER "
    F1$(10)="S/D/B/P STATS "
     FOR I=21 TO 30
     GPRINT1VE I,1,F1$(I-20),0
     NEXT

     GOTO 5090
5085 IF U6 = 2 OR U6 = 1 AND P = U9 THEN 8900
     F$=LEFT$(A$(P), 3)+NN$(P)
     CALL SPA (F$,8)
     GPRINT1VE 20,1,F$,0
     F1$(1)="1-HIT AWAY    "
     F1$(2)="2-HIT AND RUN "
     F1$(3)="3-STEAL       "
     F1$(4)="4-BUNT        "
     F1$(5)="5-PERSONNEL   "
     F1$(6)="O-GAME OPTIONS"
     F1$(7)="D/S/P/B STATS "
     F1$(8)="W-WEATHER COND"
     F1$(9)="L-SHOW LINEUPS"
    F1$(10)="Q-QUIT GAME   "
     FOR I=21 TO 30
     GPRINT1VE I,1,F1$(I-20),0
     NEXT
     GOTO 5061
5090 I$ = INKEY$: IF I$ = "" THEN 5090
     CALL GMove2VE (1,304,14,176,VA%(16),-1)
     IF UCASE$(I$) = "S" THEN CALL POPUPSTATS: GOTO 5580
     IF UCASE$(I$) = "L" THEN CALL POPUP: GOTO 5580
     IF UCASE$(I$) = "W" THEN CALL WEATHER: GOTO 5580
     IF UCASE$(I$) = "G" THEN INFPOS% = 3:F$="INF:GUARD LINE":CALL GMove2VE (52,416,14,16,VA%(1),-1):GPRINT1VE 27,52,F$,0: GOTO 5052
     IF UCASE$(I$) = "D" THEN CALL VIEWTEAM: GOTO 5580
     IF UCASE$(I$) = "O" THEN CALL OPTIONS:GOTO 5580
     IF UCASE$(I$) = "Q" THEN call SCREEN0(0): END
     IF UCASE$(I$) = "H" THEN H0% = 1 - H0%:CALL HOLDSTATUS (H0%):GOTO 5081
     IF VAL(I$) = 7 THEN P2 = 1: D2 = 1: GOTO 3050

     GOTO 5054
5100 IF D2 <> 8 THEN 3050
     CALL NEWLINES
     CALL RANDNUM(RN): IF P2 = 4 OR RN <= 50 AND P2 = 1 THEN 3050
     GOTO 3240

5500 call SCREEN0(0)
     TG = 0: PG = 0: RG = 0
     CLS
     COLOR 12: PRINT TAB(33); "PERSONNEL MENU": PRINT
     COLOR 14: PRINT "(0) "; : COLOR 15: PRINT "RESUME PLAY": PRINT
     COLOR 14: PRINT "(1) "; : COLOR 15: PRINT "RELIEF PITCHER": PRINT
     COLOR 14: PRINT "(2) "; : COLOR 15: PRINT "CHANGE BATTER/FIELDER"
5501 I$ = INKEY$: IF I$ = "" THEN 5501
     I = VAL(I$): I5 = 0: IF I > 2 THEN 5501
     ON I + 1 GOTO 5580, 5504, 5540
5504 IF P2 = 5 THEN CALL TIRING (P9): GOTO 5501
5505 CALL PITDISP (P9)
     LOCATE 23, 1: PRINT "SELECT RELIEVER (Z = NO CHANGE) ";
     IF B9%(P9, 1) <> 99 THEN CALL TIRING (P9)
     PRINT
5507 I$ = INKEY$: IF I$ = "" THEN 5507
     IF UCASE$(I$) = "Z" THEN 5500
     CALL LET2NUM (I$)
     I1 = VAL(I$): IF I1 > 21 THEN 5507
     IF P%(P9, I1, 15) > 0 THEN 5507
     LOCATE 23, 1: PRINT STRING$(78, 32): LOCATE 23, 1: PRINT P$(P9, I1); " "; B1$(P%(P9, I1, 0) + 2); " ";
     PRINT "ARE YOU SURE (YN)";
5515 I$ = INKEY$: IF I$ = "" THEN 5515
     IF UCASE$(I$) = "N" THEN 5505
     PPH = 0: PR% = 0
     IF S1%(P9) <> -1 AND P6%(P9) > 1 THEN I5 = P1%(P9): I6 = 22: CALL OFFPIT (P9,I5,I6)
     P1%(P9) = I1: P6%(P9) = P6%(P9) + 1: P%(P9, I1, 15) = P6%(P9): CALL PITSTAM (P9): CALL CHECKSAVE (D,P,S4%)
5518 P9 = 1 - P9: W5 = P9: W6 = W5: VV(P9) = 1: VV1(P9) = 9
     CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     P9 = 1 - P9: IF DH% = 1 OR I5 = 66 OR I5 = 77 THEN 5538
     CLS : LOCATE , 14: COLOR 14: PRINT "DO YOU WISH TO MAKE A FLIP FLOP OR DOUBLE SWITCH (YN)";
     COLOR 15
5520 I$ = INKEY$: IF I$ = "" THEN 5520
     IF UCASE$(I$) = "N" THEN 5538
     CLS : CALL BATDISP (PG,P9)
5523 CALL CHANGELINEUP(P9,P): LOCATE 12, 30: PRINT "LINEUP SLOT FOR PITCHER TO BAT IN (1-9)"
5524 I$ = INKEY$: IF I$ = "" THEN 5524
     IF ASC(I$) = 32 AND PG = 0 THEN PG = 1: CLS : CALL BATDISP (PG,P9): GOTO 5523
     IF ASC(I$) = 32 AND PG = 1 THEN PG = 0: CLS : CALL BATDISP (PG,P9): GOTO 5523
     IF ASC(I$) = 9 AND TG = 0 THEN CALL DISPFLDG (PG,P9,TG): GOTO 5523
     IF ASC(I$) = 9 AND TG = 1 THEN TG = 0: CALL BATDISP (PG,P9): GOTO 5523
     V2 = VAL(I$): IF V2 < 1 OR V2 > 9 THEN 5524
5526 LOCATE 13, 30: PRINT "WHO WILL REPLACE "; B$(D, B3%(D, V2)); " @ "; C$(B7%(D, V2));
5527 I$ = INKEY$: IF I$ = "" THEN 5527
     IF ASC(I$) = 32 AND PG = 0 THEN PG = 1: CALL BATDISP (PG,P9): GOTO 5526
     IF ASC(I$) = 32 AND PG = 1 THEN PG = 0: CALL BATDISP (PG,P9): GOTO 5526
     IF ASC(I$) = 9 AND TG = 0 THEN CALL DISPFLDG (PG,P9,TG): GOTO 5526
     IF ASC(I$) = 9 AND TG = 1 THEN TG = 0: CALL BATDISP (PG,P9): GOTO 5526
     CALL LET2NUM (I$): V3 = VAL(I$): IF V3 > 22 THEN 5527
     LOCATE 16, 29: PRINT B$(D, V3)
     LOCATE 17, 29: PRINT "ARE YOU SURE (YN)";
5531 I$ = INKEY$: IF I$ = "" THEN 5531
     IF UCASE$(I$) = "N" THEN 5518
     D0%(D) = D0%(D) + 1: X0%(D, 0, D0%(D)) = V3: X0%(D, 1, D0%(D)) = B7%(D, V2)
     FOR V0 = 1 TO 9
     IF B7%(D, V0) = 1 THEN X0%(D, 2, D0%(D)) = (V0 * 10) + (D0%(D) - 9)
     NEXT V0
     FOR V0 = 1 TO 9
     IF B7%(D, V0) = 1 THEN B7%(D, V0) = B7%(D, V2): B3%(D, V0) = V3: B9%(D, B7%(D, V0)) = V3: P9 = D: W5 = D: W6 = D: VV(D) = V0: VV1(D) = V0: CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     VV(D)=1:VV1(D)=9
     NEXT V0
     B%(D, V3, 21) = 1
     I7% = D: I8% = V2: I9% = V3
     CALL REPLACEMENTS (I7%,I8%,I9%)
     I5 = 0
     FOR V0 = 1 TO 9
     IF V0 = V2 THEN B7%(D, V2) = 1: B3%(D, V2) = P1%(D): B9%(D, 1) = P1%(D): VV(D) = V0: VV1(D) = V0: CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     NEXT V0
5537 VV(D)=1:VV1(D)=9
     GOTO 5500
5538 I5 = 0
     FOR V0 = 1 TO 9
     IF B7%(D, V0) = 1 OR B7%(D, V0) = 10 THEN
        B3%(D, V0) = P1%(D)
        B9%(D, 1) = P1%(D)
        B7%(D, V0) = 1
        P9 = D: W5 = D: W6 = D
        VV(D) = V0: VV1(D) = V0
        CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
        GOTO 5537
     END IF
     NEXT V0
5540 CLS : CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P)
5546 LOCATE 14, 29: COLOR 12: PRINT "SELECT:"
     LOCATE , 30: COLOR 14: PRINT " (1) "; : COLOR 15: PRINT "SELECT A PINCH-HITTER"
     LOCATE , 30: COLOR 14: PRINT " (2) "; : COLOR 15: PRINT "SWAP POSITIONS BETWEEN PLAYERS"
     LOCATE , 30: COLOR 14: PRINT " (3) "; : COLOR 15: PRINT "SUBSTITUTE PLAYER IN LINEUP"
     LOCATE , 30: COLOR 14: PRINT " (Z) "; : COLOR 15: PRINT "RESUME PLAY"
     LOCATE , 30: COLOR 14: PRINT "<SPACE> "; : COLOR 15: PRINT "TOGGLE ROSTER"; : COLOR 14: PRINT " <TAB> "; : COLOR 15: PRINT "TOGGLE FIELDING/BATTING"
5547 I$ = INKEY$: IF I$ = "" THEN 5547
     IF ASC(I$) = 32 AND PG = 0 THEN PG = 1: CALL BATDISP (PG,P9): GOTO 5547
     IF ASC(I$) = 32 AND PG = 1 THEN PG = 0: CALL BATDISP (PG,P9): GOTO 5547
     GOTO 5590


5550 LOCATE 20, 30: PRINT "LINEUP SLOT TO REPLACE (1-9) (Z)=NO CHANGE"
5552 I$ = INKEY$: IF I$ = "" THEN 5552
     IF ASC(I$) = 32 AND PG = 0 THEN PG = 1: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5552
     IF ASC(I$) = 32 AND PG = 1 THEN PG = 0: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5552
     IF ASC(I$) = 9 AND TG = 0 THEN CALL DISPFLDG (PG,P9,TG): CALL CHANGELINEUP (P9,P): GOTO 5552
     IF ASC(I$) = 9 AND TG = 1 THEN TG = 0: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5552
     IF UCASE$(I$) = "Z" THEN 5540
     I2 = VAL(I$): IF I2 < 1 OR I2 > 9 OR P9 = D AND B7%(P9, I2) = 1 THEN 5552
     LOCATE , 30
     IF B7%(P9, I2) = 1 THEN PRINT P$(P9, P1%(P9));
     IF B7%(P9, I2) <> 1 THEN PRINT B$(P9, B3%(P9, I2));
5555 LOCATE 22, 30: PRINT "# OF SUB"
5556 I$ = INKEY$: IF I$ = "" THEN 5556
     IF ASC(I$) = 32 AND PG = 0 THEN PG = 1: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5556
     IF ASC(I$) = 32 AND PG = 1 THEN PG = 0: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5556
     IF ASC(I$) = 9 AND TG = 0 THEN CALL DISPFLDG (PG,P9,TG): CALL CHANGELINEUP (P9,P): GOTO 5556
     IF ASC(I$) = 9 AND TG = 1 THEN TG = 0: CALL BATDISP (PG,P9): CALL CHANGELINEUP (P9,P): GOTO 5556
     CALL LET2NUM (I$): I1 = VAL(I$): IF I1 > 22 THEN 5555
     IF B%(P9, I1, 21) > 0 THEN 5555
     LOCATE 23, 30: PRINT B$(P9, I1); "... ARE YOU SURE (YN)"
5560 I$ = INKEY$: IF I$ = "" THEN 5560
     IF UCASE$(I$) = "N" THEN 5540
     B%(P9, I1, 21) = 1: B3%(P9, I2) = I1: B9%(P9, B7%(P9, I2)) = I1
     IF B7%(P9, I2) = 1 OR B7%(P9, I2) = 10 THEN B9%(P9, 1) = 99: B9%(P9, 0) = I2: B7%(P9, I2) = 10
     D0%(P9) = D0%(P9) + 1: X0%(P9, 0, D0%(P9)) = B3%(P9, I2)
     X0%(P9, 1, D0%(P9)) = B7%(P9, I2): X0%(P9, 2, D0%(P9)) = (I2 * 10) + (D0%(P9) - 9)
     W5 = P9: W6 = P9: VV(P9) = I2: VV1(P9) = I2
     I7% = P9: I8% = I2: I9% = I1
     CALL REPLACEMENTS (I7%,I8%,I9%)
     CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     VV(P9)=1:VV1(P9)=9
     GOTO 5540
5567 LOCATE 20, 29: PRINT "B.O. #1 (1-9)"
5568 I$ = INKEY$: IF I$ = "" THEN 5568
     I1 = VAL(I$): IF I1 < 1 OR I1 > 9 THEN 5568
     IF B7%(P9, I1) = 1 THEN 5568
     LOCATE 21, 29: PRINT "B.O. #2 (1-9)"
5572 I$ = INKEY$: IF I$ = "" THEN 5572
     I2 = VAL(I$): IF I2 < 1 OR I2 > 9 THEN 5572
     IF B7%(P9, I2) = 1 THEN 5572
     I3 = B7%(P9, I2): B7%(P9, I2) = B7%(P9, I1): B7%(P9, I1) = I3
     B9%(P9, B7%(P9, I1)) = B3%(P9, I1)
     B9%(P9, B7%(P9, I2)) = B3%(P9, I2)
     I7% = P9: I8% = I2: I9% = B3%(P9, I2)
     CALL REPLACEMENTS (I7%,I8%,I9%)
     I7% = P9: I8% = I1: I9% = B3%(P9, I1)
     CALL REPLACEMENTS (I7%,I8%,I9%)
     GOTO 5540
5580 FOR I = 1 TO 9
     IF B7%(P9, I) = 1 THEN 154
     B%(P9, B3%(P9, I), 21) = 1
154  NEXT I
     CALL LOADSCREEN
     CALL FLDDISP (BC%,P,U%,D)
     GOTO 5051
5590 IF ASC(I$) = 9 AND TG = 0 THEN CALL DISPFLDG (PG,P9,TG): GOTO 5547
     IF ASC(I$) = 9 AND TG = 1 THEN TG = 0: CALL BATDISP (PG,P9): GOTO 5547
     IF UCASE$(I$) = "Z" THEN 5500
     I1 = VAL(I$): IF I1 = 2 THEN 5567
     IF I1 = 3 THEN 5550
     IF D2 = 6 THEN 5550
     I2 = B1!(P): GOTO 5555
     FOR I5 = 20 TO 23: LOCATE I5, 28
     PRINT "                                                    "
     NEXT I5
     RETURN
8500 IF S6%(0, 0) > S6%(1, 0) THEN CALL PBP (A$(0)+" defeats "+A$(1)+"!") ELSE CALL PBP (A$(1)+" defeats "+A$(0)+"!")
     IF AP% = 1 AND U6 = 2 THEN 8607
     G4=1
     CALL PBP ("PRESS ANY KEY TO CONTINUE")
     IF DRIVE>=67 THEN 8606
     CALL PBP ("INSERT PROGRAM DISK # 2")
8606 I$ = INKEY$: IF I$ = "" THEN 8606
8607 call SCREEN0(0):CHAIN "POSTGAME.exe"
     END
8700 H0% = 0: CALL HOLDSTATUS (H0%)
     A1 = S6%(D, 0) - S6%(P, 0)
     IF A1 > 1 OR S4% = 3 OR S4% >= 6 THEN 8800
     IF S4% <> 1 AND S4% <> 5 THEN I2 = 2: GOTO 8800
     I2 = 1
     J = B0%(I2, 0): IF J = 0 THEN 8800
     IF B7%(P, J) = 1 THEN 8800
     IF B7%(P, J) <> 1 AND B%(P, B3%(P, J), 13) >= 10 THEN H0% = 1: CALL HOLDSTATUS (H0%)
8800 IF SU%(D,0)<>-1 AND MG%(D,589)=0 AND INNING%=MG%(D,588) THEN CALL PITSTRAT
     IF SU%(D,1)<>-1 AND MG%(D,589)=0 AND INNING%>MG%(D,588) THEN CALL PITSTRAT
     if SU%(D,1)<>-1 AND MG%(D,589)<>0 AND INNING%>=MG%(D,588) THEN CALL PITSTRAT


     IF PZ=1 THEN PZ=0:GOTO 5055
     A1 = S6%(D, 0) - S6%(P, 0)
     CALL RANDNUM (RN)
     IF INNING% < 9 OR CZ%(D)=-1 OR MG%(D,581)=999 THEN 8725


     CZ%(D) = -1
     IF A1 > 3 OR A1 <0 THEN 8725
     IF P6%(D) <> 1 THEN 8730
     IF S6%(P, 0) = 0 AND P%(D, P1%(D), 5) > 0 AND RN <= INT(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) + 10 THEN 8725
     IF P%(D, P1%(D), 5) > 0 AND RN <= INT(P%(D, P1%(D), 16) / P%(D, P1%(D), 5) * 100 + .5) THEN 8725
     IF CGERA%>0 AND INNING%>7 AND S6%(D,0)>S6%(P,0) AND P%(D,P1%(D),1)>0 AND P6%(D)=1 AND INT(P%(D,P1%(D),16)/P%(D,P1%(D),1)*100+.5)<=100 AND INT(RND(1)*100)+1<=INT(P%(D,P1%(D),16)/P%(D,P1%(D),1)*100+.5) THEN 8725
     IF CGERA%>0 AND INNING%>7 AND (S6%(D,0)>=S6%(P,0) OR S6%(P,0)-S6%(D,0)<=2) AND P%(D, P1%(D), 1) > 0 AND P6%(D)=1 AND INT(P%(D, P1%(D), 16) / P%(D, P1%(D), 1) * 100 + .5)>=100 THEN 8725
     IF CGERA%>=8 THEN 8725
     GK%(D) = 1
     GOTO 8725
8730 CALL RANDNUM(RN)
     IF RN <= 80 AND P%(D, P1%(D), 3) < 15 THEN GK%(D) = 1
8725 IF GK%(D) = 1 THEN GK%(D) = 0: CALL RELIEVERS :GOSUB 3955: GOTO 5055
     IF B7%(P, B1!(P)) = 1 THEN BR = 0
     IF B7%(P, B1!(P)) <> 1 THEN BR = B%(P, B3%(P, B1!(P)), 30)
     IF p% (P, P1%(P), 11) <> 999 THEN I7! = p%(I, P1%(I), 11):GOTO 8726
     IF p%(P, P1%(P), 20) = 0 THEN I7!=0:GOTO 8726
     I7! = INT((p%(P, P1%(P), 22) / p%(P, P1%(P), 20)*1000+.5))
8726 D2 = 1:IF INFPOS%>0 THEN 8820

     IF MG%(D,690)<>999 THEN 8801
REM: PROFILE INFIELD IN WITH 3RD OCCUPIED
     IF S4%<3 OR S4%=4 THEN 8801
     CALL RANDNUM (RN)
     IF O%(0)=0 AND A1>=MG%(D,691)*-1 AND A1<=MG%(D,692)*-1 AND INNING%>=MG%(D,693) AND RN<=MG%(D,694) THEN INFPOS%=2:D2=3:GOTO 5055
     IF O%(0)=0 AND A1>=MG%(D,695)*-1 AND A1<=MG%(D,696)*-1 AND INNING%>=MG%(D,697) AND RN<=MG%(D,698) THEN INFPOS%=2:D2=3:GOTO 5055
     IF O%(0)=1 AND (S4%=5 OR S4%=7) AND A1>=MG%(D,699)*-1 AND A1<=MG%(D,700)*-1 AND INNING%>=MG%(D,701) AND RN<=MG%(D,702) THEN INFPOS%=2:D2=3:GOTO 5055
     IF O%(0)=1 AND (S4%=5 OR S4%=7) AND A1>=MG%(D,703)*-1 AND A1<=MG%(D,704)*-1 AND INNING%>=MG%(D,705) AND RN<=MG%(D,706) THEN INFPOS%=2:D2=3:GOTO 5055
8801 IF MG%(D,707)<>999 THEN 8802
     IF S4%<>0 THEN 8802
     CALL RANDNUM (RN)
REM: PROFILE CORNERS IN AGAINST A BUNT HIT
     IF BR>=MG%(D,708) AND RN<=MG%(D,709) OR br>=MG%(D,710) AND RN<=MG%(D,711) OR br>=MG%(D,712) AND RN<=MG%(D,713) THEN 8811
8802 IF MG%(D,714)<>999 OR B7%(P,B1!(P))=1 THEN 8803
     IF O%(0)<>0 THEN 8803
     IF S4%<>1 AND S4%<>4 THEN 8803
REM: PROFILE CORNERS IN VS HITTER IN SAC BUNT SITUATION
     CALL RANDNUM (RN)
     IF A1<=MG%(D,715) AND A1>=MG%(D,716)*-1 AND S4%=1 AND BR>=MG%(D,717) AND RN<=MG%(D,718) THEN 8811
     IF A1<=MG%(D,719) AND A1>=MG%(D,720)*-1 AND S4%=1 AND BR>=MG%(D,721) AND RN<=MG%(D,722) THEN 8811
     IF A1<=MG%(D,723) AND A1>=MG%(D,724)*-1 AND S4%=4 AND BR>=MG%(D,725) AND RN<=MG%(D,726) THEN 8811
     IF A1<=MG%(D,727) AND A1>=MG%(D,728)*-1 AND S4%=4 AND BR>=MG%(D,729) AND RN<=MG%(D,730) THEN 8811
8803 IF MG%(D,731)<>999 OR B7%(P,B1!(P))<>1 OR O%(0)=2 THEN 8804
     IF S4%<>1 AND S4%<>2 AND S4%<>4 THEN 8804
REM: PROFILE CORNERS IN VS A PITCHER IN SAC BUNT SITUATION
     CALL RANDNUM (RN)
     IF I7!<=MG%(D,732) AND RN<=MG%(D,733) AND S4%=1 AND O%(0)<=1 THEN 8811
     IF I7!<=MG%(D,734) AND RN<=MG%(D,735) AND S4%=1 AND O%(0)<=1 THEN 8811
     IF I7!<=MG%(D,736) AND RN<=MG%(D,737) AND (S4%=2 OR S4%=4) AND O%(0)=0 THEN 8811
     IF I7!<=MG%(D,738) AND RN<=MG%(D,739) AND (S4%=2 OR S4%=4) AND O%(0)=0 THEN 8811
8804 IF MG%(D,740)<>999 THEN 8805
     IF O%(0)=2 OR S4%<>3 AND S4%<5 THEN 8805
REM: PROFILE CORNERS IN FOR A SQUEEZE
     CALL RANDNUM (RN)
     IF O%(0)=0 AND A1<=MG%(D,741) AND A1>=MG%(D,742) AND br>=MG%(D,743) AND RN<=MG%(D,744) THEN 8811
     IF O%(0)=1 AND A1<=MG%(D,745) AND A1>=MG%(D,746) AND br>=MG%(D,747) AND RN<=MG%(D,748) THEN 8811
8805 IF MG%(D,756)<>999 THEN 8806
     IF S4%=1 OR S4%=4 OR S4%=5 OR S4%=7 THEN 8806
     MOB%=0
     FOR I=1 TO 3
     IF B0%(I, 0)>0 THEN MOB%=MOB%+1
     NEXT
     IF A1+MOB%+1>=0 THEN 8806:REM BATTER NOT TYING OR WINNING RUN
     CALL RANDNUM (RN)
     IF b%(P, B3%(P, B1!(P)), 4) = 0 THEN I7! = 0: I6!=0: GOTO 8806
     I6! = INT((b%(P, B3%(P, B1!(P)), 6)+b%(P, B3%(P, B1!(P)), 7)+b%(P, B3%(P, B1!(P)), 8)*2+b%(P, B3%(P, B1!(P)), 9)*3)/b%(P, B3%(P, B1!(P)), 4)*1000+.5)
     IF I6!<=MG%(D,757) AND INNING%>=MG%(D,758) AND RN<=MG%(D,759) THEN D2=8:GOTO 8850
     IF I6!<=MG%(D,760) AND INNING%>=MG%(D,761) AND RN<=MG%(D,762) THEN D2=8:GOTO 8850
     IF I6!<=MG%(D,763) AND INNING%>=MG%(D,764) AND RN<=MG%(D,765) THEN D2=8:GOTO 8850
8806 A1 = S6%(D, 0) - S6%(P, 0)
     IF A1 >= -1 AND A1 <= 1 AND INNING% >= 8 AND (S4% = 3 OR S4% >= 5) AND O%(0) < 2 THEN 8809
     IF A1 > 0 OR O%(0) > 0 OR S4% <= 2 OR S4% = 4 OR S4% = 5 AND O%(0) = 1 OR INNING% <= 5 AND A1 < -2 OR INNING% > 5 AND A1 < -4 THEN 8810
8809 INFPOS% = 2: D2 = 3: GOTO 5055
8810 IF S4% = 0 OR S4% = 2 OR S4% = 3 OR S4% >= 5 OR O%(0) = 2 OR B1!(P) >= 3 AND B1!(P) <= 6 OR INNING% <= 6 AND A1 >= 3 OR INNING% <= 6 AND A1 <= -3 THEN 8820
     IF INNING% > 7 AND A1 >= 1 OR INNING% > 7 AND A1 < -1 THEN 8820
8811 INFPOS% = 1: D2 = 4: GOTO 5055
8820 IF A1 <= 0 OR A1 = 0 AND S4% <= 1 OR A1 = 0 AND S4% = 4 OR S4% = 7 OR B7%(P, B1!(P)) = 1 THEN 8850
     IF INNING% >= 9 AND P = 1 AND O%(0) < 2 AND A1 = 0 AND S4% >= 2 AND S4% <= 3 OR INNING% >= 9 AND P = 1 AND O%(0) = 2 AND A1 = 0 AND S4% >= 5 AND S4% <= 6 THEN 8835
     I = B4%(P, 0, B1!(P)): I1 = B4%(P, 3, B1!(P)): IF B1!(P) = 9 THEN I2 = B4%(P, 0, 0): I3 = B4%(P, 3, 0)
     IF B1!(P) < 9 THEN I2 = B4%(P, 0, B1!(P) + 1): I3 = B4%(P, 3, B1!(P) + 1)
     IF S4% <> 2 AND S4% <> 6 THEN 8850
     IF I - I2 >= 50 AND I1 - I3 >= 50 AND A1 < 1 AND INNING% > 8 OR I >= 280 AND I1 >= 140 AND INNING% > 8 AND I1 > I3 AND A1 < 1 THEN 8835
     REM:IF I-I2>=50 AND I1-I3>=50 OR I>=280 AND I1>=140 AND I1>I3 THEN 8835
     GOTO 8850
8835 D2 = 2: GOTO 5058
8850 IF S8%(D, 0) <= P8%(D, P1%(D), 4) + P8%(D, P1%(D), 1) AND S8%(D, 1) <= P8%(D, P1%(D), 2) THEN CALL RELIEVERS :GOSUB 3955: GOTO 5055
     IF B9%(D, 1) = 99 THEN CALL RELIEVERS
     GOSUB 3955
     GOTO 5055
8900 JM = B3%(P, B1!(P))
     IF PPH = 1 THEN 8905
     A1 = S6%(P, 0) - S6%(D, 0)
     IF B7%(P, B1!(P)) <> 1 THEN 8906
     FOR I = 0 TO 21
     IF P%(P, I, 15) = 0 THEN 8903
     NEXT
     GOTO 8941
8903 IF MG%(P,590)=999 THEN 8985
     IF INNING% > 4 AND INNING% < 7 AND A1 < -2 AND S6%(P, 0) > 0 THEN 8930
     IF INNING% > 4 AND INNING% < 7 AND A1 < -3 THEN 8930
     IF INNING% > 6 AND A1 < 0 OR A1 = 0 AND INNING% > 6 AND INNING% < 9 AND S4% > 1 THEN 8930
     IF A1 = 0 AND INNING% >= 9 AND S4% > 0 OR A1 = 0 AND INNING% >= 9 AND O%(0) = 0 THEN 8930
     IF P%(P, P1%(P), 6) / P%(P, P1%(P), 4) <= 2 AND P8%(P, P1%(P), 0) >= 6 THEN 8930
8905 GOTO 8941
8906 IF MG%(P,847)=999 THEN 8991
     IF B%(P, JM, 4) = 0 THEN 8909
     IF B%(P, JM, 6) / B%(P, JM, 4) * 1000 > 233 OR B%(P, JM, 9) >= 15 THEN 8908
     IF INNING% > 6 AND A1 < -2 OR A1 <= 0 AND INNING% >= 8 OR A1 = 0 AND INNING% >= 9 THEN 8909
8908 GOTO 8941
8909 REM:GET A PINCH HITTER FOR A POSITION PLAYER
     IF MG%(P,540)=999 THEN 8993
8935 FOR I = 0 TO 22: IF B%(P, I, 21) > 0 OR B$(P, I) = "XXX" OR B%(P, I, 4) = 0 THEN 8917
     IF B7%(P, B1!(P)) <> B%(P, I, 22) AND B7%(P, B1!(P)) <> B%(P, I, 23) AND B7%(P, B1!(P)) <> B%(P, I, 24) AND B7%(P, B1!(P)) <> B%(P, I, 25) THEN 8917
     IF P%(D, P1%(D), 0) = 1 AND B%(P, I, 0) > 0 AND B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 35 THEN 8916
     IF P%(D, P1%(D), 0) = -1 AND B%(P, I, 0) < 0 AND B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 45 THEN 8916
     IF B%(P, I, 0) = 0 AND B%(P, I, 6) / B%(P, I, 4) * 1000 > B%(P, JM, 6) / B%(P, JM, 4) * 1000 + 30 THEN 8916
     GOTO 8917
8916 PPH = 1: D0%(P) = D0%(P) + 1: X0%(P, 0, D0%(P)) = I: X0%(P, 1, D0%(P)) = B7%(P, B1!(P)): X0%(P, 2, D0%(P)) = (B1!(P) * 10) + (D0%(P) - 9): GOTO 8919
8917 NEXT I
     GOTO 8925
8919 B%(P, I, 21) = 1: B3%(P, B1!(P)) = I: B9%(P, B7%(P, B1!(P))) = I
     IF B7%(P, B1!(P)) = 1 OR B7%(P, B1!(P)) = 10 THEN B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10
     I7% = P: I8% = B1!(P): I9% = B3%(P, B1!(P))
     CALL REPLACEMENTS (I7%,I8%,I9%)
     GOTO 8938
8925 GOTO 8941
8930 REM:GET A PINCH HITTER FOR THE PITCHER
     IF MG%(P,540)=999 THEN 8990
8933 J% = 0: GP% = 0

     IF CGERA%>1 AND INNING%<4 THEN 8941
     IF CGERA%>2 AND INNING%<7 THEN 8941
     IF CGERA%>3 AND inning%<9 then 8941
     IF CGERA%>=8 THEN 8941
     IF CGERA%>0 AND INNING%>7 AND S6%(P,0)>S6%(D,0) AND P%(P,P1%(P),1)>0 AND P6%(P)=1 AND INT(P%(P,P1%(P),16)/P%(P,P1%(P),1)*100+.5)<=100 AND INT(RND(1)*100)+1<=INT(P%(P,P1%(P),16)/P%(P,P1%(P),1)*100+.5) THEN 8941
     IF CGERA%>0 AND INNING%>7 AND (S6%(P,0)>=S6%(D,0) OR S6%(D,0)-S6%(P,0)<=2) AND P%(P, P1%(P), 1) > 0 AND P6%(P)=1 AND INT(P%(P, P1%(P), 16) / P%(P, P1%(P), 1) * 100 + .5)>=100 THEN 8941

     FOR I = 0 TO 22
     IF B$(P, I) = "XXX" OR B%(P, I, 21) > 0 OR B%(P, I, 4) = 0 THEN 8932
     IF B%(P, I, 0) > 0 AND P%(D, P1%(D), 0) = 1 OR B%(P, I, 0) < 0 AND P%(D, P1%(D), 0) = -1 THEN PH%(J%, 0) = INT(B%(P, I, 3) / 4 + .5): GOTO 8931
     PH%(J%, 0) = B%(P, I, 3)
8931 PH%(J%, 1) = I: GP% = GP% + PH%(J%, 0): J% = J% + 1
8932 NEXT
     IF J% = 0 THEN 8941
     RN = INT(RND(1) * GP%) + 1
     FOR I = 0 TO J% - 1
     IF RN <= PH%(I, 0) THEN PH% = PH%(I, 1): GOTO 8934
     PH%(I + 1, 0) = PH%(I, 0) + PH%(I + 1, 0)
     NEXT: GOTO 8941
8934 I = PH%: PPH = 1: D0%(P) = D0%(P) + 1
     X0%(P, 0, D0%(P)) = I
     X0%(P, 1, D0%(P)) = 10
     X0%(P, 2, D0%(P)) = 81 + D0%(P)
     B%(P, I, 21) = 1: B3%(P, B1!(P)) = I
     IF B7%(P, B1!(P)) = 1 OR B7%(P, B1!(P)) = 10 THEN B9%(P, 1) = 99: B9%(P, 0) = B1!(P): B7%(P, B1!(P)) = 10
8938 P9 = P: W5 = P9: W6 = P9: VV(P9) = B1!(P): VV1(P9) = B1!(P)
     CALL HITTINGLINES(L%(), P%(), P1%(), B7%(), B%(), VV(), VV1(), B3%(), B4%(), K9!(), WF!)
     CALL FLDDISP (BC%,P,U%,D)
     CALL MAINSCREEN (D,P,BC%,U%,MO)
     VV(P9)=1:VV1(P9)=9
     Q$(0) = B$(P, B3%(P, B1!(P)))
     IF B7%(P, B1!(P)) = 1 THEN Q$(0) = P$(P, P1%(D))
     CALL NEWLINES: CALL PBP (M$(P)+" is sending up a pinch hitter"): PPH = 1: GOTO 5052

8941 IF PR%<>1 THEN CALL PINCHRUNNER (MS%,A1,S4%,I2,P,PR%,BC%,U%,D,MO)
     IF MS% = 1 THEN MS% = 0: GOTO 5052


8971 IF B7%(P, B1!(P)) = 1 THEN BR = 0
     IF B7%(P, B1!(P)) <> 1 THEN BR = B%(P, B3%(P, B1!(P)), 30)
     CALL RANDNUM(RN)
     IF S4% = 0 AND (INFPOS% > 0 AND RN <= 5 OR INFPOS% = 0 AND RN <= 5) AND BR > 8 THEN P2 = 4: GOTO 4100
REM: MANAGER PROFILE TO HIT AND RUN
     CALL RANDNUM (RN)
     IF S4%=0 OR MG%(P,671)<>999 OR B7%(P, B1!(P)) = 1 THEN 8945
     IF b%(P, B3%(P, B1!(P)), 4) = 0 THEN I7!=0 ELSE I7! = INT((b%(P, B3%(P, B1!(P)), 6)+b%(P, B3%(P, B1!(P)), 7)+b%(P, B3%(P, B1!(P)), 8)*2+b%(P, B3%(P, B1!(P)), 9)*3)/b%(P, B3%(P, B1!(P)), 4)*1000+.5)
     IF b%(P, B3%(P, B1!(P)), 12) = 0 THEN I6!=0 ELSE I6! = INT(b%(P, B3%(P, B1!(P)), 4) / b%(P, B3%(P, B1!(P)), 12)+.5)
8949 IF S4%=1 AND O%(0)=0 AND A1<=MG%(P,672) AND A1>=MG%(P,673)*-1 AND RN<=MG%(P,674) AND I7!<=MG%(p,872) AND I6!>=MG%(p,866) THEN P2 = 2:GOTO 5067
     IF S4%=1 AND O%(0)=1 AND A1<=MG%(P,675) AND A1>=MG%(P,676)*-1 AND RN<=MG%(P,677) AND I7!<=MG%(p,873) AND I6!>=MG%(p,867) THEN P2 = 2:GOTO 5067
     IF S4%=5 AND O%(0)=0 AND A1<=MG%(P,678) AND A1>=MG%(P,679)*-1 AND RN<=MG%(P,680) AND I7!<=MG%(p,874) AND I6!>=MG%(p,868) THEN P2 = 2:GOTO 5067
     IF S4%=5 AND O%(0)=1 AND A1<=MG%(P,681) AND A1>=MG%(P,682)*-1 AND RN<=MG%(P,683) AND I7!<=MG%(p,875) AND I6!>=MG%(p,869) THEN P2 = 2:GOTO 5067
     IF S4%=4 AND O%(0)=0 AND A1<=MG%(P,684) AND A1>=MG%(P,685)*-1 AND RN<=MG%(P,686) AND I7!<=MG%(p,876) AND I6!>=MG%(p,870) THEN P2 = 2:GOTO 5067
     IF S4%=4 AND O%(0)=1 AND A1<=MG%(P,687) AND A1>=MG%(P,688)*-1 AND RN<=MG%(P,689) AND I7!<=MG%(p,877) AND I6!>=MG%(p,871) THEN P2 = 2:GOTO 5067
8945 IF MG%(P,609)<>999 OR B7%(P, B1!(P)) <> 1 THEN 8946
     IF S4%<>1 AND S4%<>2 AND S4%<>4 THEN 8946
     CALL RANDNUM (RN)
     IF S4%=1 AND INNING%<=7 AND MG%(P,610)>=O%(0) AND A1<=MG%(P,611) AND A1>=MG%(P,612)*-1 AND RN<=MG%(P,613) THEN 8970
     IF S4%=1 AND INNING%>7 AND MG%(P,614)>=O%(0) AND A1<=MG%(P,615) AND A1>=MG%(P,616)*-1 AND RN<=MG%(P,617) THEN 8970
     IF S4%=2 AND INNING%<=7 AND MG%(P,618)>=O%(0) AND A1<=MG%(P,619) AND A1>=MG%(P,620)*-1 AND RN<=MG%(P,621) THEN 8970
     IF S4%=2 AND INNING%>7 AND MG%(P,622)>=O%(0) AND A1<=MG%(P,623) AND A1>=MG%(P,624)*-1 AND RN<=MG%(P,625) THEN 8970
     IF S4%=4 AND INNING%<=7 AND MG%(P,626)>=O%(0) AND A1<=MG%(P,627) AND A1>=MG%(P,628)*-1 AND RN<=MG%(P,629) THEN 8970
     IF S4%=4 AND INNING%>7 AND MG%(P,630)>=O%(0) AND A1<=MG%(P,631) AND A1>=MG%(P,632)*-1 AND RN<=MG%(P,633) THEN 8970
8946 IF MG%(P,634)<>999 OR B7%(P, B1!(P))=1 OR O%(0)<>0 THEN 8947
     IF S4%<>1 AND S4%<>2 AND S4%<>4 THEN 8947
     IF b%(P, B3%(P, B1!(P)), 4) = 0 THEN I7! = 0: I6!=0: GOTO 8948
     I7! = INT(b%(P, B3%(P, B1!(P)), 6) / b%(P, B3%(P, B1!(P)), 4)*1000+.5)
     I6! = INT((b%(P, B3%(P, B1!(P)), 6)+b%(P, B3%(P, B1!(P)), 7)+b%(P, B3%(P, B1!(P)), 8)*2+b%(P, B3%(P, B1!(P)), 9)*3)/b%(P, B3%(P, B1!(P)), 4)*1000+.5)
8948 CALL RANDNUM (RN)
     IF S4%=1 AND INNING%<=7 AND A1<=MG%(P,635) AND A1>=MG%(P,636)*-1 AND I7!<=MG%(P,637) AND I6!<=MG%(P,638) AND P%(D,P1%(D),10)<=MG%(P,639) AND RN<=MG%(P,640) THEN 8970
     IF S4%=2 AND INNING%<=7 AND A1<=MG%(P,641) AND A1>=MG%(P,642)*-1 AND I7!<=MG%(P,643) AND I6!<=MG%(P,644) AND P%(D,P1%(D),10)<=MG%(P,645) AND RN<=MG%(P,646) THEN 8970
     IF S4%=4 AND INNING%<=7 AND A1<=MG%(P,647) AND A1>=MG%(P,648)*-1 AND I7!<=MG%(P,649) AND I6!<=MG%(P,650) AND P%(D,P1%(D),10)<=MG%(P,651) AND RN<=MG%(P,652) THEN 8970
     IF S4%=1 AND INNING%>7 AND A1<=MG%(P,653) AND A1>=MG%(P,654)*-1 AND I7!<=MG%(P,655) AND I6!<=MG%(P,656) AND P%(D,P1%(D),10)<=MG%(P,657) AND RN<=MG%(P,658) THEN 8970
     IF S4%=2 AND INNING%>7 AND A1<=MG%(P,659) AND A1>=MG%(P,660)*-1 AND I7!<=MG%(P,661) AND I6!<=MG%(P,662) AND P%(D,P1%(D),10)<=MG%(P,663) AND RN<=MG%(P,664) THEN 8970
     IF S4%=4 AND INNING%>7 AND A1<=MG%(P,665) AND A1>=MG%(P,666)*-1 AND I7!<=MG%(P,667) AND I6!<=MG%(P,668) AND P%(D,P1%(D),10)<=MG%(P,669) AND RN<=MG%(P,670) THEN 8970

8947 P2 = 1
     IF S4% = 1 OR S4% = 5 THEN 8944
     IF S4% = 2 OR S4% = 4 THEN 8950
8943 GOTO 8960
8944 IF A1 <= -1 AND B%(P, B3%(P, B0%(1, 0)), 13) < 10 OR B0%(1, 2) > 1 THEN 8943: REM:CHANGED FROM <10
     IF A1 >= -1 AND S4% = 5 THEN 8943
     IF A1 < -5 OR B7%(P, B0%(1, 0)) = 1 AND S4% = 1 OR B7%(P, B0%(2, 0)) = 1 AND S4% = 2 THEN 8943
     IF A1 < 0 AND INNING% >= 7 THEN 8943
     P2 = 3: GOTO 4800
8950 IF B%(P, B3%(P, B0%(2, 0)), 13) < 40 OR B7%(P, B1!(P)) = 1 OR O%(0) = 2 THEN 8943
     IF B0%(2, 2) > 1 THEN 8943
     P2 = 3: GOTO 4800
8960 IF S4% = 0 OR O%(0) > 0 OR S4% = 7 OR S4% = 6 OR S4% = 3 OR S4% = 5 OR U% = 2 OR P = 0 AND INNING% > 7 THEN 8968
     IF B7%(P, B1!(P)) = 1 AND S4% = 1 AND O%(0) < 2 OR B7%(P, B1!(P)) = 1 AND S4% = 2 AND O%(0) = 0 OR B7%(P, B1!(P)) = 1 AND S4% = 4 AND O%(0) = 0 THEN 8969
     IF B1!(P) > 2 AND B1!(P) < 6 OR B4%(P, 0, B1!(P)) > 290 AND B7%(P, B1!(P)) <> 1 OR B7%(P, B1!(P)) <> 1 AND B4%(P, 3, B1!(P)) > 70 THEN 8968
     IF INNING% > 7 AND A1 = -1 AND O%(0) = 0 AND S4% = 1 THEN 8969
8968 PPH = 0: PR% = 0: GOTO 8980
8969 IF O%(0) >= 1 AND INNING% >= 7 THEN 8968
8970 P2 = 4: GOTO 4100
8980 IF O%(0) = 2 OR B4%(P, 4, B1!(P)) > 100 OR S4% = 0 THEN 5064
     IF S4% = 7 OR S4% = 6 OR S4% = 3 OR S4% = 2 OR A1 < -3 THEN 5064
     IF INT(RND(1) * 100) + 1 > 20 THEN 5064
     P2 = 2
     GOTO 5067
8985 REM:PH FOR PITCHER WITH PROFILE
     CALL RANDNUM(RN)
     MOB%=0
     FOR I=1 TO 3
     IF B0%(I, 0)>0 THEN MOB%=MOB%+1
     NEXT
     IF p% (P, P1%(P), 11) <> 999 THEN I7! = p%(I, P1%(I), 11):GOTO 8986
     IF p%(P, P1%(P), 20) = 0 THEN I7!=0:GOTO 8986
     I7! = INT((p%(P, P1%(P), 22) / p%(P, P1%(P), 20)*1000+.5))
8986 FOR I=0 TO 2
     IF INNING% >= MG%(P,591+I*6) AND A1 <= MG%(P,592+I*6)*-1 AND O%(0)<= MG%(P,594+I*6) AND I7!<=MG%(P,595+I*6) AND RN<=MG%(P,596+I*6) AND MOB%>=MG%(P,593+I*6) THEN 8930
     NEXT
     GOTO 8905
8990 REM:SELECT PH FOR PITCHER
     I=P%(D,P1%(D),0)
     I1=6
     IF I<0 THEN I1=1
     FOR I=I1 TO I1+4
     IF MG%(P,I1+540)<>-1 AND B%(P,MG%(P,I1+540),21)=0 THEN PH%=MG%(P,I1+540):MG%(P,I1+540)=-1:GOTO 8934
     NEXT
     GOTO 8933
8991 REM:PH FOR POS PLAYER CASES
     CALL RANDNUM (RN)
     MOB%=0
     FOR I=1 TO 3
     IF B0%(I, 0)>0 THEN MOB%=MOB%+1
     NEXT
     IF b%(P, B3%(P, B1!(P)), 4) = 0 THEN I7! = 0: I6!=0: GOTO 8992
     I7! = INT(b%(P, B3%(P, B1!(P)), 6) / b%(P, B3%(P, B1!(P)), 4)*1000+.5)
     I6! = INT((b%(P, B3%(P, B1!(P)), 6)+b%(P, B3%(P, B1!(P)), 7)+b%(P, B3%(P, B1!(P)), 8)*2+b%(P, B3%(P, B1!(P)), 9)*3)/b%(P, B3%(P, B1!(P)), 4)*1000+.5)
8992 FOR I=0 TO 2
     IF INNING% >= MG%(P,848+I*6) AND A1 <= MG%(P,849+I*6)*-1 AND I6!<= MG%(P,851+I*6) AND I7!<=MG%(P,852+I*6) AND RN<=MG%(P,853+I*6) AND MOB%>=MG%(P,850+I*6) THEN 8909
     NEXT
     GOTO 8905

8993 REM:SELECT PH FOR HITTER
     FOR I = 0 TO 22
     IF B%(P, I, 21) > 0 OR B$(P, I) = "XXX" THEN 8994
     IF B7%(P, B1!(P)) <> B%(P, I, 22) AND B7%(P, B1!(P)) <> B%(P, I, 23) AND B7%(P, B1!(P)) <> B%(P, I, 24) AND B7%(P, B1!(P)) <> B%(P, I, 25) THEN 8994
     GOTO 8995
8994 NEXT
     GOTO 8935
8995 I=P%(D,P1%(D),0)
     I1=6
     IF I<0 THEN I1=1
     FOR I=I1 TO I1+4
     IF MG%(P,I1+540)<>-1 AND B%(P,MG%(P,I1+540),21)=0 THEN I=MG%(P,I1+540):MG%(P,I1+540)=-1:GOTO 8916
     NEXT
     GOTO 8935

SUB DisplayPCXFile (FileName$, VideoPage%) STATIC

   L = INSTR(FileName$, ".")   'Append the .PCX extension if it isn't already there
   IF L = 0 THEN FileName$ = FileName$ + ".PCX"

   Array$ = SPACE$(68 + 768)   'Set up an array for the header and VGA 256 palette
   IF NOT OpenPCXFile(FileName$, Array$) THEN EXIT SUB

   CALL WhichPCXScreen(Array$, WhichScreen%)

   CALL HandlePCXPalette(Array$, WhichScreen%)

   'Each of the if statements before the CALL's to Screen statements
   'could be removed if you want the screen mode to be set by this routine
   'and not by calling SetVideo prior to calling this routine.  What the
   'if statements do is attempt not to set the screen mode if it the computer
   'is already in that screen mode.
   IF WhichScreen = 4 OR WhichScreen = 6 THEN
      IF WhichScreen = 4 THEN CALL Screen1(0) ELSE CALL Screen2(0)
      CALL DispPCXC(VideoPage%)
   ELSEIF WhichScreen = 2 THEN
      IF GPDat%(31) <> 3 THEN CALL Screen3(0)
      CALL DispPCXH(VideoPage%)
   ELSEIF WhichScreen = 9 AND GPDat%(31) = 8 THEN
      CALL Screen13(0)
      CALL DispPCXV256(VideoPage%)
   ELSEIF WhichScreen = 5 THEN
      IF GPDat%(31) = 8 THEN CALL Screen9(0)
      CALL DispPCXVE(VideoPage%)
   ELSE 'WhichScreen = 8
      'Un-REM this next line if you want the routine to set its own screen
      'mode.  Otherwise if the image is VGA, then the machine is likely to
      'be set in either VGA or EGA mode already.
      'IF GPDat%(31) = 5 THEN CALL Screen9(0) ELSE CALL Screen12(0)
      CALL DispPCXVE(VideoPage%)
   END IF
  
   Array$ = ""     'Empty the Array$.  We're done working with it.

END SUB
SUB HandlePCXPalette (Array$, WhichScreen) STATIC

  PCXVersion = ASC(MID$(Array$, 2, 1))
  IF PCXVersion <> 2 AND PCXVersion <> 5 THEN EXIT SUB

  I = 17
  IF WhichScreen = 9 THEN I = 69

  IF WhichScreen = 8 THEN
        IF GPDat%(31) = 5 THEN
          FOR T = 0 TO 15
        N = 0
        a = ASC(MID$(Array$, I, 1)) \ 64
        IF (a AND 1) = 1 THEN N = N + 32
        IF (a AND 2) = 2 THEN N = N + 4
        I = I + 1
        b = ASC(MID$(Array$, I, 1)) \ 64
        IF (b AND 1) = 1 THEN N = N + 16
        IF (b AND 2) = 2 THEN N = N + 2
        I = I + 1
        c = ASC(MID$(Array$, I, 1)) \ 64
        IF (c AND 1) = 1 THEN N = N + 8
        IF (c AND 2) = 2 THEN N = N + 1
        I = I + 1
        CALL SetPaletteEGA(T, N)
          NEXT
        ELSE
          FOR T = 0 TO 15
        CALL SetPaletteEGA(T, T)
          NEXT
          FOR T = 0 TO 15
        a = ASC(MID$(Array$, I, 1)) \ 4
        I = I + 1
        b = ASC(MID$(Array$, I, 1)) \ 4
        I = I + 1
        c = ASC(MID$(Array$, I, 1)) \ 4
        I = I + 1
        CALL SetPalTripleVGA(T, a, b, c)
          NEXT
        END IF
  ELSEIF WhichScreen = 5 THEN
        FOR T = 0 TO 15
          N = 0
          a = ASC(MID$(Array$, I, 1)) \ 64
          IF (a AND 1) = 1 THEN N = N + 32
          IF (a AND 2) = 2 THEN N = N + 4
          I = I + 1
          b = ASC(MID$(Array$, I, 1)) \ 64
          IF (b AND 1) = 1 THEN N = N + 16
          IF (b AND 2) = 2 THEN N = N + 2
          I = I + 1
          c = ASC(MID$(Array$, I, 1)) \ 64
          IF (c AND 1) = 1 THEN N = N + 8
          IF (c AND 2) = 2 THEN N = N + 1
          I = I + 1
          CALL SetPaletteEGA(T, N)
        NEXT
  ELSEIF WhichScreen = 9 THEN
        FOR T = 0 TO 255
          a = ASC(MID$(Array$, I, 1)) \ 4
          I = I + 1
          b = ASC(MID$(Array$, I, 1)) \ 4
          I = I + 1
          c = ASC(MID$(Array$, I, 1)) \ 4
          I = I + 1
          CALL SetPalTripleVGA(T, a, b, c)
        NEXT
  END IF
 
END SUB

SUB WhichPCXScreen (Array$, WhichScreen) STATIC

  Manufacturer = ASC(MID$(Array$, 1, 1))
  PCXVersion = ASC(MID$(Array$, 2, 1))

  Compression = ASC(MID$(Array$, 3, 1))
  BitsPerPixel = ASC(MID$(Array$, 4, 1))

  XMin = CVI(MID$(Array$, 5, 2))          'Image locations
  YMin = CVI(MID$(Array$, 7, 2))
  XMax = CVI(MID$(Array$, 9, 2))
  YMax = CVI(MID$(Array$, 11, 2))

  XSize = CVI(MID$(Array$, 13, 2))        'Monitor Display Size
  YSize = CVI(MID$(Array$, 15, 2))
 
  NumPlanes = ASC(MID$(Array$, 66, 1))    'Number of video planes
  BytesPerLine = CVI(MID$(Array$, 67, 2)) 'Bytes Per Line

  'Determine Monitor Needed
  IF (XMax - XMin + 1) = 320 OR XSize = 320 THEN
        IF BitsPerPixel = 8 THEN
          WhichScreen = 9
        ELSE
          WhichScreen = 4
        END IF
  ELSEIF (XSize = 720) OR (XMax - XMin + 1) = 720 THEN
        WhichScreen = 2
  ELSEIF (XSize = 640) OR (XMax - XMin + 1) = 640 THEN
        IF (YSize = 200) OR (YMax - YMin + 1) = 200 THEN
          WhichScreen = 6
        ELSEIF (YSize = 350) OR (YMax - YMin + 1) <= 350 THEN
          WhichScreen = 5
        ELSEIF (YSize = 480) OR (YMax - YMin + 1) <= 480 THEN
          WhichScreen = 8
        END IF
  END IF

  IF WhichScreen = 0 THEN
        IF NumPlanes > 1 THEN WhichScreen = 8 ELSE WhichScreen = 6
        IF BitsPerPixel = 2 THEN WhichScreen = 4
        IF BitsPerPixel = 8 THEN WhichScreen = 9
  END IF

END SUB
SUB WEATHER
     CALL SCREEN0(0)
     CALL DRAWBOX (5)
     LOCATE 9, 11
     IF L%(1, 9) = 1 THEN LOCATE , 11: PRINT "TEMPERATURE: 70": LOCATE , 11: PRINT "WEATHER: DOME": LOCATE , 11: PRINT "WIND: NONE": GOTO 8719
     LOCATE , 11: PRINT "TEMPERATURE: "; TP%
     LOCATE , 11: PRINT "SKY CONDITIONS: "; SC$(SC%)
     WS% = INT(RND(1) * 15) + 7
     LOCATE , 11: PRINT "WIND: ";WI$
     LOCATE , 11: PRINT "PRECIPITATION: "; PC$(PC%)
8719 I$=INKEY$:IF I$="" THEN 8719
END SUB
