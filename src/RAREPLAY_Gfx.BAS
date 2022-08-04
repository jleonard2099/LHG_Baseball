DEFINT A-Z
  '$INCLUDE: 'GWDECL.BAS'
DECLARE SUB ADDPIT (D,I5,I6)
DECLARE SUB ADVANCEMENT (P,D,S2%,F%,I3)
DECLARE SUB PBP (F$)
DECLARE SUB FLDDISP(BC%, p, U%, D)
DECLARE SUB DELAY ()
DECLARE SUB PLYRHRT (P9,W5,W6,BC%,U%,D,MO)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB INCPITOFF (P,I5,I6)
DECLARE SUB SCOREBOARD ()
DECLARE SUB EJECTINC (EJ%,X$)
DECLARE SUB EJECTIONINJURY (D,PO%)
DECLARE SUB INJURYLENGTH (D,PO%)
DECLARE SUB INJPIT (P,D,IP%)
DECLARE SUB CREDITASSIST (W%,D)
DECLARE SUB CREDITPUTOUT (I3,FB2P%,AST%,D)
DECLARE SUB CREDITREGPO (W%,D)
DECLARE SUB NEWLINES ()
DECLARE SUB LONGFLY (D,W%)
DIM K9(1) AS SINGLE
TYPE PBP
YY AS STRING * 30
END TYPE
  DIM s$(1), A$(1), L%(1, 13), T1(1), b%(1, 22, 80), p%(1, 21, 89), b$(1, 22), p$(1, 21), P8%(1, 21, 41), H$(4)
  DIM B8%(1, 22, 21), P6%(1), P1%(1), s8%(1, 1), B1!(9), B0%(3, 2), B1%(3, 1), S1%(1), B9%(1, 10), NN$(1)
  DIM s%(1, 26), B3%(1, 9), B7%(1, 9), O%(1), S6%(1, 2), B1$(4), B4%(1, 7, 9), X$(12), c$(11), F$(10)
  DIM VV(1), VV1(1), D3%(1), m%(9), P2%(1), a5%(4), U$(4), DP%(1), EJ$(20), IJ$(20)
  DIM X0%(1, 2, 23), D0%(1), P5%(1, 21, 41), B5%(1, 22, 21), TP%(11, 2), L0%(1), T3%(22)
  DIM T%(22), P4%(22), B2%(1, 22), P3%(1, 21)
  DIM MG%(1,999),TS%(1,11),SA%(1,24)
  DIM CSS%(1,22,6),SU%(1,1)
  DIM NG%(18), YN$(1), Q$(23), H0$(1), tm$(4), SC$(4), PC$(4), N$(1), D$(3), m$(1)
  DIM H0%(1), CZ%(1), BP(1), BT%(1, 9, 9), CF%(9, 2), LB%(9, 1), SO%(5, 2)
  DIM PR%(24), GK%(1),AP%(2),VA%(26)
COMMON SHARED TS%(),SA%(),P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
COMMON SHARED C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
COMMON SHARED S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
COMMON SHARED B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
COMMON SHARED VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), h$(), D0%(), M$(), LB%()
COMMON SHARED F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
COMMON SHARED S$, P, U6, DH%, U9, G4 AS SINGLE, RE, RD
COMMON SHARED TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, CC%, AP%,INNING%,BUCK AS PBP
COMMON SHARED MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%
'$INCLUDE: 'COMMON.GW'
SUB RAREPLAYS (H6%, H7%, IP%)
     IF H7% = 20 THEN H7% = 0: GOTO 2008
2000 CALL NEWLINES: H6% = INT(RND(1) * 100) + 1
     IF H6% = 1 THEN F$=b$(D, B9%(D, 2))+" is arguing the calls":CALL PBP (F$)
     IF H6% = 1 THEN CALL PBP (U$(1)+" has heard enough")
     IF H6% = 1 THEN CALL PBP (b$(D, B9%(D, 2))+" has been ejected!!"): X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$): po% = 2: CALL EJECTIONINJURY (D,PO%): GOTO 2850
     IF H6% <= 3 THEN CALL PBP (p$(D, P1%(D))+" steps off the mound")
     IF H6% <= 3 THEN CALL PBP ("He is shouting at "+U$(1))
     IF H6% <= 3 THEN CALL PBP (U$(1)+" does not appreciate it..."):CALL PBP (p$(D, P1%(D))+" has been ejected!!")
     IF H6% <= 3 THEN X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL INJPIT (P,D,IP%): GOTO 2850
     IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN CALL PBP (Q$(0)+" is unhappy with that last call"):CALL PBP ("He is venting his opinion to "+U$(1)):CALL PBP ("Thats it!! "+U$(1)+" ejects him!!")
     IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO)
     IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN 2850
     IF H6% = 4 THEN CALL PBP (Q$(0)+" is unhappy with the last call!!"): GOTO 2850
     IF H6% > 5 THEN 2005
     call pbp (Q$(0)+" hits a slow roller to first")
     call pbp (b$(D, B9%(D, 3))+" fields it cleanly")
     call pbp ("He flips it to "+p$(D, P1%(D))+" covering just in time!")
     call pbp ("Wait... "+U$(2)+" rules safe!!")
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
     FOR i = 0 TO 3: a5%(i) = 1: NEXT i: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
     CALL NEWLINES
     call pbp (m$(D)+" charges from the dugout")
     call pbp (m$(D)+", "+p$(D, P1%(D))+",")
     call pbp (b$(D, B9%(D, 2))+", and "+b$(D, B9%(D, 3)))
     call pbp ("are going at it with "+U$(2))
     call pbp (U$(3)+" and "+U$(1)+" come to help")
     call pbp ("This argument is out of control now!!")
     call pbp ("What is this!!")
     call pbp ("All of them have been ejected!!!")
     X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$)
     X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$)
     X$ = b$(D, B9%(D, 3)): CALL EJECTINC (EJ%,X$)
     X$ = m$(D): CALL EJECTINC (EJ%,X$)
     po% = 2: CALL EJECTIONINJURY (D,PO%): po% = 3: CALL EJECTIONINJURY (D,PO%): CALL INJPIT (P,D,IP%): GOTO 2850
2005 IF H6% > 7 THEN 2007
     call pbp (Q$(0)+" sends a long one")
     call pbP ("down the left field line")
     call pbp ("It may go..."):call pbp ("HOME RUN!!!")
     call pbp ("Wait!!! "):call pbp (U$(4)+" is calling it foul!!")
     CALL NEWLINES
     call pbp (Q$(0)+" is arguing vehemently")
     call pbp (m$(p)+" charges from the dugout")
     call pbp ("He and "+Q$(0)+" are really fuming")
     call pbp (Q$(0)+" has just been tossed!!")
     X$ = Q$(0): CALL EJECTINC (EJ%,X$)
     CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): GOTO 2850
2007 IF H6% = 8 THEN CALL PBP (U$(1)+" has just ejected "+p$(D, P1%(D)))
     IF H6% = 8 THEN CALL PBP ("He was wetting the ball!!"): CALL EJECTINC (EJ%,X$): EJ$(EJ%) = p$(D, P1%(D)): CALL INJPIT (P,D,IP%): GOTO 2850
     IF H6% = 9 THEN CALL PBP ("Here's the 3-2 pitch..."): CALL PBP ("Time called..."):CALL PBP (U$(1)+" is calling it ball 4"):CALL PBP ("He says "+p$(D, P1%(D))+" went to his mouth"): GOTO 2850
     IF H6% > 15 THEN 2006
     CALL PBP (Q$(0)+" hits the deck on a high")
     CALL PBP ("and tight pitch from "+p$(D, P1%(D)))
     CALL PBP (Q$(0)+" charges the mound!")
     CALL PBP ("Both dugouts have emptied...")
     CALL PBP ("It's a bench-clearing brawl!")
     CALL NEWLINES
     CALL PBP ("It will take time to clean this up"): i = INT(RND(1) * 3) + 1
     IF i = 1 THEN CALL PBP (p$(D, P1%(D))+" has been ejected"): X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL INJPIT (P,D,IP%): GOTO 2850
     IF i = 2 THEN CALL PBP (Q$(0)+" has been ejected"): X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): GOTO 2850
     CALL PBP (Q$(0)+" and "+p$(D, P1%(D))+" have been ejected"): X$ = Q$(0): CALL EJECTINC (EJ%,X$): X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL INJPIT (P,D,IP%): CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): GOTO 2850
2006 IF H6% > 21 THEN 2010
2008 H6%=21:CALL NEWLINES
     IF RD = 0 AND (PC% = 0 OR PC% = 4) THEN CALL PBP (Q$(0)+" fouls the pitch out of play"): GOTO 2850
     tm = INT(RND(1) * 59) + 2: CALL PBP ("The game has been halted by rain"): FOR i = 1 TO 2: CALL DELAY: NEXT
     CALL PBP ("Rain delay lasted"+STR$(tm)+" minutes"): CALL DELAY
     RD% = 1
     IF tm > 28 THEN CALL PBP ("Pitchers lost effectiveness during delay"): CALL PBP ("Their arms have stiffened"): CALL DELAY: FOR i = 0 TO 1: s8%(i, 0) = s8%(i, 0) - 2: NEXT
     GOTO 2850
2010 IF H6% > 24 THEN 2015
     IF s4% = 0 AND L%(1, 13) = 14 THEN CALL PBP (Q$(0)+" steps out of the box"):CALL PBP ("A jet is passing over"): GOTO 2850
     IF s4% = 0 THEN call pbp (U$(1)+" calls time to get new baseballs"): GOTO 2850
     F$=b$(D, B9%(D, 2))+" rifles the ball to "
     IF s4% = 1 THEN CALL PBP (F$+"1st..."): i = 2: GOTO 2012
     IF s4% = 2 OR s4% = 4 THEN CALL PBP (F$+"2nd base..."): i = 3: GOTO 2012
     CALL PBP (F$+"3rd base..."): i = 4
2012 call pbp (Q$(i - 1)+" is picked off base!")
     a5%(i - 1) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 2
     CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF O%(0) >= 3 THEN B1!(p)=B1!(p)-1:H6%=5:GOTO 2850
2015 IF H6% > 29 THEN 2020
     CALL PBP (Q$(0)+" pops it up near the 3rd base stands")
     CALL PBP ("A fan interferes...")
     CALL PBP (b$(D, B9%(D, 6))+" makes a great catch!!"): W% = 6: CALL CREDITREGPO (W%,D)
     O%(0) = O%(0) + 1: O%(1) = O%(1) + 1
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6)
     I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6)
     GOTO 2850
2020 IF H6% <= 31 THEN CALL PBP (Q$(0)+" pops it up foul near the stands")
     IF H6% <= 30 THEN CALL PBP (b$(D, B9%(D, 2))+" runs into the stands!!"): po% = 2: GOTO 2099
     IF H6% <= 31 THEN CALL PBP (b$(D, B9%(D, 3))+" gives chase"):CALL PBP ("he runs into the stands!!"): po% = 3: GOTO 2099
     IF H6% = 32 THEN call pbp (Q$(0)+" pops it up foul down the rf line"): CALL PBP (b$(D, B9%(D, 4))+" crashes into the wall"): po% = 4: GOTO 2099
     IF H6% = 33 THEN CALL PBP (Q$(0)+" pops it up foul")
     IF H6% = 33 THEN CALL PBP ("it's by the 3rd base dugout")
     IF H6% = 33 THEN CALL PBP (b$(D, B9%(D, 5))+" tumbles in!!"): po% = 5: GOTO 2099
     IF H6% = 34 THEN call pbp (Q$(0)+" pops it up foul")
     IF H6% = 34 THEN CALL PBP ("down the lf line")
     IF H6% = 34 THEN CALL PBP (b$(D, B9%(D, 6))+" tumbles over the railing!!"):PO%=6:GOTO 2099
     IF H6% > 37 THEN 2023
     CALL PBP (Q$(0)+" lines one to deep left"):CALL PBP (b$(D, B9%(D, 7))+" races back..."):CALL PBP ("He slams into the wall and is down!!")
     op% = 7: CALL PBP (b$(D, B9%(D, 8))+" comes over"):CALL PBP ("He gets the ball in")
2021 I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
     FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES: po% = op%: GOTO 2098
2023 IF H6% > 40 THEN 2025
     call pbp (Q$(0)+" drives a sinking liner to cf")
     CALL PBP (b$(D, B9%(D, 8))+" gets a good jump on it")
     CALL PBP ("He crashes into the wall")
     CALL PBP ("the ball bounces away!!")
     CALL PBP ("The runners are circling the bases")
     CALL PBP (b$(D, B9%(D, 9))+" retrieves the ball")
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): I6 = 14: CALL ADDPIT (D,I5,I6): W% = 8
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 6: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 29: CALL INCPITOFF (P,I5,I6)
     a5%(0) = 4: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3):CALL PBP ("It's an inside the park HOME RUN!!!"): CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 8: GOTO 2098
2025 IF H6% > 43 THEN 2027
     CALL PBP (Q$(0)+" lifts a deep drive to rf"):CALL PBP (b$(D, B9%(D, 9))+" races back..."):CALL PBP ("He slams into the wall and is down!!")
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 9: CALL PBP (b$(D, B9%(D, 8))+" comes over"):CALL PBP ("he gets the ball back in")
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 5: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 28: CALL INCPITOFF (P,I5,I6)
     a5%(0) = 3: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): call pbp ("Triple!!"):CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 9: GOTO 2098
2027 IF H6% > 44 THEN 2030
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = INT(RND(1) * 3) + 7:CALL PBP (Q$(0)+" hits a sinking liner to "+c$(W%))
     CALL PBP (b$(D, B9%(D, W%))+" dives for the catch...")
     CALL PBP ("the ball rolls free!")
     CALL PBP (b$(D, B9%(D, W% - 1))+" gets the ball in")
     op% = W%: GOTO 2021
2030 IF H6% = 45 THEN CALL PBP (Q$(0)+" is beaned and injured"): CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): GOTO 2850
     IF H6% <= 49 THEN call pbp (b$(D, B9%(D, 2))+" is injured by a foul tip!"): po% = 2: GOTO 2099
     IF H6% > 58 THEN 2037
     i = INT(RND(1) * 3) + 7
     IF i = 9 THEN hi1 = 3: hi2 = 9: hi3 = 8
     IF i = 8 THEN hi1 = 4: hi2 = 8: hi3 = 7
     IF i = 7 THEN hi1 = 6: hi2 = 7: hi3 = 8
     CALL PBP (Q$(0)+" hits a weak fly to short "+c$(i))
     CALL PBP (b$(D, B9%(D, hi1))+" goes out")
     CALL PBP (b$(D, B9%(D, hi2))+" is charging in")
     CALL PBP ("They collide and the ball rolls free!!")
     CALL PBP (b$(D, B9%(D, hi3))+" gets it back in")
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
     FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
     CALL NEWLINES: po% = hi1: CALL INJURYLENGTH (D,PO%): po% = hi2: CALL INJURYLENGTH (D,PO%): H6% = 5: GOTO 2850
2037 IF H6% <= 61 THEN CALL PBP (Q$(0)+" fouls one off his foot"):call pbp ("He is injured"):CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
     IF H6% > 64 THEN 2040
     call pbp (Q$(0)+" pops it up"):call pbp ("it's along the 3b line"):call pbp ("It drifts into foul territory")
     CALL PBP (b$(D, B9%(D, 5))+" and "+p$(D, P1%(D))+" give chase")
     call pbp ("they collide!!")
     CALL PBP (b$(D, B9%(D, 5))+" is down and injured"): po% = 5: GOTO 2099
2040 IF H6% > 70 THEN 2045
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 8: CALL LONGFLY (D,W%)
     CALL PBP (b$(D, B9%(D, 7))+" collides with "+b$(D, B9%(D, 8))+"!!")
     CALL PBP ("both fielders are down... ")
     CALL PBP (b$(D, B9%(D, 6))+" gets the ball back in")
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
     a5%(0) = 2: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1
     CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL PBP ("Double!!"): CALL SCOREBOARD
     CALL NEWLINES: po% = 7: CALL INJURYLENGTH (D,PO%)
     po% = 8: CALL INJURYLENGTH (D,PO%): H6% = 5
     GOTO 2850
2045 IF H6% > 85 THEN 2055
     CALL PBP (Q$(0)+" lines it hard")
     i = INT(RND(1) * 5) + 1: ON i GOTO 2046, 2047, 2047, 2047, 2047
2046 call PBP ("It's off the leg of "+p$(D, P1%(D)))
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
     call pbp ("He reaches on a single")
     call pbp (p$(D, P1%(D))+" is down and in pain!")
     CALL INJPIT (P,D,IP%): H6% = 5: GOTO 2850
2047 po% = i + 1
     CALL PBP ("it's off of "+b$(D, B9%(D, po%))+" for a single")
     IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
     IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
     I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
     FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
     CALL PBP (b$(D, B9%(D, po%))+" is down and injured")
     CALL INJURYLENGTH (D,PO%): H6% = 5: GOTO 2850
2055 IF H6% <= 89 THEN CALL PBP (p$(D, P1%(D))+" is motioning for "+m$(D)):CALL PBP ("It appears he has a blister"):CALL PBP ("He will have to leave the game"): CALL INJPIT (P,D,IP%): H6% = 1: GOTO 2850
     IF H6% = 90 THEN CALL PBP (Q$(0)+" takes a mighty swing!"):CALL PBP ("It looks like he injured himself"):CALL PLYRHRT (P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
     IF H6% = 91 THEN CALL PBP ("Time is called"):CALL PBP ("Morganna strikes again!!"): H6% = 1: GOTO 2850
     CALL PBP (U$(1)+" calls time to dust the plate"): H6% = 1: GOTO 2850
2098 CALL INJURYLENGTH (D,PO%): H6% = 5: GOTO 2850
2099 CALL INJURYLENGTH (D,PO%): H6% = 1: GOTO 2850
2850 REM:
END SUB




