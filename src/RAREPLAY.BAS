'$INCLUDE: 'COMMON.GW'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Rare Play"
$ExeIcon:'./lhg_fcbase.ico'
_Icon	
_AllowFullScreen

DEFINT A-Z

DECLARE SUB ADDPIT (D,I5,I6)
DECLARE SUB ADVANCEMENT (P,D,S2%,F%,I3)
DECLARE SUB DELAY ()
DECLARE SUB PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO)
DECLARE SUB INCBATOFF (P,I5,I6)
DECLARE SUB INCPITOFF (P,I5,I6)
DECLARE SUB SCOREBOARD ()
DECLARE SUB EJECTINC (EJ%,X$)
DECLARE SUB EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6)
DECLARE SUB INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6)
DECLARE SUB INJPIT (P,D)
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
Dim Shared TS%(),SA%(),P8%(), B8%(), P%(), P$(), B$(), P5%(), B5%(), X0%(), B%(), AP%(), A$()
Dim Shared C$(), T3%(), P6%(), P2%(), L0%(), DP%(), U$(), IJ$(), EJ$(), SC$(), PC$()
Dim Shared S%(), P1%(), S1%(), D$(), S6%(), L%(), D3%(), NG%(), A5%(), B1!(), B1%(), B7%()
Dim Shared B0%(), B3%(), B9%(), Q$(), PR%(), W%, GK%(), B1$(), X$(), S8%(), NN$(), N$(), O%()
Dim Shared VV(), VV1(), B4%(), K9() AS SINGLE, H0$(), h$(), D0%(), M$(), LB%()
Dim Shared F$(), TM$(), T1%(), YN$(), TP%(), SO%(), CF%(), BT%()
Dim Shared S$, P, U6, DH%, U9, displayPause AS SINGLE, RE, RD
Dim Shared TA%, SC%, WF AS SINGLE, PC%, TP%, WS%, WI$, AP%,INNING%,BUCK AS PBP
Dim Shared MG%(),CSS%(),D,IJ%,DR%,CGERA%,IJL%,EJ%


'------------------------------
'   RAREPLAYS Subroutine
'------------------------------
' ...explanation...
SUB RAREPLAYS (H6%, H7%)
IF H7% = 20 THEN H7% = 0: GOTO 2008

2000 CALL NEWLINES: H6% = INT(RND(1) * 100) + 1
	IF H6% = 1 THEN PRINT b$(D, B9%(D, 2)); " is arguing ball and strike calls": CALL DELAY
	IF H6% = 1 THEN PRINT U$(1); " has heard enough": CALL DELAY
	IF H6% = 1 THEN PRINT b$(D, B9%(D, 2)); " has been ejected!!": X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$): CALL DELAY: po% = 2: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): GOTO 2850
	IF H6% <= 3 THEN PRINT p$(D, P1%(D)); " steps off the mound"
	IF H6% <= 3 THEN PRINT "after that last call": CALL DELAY: PRINT "He is shouting at "; U$(1): CALL DELAY
	IF H6% <= 3 THEN PRINT U$(1); " does not appreciate what he hears...": CALL DELAY: PRINT p$(D, P1%(D)); " has been ejected!!"
	IF H6% <= 3 THEN X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN PRINT Q$(0); " is unhappy with that last call": CALL DELAY: PRINT "He is venting his opinion to "; U$(1): CALL DELAY: PRINT "Thats it!! "; U$(1); " has ejected "; Q$(0); "!!"
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN X$ = Q$(0): CALL EJECTINC (EJ%,X$):CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO)
	IF H6% = 4 AND B7%(p, B1!(p)) <> 1 THEN 2850
	IF H6% = 4 THEN PRINT Q$(0); " did not like that": PRINT "last call at all!!": CALL DELAY: GOTO 2850
	IF H6% > 5 THEN 2005
	PRINT Q$(0); " hits a slow roller to first"
	CALL DELAY: PRINT b$(D, B9%(D, 3)); " fields it cleanly"
	CALL DELAY: PRINT "He flips it to "; p$(D, P1%(D)); " covering just in time!"
	CALL DELAY: PRINT "Wait... "; U$(2); " rules safe!!": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT i: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
	CALL NEWLINES: PRINT m$(D); " charges from the dugout": CALL DELAY
	PRINT m$(D); ", "; p$(D, P1%(D)); ","
	PRINT b$(D, B9%(D, 2)); ", and "; b$(D, B9%(D, 3))
	PRINT "are going at it with "; U$(2)
	CALL DELAY: CALL DELAY: CALL DELAY
	PRINT U$(3); " and "; U$(1); " come in to help out": CALL DELAY
	PRINT "This argument is out of control now!!": CALL DELAY
	PRINT "What is this!!": CALL DELAY
	PRINT "All of them have been ejected!!!"
	X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$)
	X$ = b$(D, B9%(D, 2)): CALL EJECTINC (EJ%,X$)
	X$ = b$(D, B9%(D, 3)): CALL EJECTINC (EJ%,X$)
	X$ = m$(D): CALL EJECTINC (EJ%,X$)
	CALL DELAY: po% = 2: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): po% = 3: CALL EJECTIONINJURY (U6,P,U9,D,PO%,P9,W5,W6): CALL INJPIT (P,D): GOTO 2850

2005 IF H6% > 7 THEN 2007
	PRINT Q$(0); " sends a long one"
	PRINT "down the left field line": CALL DELAY
	PRINT "It may go...": CALL DELAY: PRINT "HOME RUN!!!": CALL DELAY
	PRINT "Wait!!! "; : CALL DELAY: PRINT U$(4); " is calling it foul!!": CALL DELAY: CALL NEWLINES
	PRINT Q$(0); " is arguing vehemently"
	PRINT "with "; U$(1); " and "; U$(4): CALL DELAY
	PRINT m$(p); " charges from the dugout": CALL DELAY
	PRINT "He and "; Q$(0); " are really going"
	PRINT "at it with the umpires!!": CALL DELAY: CALL DELAY
	PRINT Q$(0); " has just been tossed!!": X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL DELAY
	CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850

2007 IF H6% = 8 THEN PRINT U$(1); " has just ejected "; p$(D, P1%(D))
	IF H6% = 8 THEN PRINT "for wetting the ball!!": CALL EJECTINC (EJ%,X$): EJ$(EJ%) = p$(D, P1%(D)): CALL DELAY: CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF H6% = 9 THEN PRINT "Here's the 3-2 pitch...": CALL DELAY: PRINT "Time called...": PRINT U$(1); " is calling it ball 4": CALL DELAY: PRINT "He says "; p$(D, P1%(D)); " went to his mouth": CALL DELAY: GOTO 2850
	IF H6% > 15 THEN 2006
	PRINT Q$(0); " hits the deck on a high"
	PRINT "and tight pitch from "; p$(D, P1%(D)): CALL DELAY: CALL DELAY
	PRINT Q$(0); " charges the mound!": CALL DELAY
	PRINT "Both dugouts have emptied...": CALL DELAY
	PRINT "It's a bench-clearing brawl!": CALL DELAY
	CALL NEWLINES: PRINT "It will take a few minutes to clean this up": CALL DELAY: i = INT(RND(1) * 3) + 1
	IF i = 1 THEN PRINT p$(D, P1%(D)); " has been ejected": X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): GOTO 2850
	IF i = 2 THEN PRINT Q$(0); " has been ejected": X$ = Q$(0): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850
	PRINT Q$(0); " and "; p$(D, P1%(D)); " have been ejected": X$ = Q$(0): CALL EJECTINC (EJ%,X$): X$ = p$(D, P1%(D)): CALL EJECTINC (EJ%,X$): CALL DELAY: CALL INJPIT (P,D): CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850

2006 IF H6% > 21 THEN 2010

2008 H6%=21:CALL NEWLINES: IF RD = 0 AND (PC% = 0 OR PC% = 4) THEN PRINT Q$(0); " fouls the pitch out of play": CALL DELAY: GOTO 2850
	tm = INT(RND(1) * 59) + 2: COLOR 0, 7: PRINT "The game has been halted by rain": FOR i = 1 TO 3: CALL DELAY: NEXT
	PRINT "Rain delay lasted"; tm; " minutes": CALL DELAY: CALL DELAY
	RD% = 1
	IF tm > 28 THEN PRINT "Pitchers have lost effectiveness during delay": CALL DELAY: PRINT "Their arms have stiffened up": CALL DELAY: CALL DELAY: FOR i = 0 TO 1: s8%(i, 0) = s8%(i, 0) - 2: NEXT
	GOTO 2850

2010 IF H6% > 24 THEN 2015
	IF s4% = 0 AND L%(1, 13) = 14 THEN PRINT Q$(0); " steps out of the box as a jet passes over": CALL DELAY: GOTO 2850
	IF s4% = 0 THEN PRINT U$(1); " calls time to get some new baseballs": CALL DELAY: GOTO 2850
	PRINT b$(D, B9%(D, 2)); " rifles the ball to ";
	IF s4% = 1 THEN PRINT "1st...": CALL DELAY: i = 2: GOTO 2012
	IF s4% = 2 OR s4% = 4 THEN PRINT "2nd base...": CALL DELAY: i = 3: GOTO 2012
	PRINT "3rd base...": CALL DELAY: i = 4

2012 PRINT Q$(i - 1); " is picked off base!"
	a5%(i - 1) = 10: O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: W% = 2
	CALL CREDITASSIST (W%,D): CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL CREDITPUTOUT (I3,FB2P%,AST%,D)
	I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
	IF O%(0) >= 3 THEN B1!(p)=B1!(p)-1:H6%=5:GOTO 2850

2015 IF H6% > 29 THEN 2020
	PRINT Q$(0); " pops it up near the 3rd base stands": CALL DELAY
	PRINT "A fan interferes...": CALL DELAY
	PRINT b$(D, B9%(D, 6)); " makes a great catch!!": W% = 6: CALL CREDITREGPO (W%,D)
	O%(0) = O%(0) + 1: O%(1) = O%(1) + 1: CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6)
	I5 = P1%(D): I6 = 0: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6)
	CALL DELAY: GOTO 2850

2020 IF H6% <= 31 THEN PRINT Q$(0); " pops it up foul near the stands": CALL DELAY
	IF H6% <= 30 THEN PRINT b$(D, B9%(D, 2)); " runs into the stands!!": po% = 2: GOTO 2099
	IF H6% <= 31 THEN PRINT b$(D, B9%(D, 3)); " gives chase and runs into the stands!!": po% = 3: GOTO 2099
	IF H6% = 32 THEN PRINT Q$(0); " pops it up foul down the rf line": CALL DELAY: PRINT b$(D, B9%(D, 4)); " crashes into the wall going for it": po% = 4: GOTO 2099
	IF H6% = 33 THEN PRINT Q$(0); " pops it up foul by"
	IF H6% = 33 THEN PRINT "the 3rd base dugout": CALL DELAY
	IF H6% = 33 THEN PRINT b$(D, B9%(D, 5)); " falls in trying to make the catch!!": CALL DELAY: po% = 5: GOTO 2099
	IF H6% = 34 THEN PRINT Q$(0); " pops it up foul"
	IF H6% = 34 THEN PRINT "down the lf line": CALL DELAY
	IF H6% = 34 THEN PRINT b$(D, B9%(D, 6)); " tumbles over the railing"
	IF H6% = 34 THEN PRINT "trying to make the catch!": CALL DELAY: CALL DELAY: po% = 6: GOTO 2099
	IF H6% > 37 THEN 2023
	PRINT Q$(0); " lines one to deep left field": CALL DELAY: PRINT b$(D, B9%(D, 7)); " races back...": CALL DELAY: PRINT "He slams into the wall and is down!!": CALL DELAY
	op% = 7: PRINT b$(D, B9%(D, 8)); " comes over and gets the ball in": CALL DELAY

2021 I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES: po% = op%: GOTO 2098

2023 IF H6% > 40 THEN 2025
	PRINT Q$(0); " drives a sinking liner"
	PRINT "into deep center": CALL DELAY
	PRINT b$(D, B9%(D, 8)); " gets a good jump on it": CALL DELAY
	PRINT "He crashes into the wall and"
	PRINT "the ball bounces away!!": CALL DELAY
	PRINT "The runners are circling the bases": CALL DELAY
	PRINT b$(D, B9%(D, 9)); " retrieves the ball": CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): I6 = 14: CALL ADDPIT (D,I5,I6): W% = 8
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 6: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 29: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 4: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "It's an inside the park HOME RUN!!!": CALL DELAY: CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 8: GOTO 2098

2025 IF H6% > 43 THEN 2027
	PRINT Q$(0); " lifts a deep drive into right": CALL DELAY: PRINT b$(D, B9%(D, 9)); " races back...": CALL DELAY: PRINT "He slams into the wall and is down!!": CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 9: PRINT b$(D, B9%(D, 8)); " comes over to get the ball back in": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 5: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 28: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 3: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1: CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "Triple!!": CALL DELAY: CALL DELAY: CALL SCOREBOARD: CALL NEWLINES: po% = 9: GOTO 2098

2027 IF H6% > 44 THEN 2030
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = INT(RND(1) * 3) + 7: PRINT Q$(0); " hits a sinking liner into "; c$(W%): CALL DELAY
	PRINT b$(D, B9%(D, W%)); " dives for the catch...": CALL DELAY
	PRINT "the ball rolls free!": CALL DELAY
	PRINT b$(D, B9%(D, W% - 1)); " gets the loose ball in": CALL DELAY
	op% = W%: GOTO 2021

2030 IF H6% = 45 THEN PRINT Q$(0); " is hit by a pitch and injured": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): GOTO 2850
	IF H6% <= 49 THEN PRINT b$(D, B9%(D, 2)); " is injured by a foul tip!": CALL DELAY: po% = 2: GOTO 2099
	IF H6% > 58 THEN 2037
	i = INT(RND(1) * 3) + 7
	IF i = 9 THEN hi1 = 3: hi2 = 9: hi3 = 8
	IF i = 8 THEN hi1 = 4: hi2 = 8: hi3 = 7
	IF i = 7 THEN hi1 = 6: hi2 = 7: hi3 = 8
	PRINT Q$(0); " hits a weak fly into short "; c$(i): CALL DELAY
	PRINT b$(D, B9%(D, hi1)); " goes out": CALL DELAY
	PRINT b$(D, B9%(D, hi2)); " is charging in": CALL DELAY
	PRINT "They collide and the ball rolls free!!": CALL DELAY
	PRINT b$(D, B9%(D, hi3)); " gets it back in"
	CALL DELAY
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 2: NEXT i: a5%(3) = 1: a5%(1) = 3: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD
	CALL NEWLINES: po% = hi1: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): po% = hi2: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2037 IF H6% <= 61 THEN PRINT Q$(0); " fouls the pitch off his foot": CALL DELAY: PRINT "He is injured": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
	IF H6% > 64 THEN 2040
	PRINT Q$(0); " pops it up in foul along the 3b line": CALL DELAY: PRINT "It drifts into foul territory": CALL DELAY
	PRINT b$(D, B9%(D, 5)); " and "; p$(D, P1%(D)); " chase after it and collide!!": CALL DELAY
	PRINT b$(D, B9%(D, 5)); " is down and injured": CALL DELAY: po% = 5: GOTO 2099

2040 IF H6% > 70 THEN 2045
	PRINT Q$(0); : I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6): W% = 8: CALL LONGFLY (D,W%)
	PRINT b$(D, B9%(D, 7)); " collides with "; b$(D, B9%(D, 8)); "!!": CALL DELAY
	PRINT "both fielders are down... ": CALL DELAY
	PRINT b$(D, B9%(D, 6)); " gets the ball back in": CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6): I6 = 4: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6): I6 = 27: CALL INCPITOFF (P,I5,I6)
	a5%(0) = 2: a5%(1) = 3: a5%(2) = 2: a5%(3) = 1
	CALL ADVANCEMENT (P,D,S2%,F%,I3): PRINT "Double!!": CALL SCOREBOARD
	CALL NEWLINES: po% = 7: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6)
	po% = 8: CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5
	GOTO 2850

2045 IF H6% > 85 THEN 2055
	PRINT Q$(0); " lines it off ";
	i = INT(RND(1) * 5) + 1: ON i GOTO 2046, 2047, 2047, 2047, 2047

2046 PRINT "the leg of "; p$(D, P1%(D)): CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
	PRINT "He reaches on a single": CALL DELAY: PRINT p$(D, P1%(D)); " is down and in pain!": CALL DELAY
	CALL INJPIT (P,D): H6% = 5: GOTO 2850

2047 po% = i + 1: PRINT "of "; b$(D, B9%(D, po%)); " for a single": CALL DELAY: CALL DELAY
	IF B7%(p, B1!(p)) <> 1 THEN I5 = B3%(p, B1!(p)): I6 = 0: CALL INCBATOFF (P,I5,I6): I6 = 1: CALL INCBATOFF (P,I5,I6)
	IF B7%(p, B1!(p)) = 1 THEN I5 = P1%(p): I6 = 23: CALL INCPITOFF (P,I5,I6): I6 = 24: CALL INCPITOFF (P,I5,I6)
	I5 = P1%(D): I6 = 1: CALL ADDPIT (D,I5,I6)
	FOR i = 0 TO 3: a5%(i) = 1: NEXT: CALL ADVANCEMENT (P,D,S2%,F%,I3): CALL SCOREBOARD: CALL NEWLINES
	PRINT b$(D, B9%(D, po%)); " is down and injured": CALL DELAY
	CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2055 IF H6% <= 89 THEN PRINT p$(D, P1%(D)); " is motioning for "; m$(D); " to come out": CALL DELAY: PRINT "It appears he has a blister": CALL DELAY: PRINT "He will have to leave the game": CALL DELAY: CALL INJPIT (P,D): H6% = 1: GOTO 2850
	IF H6% = 90 THEN PRINT Q$(0); " takes a mighty swing!": CALL DELAY: PRINT "It looks like he injured himself": CALL DELAY: CALL PLYRHRT (P,P9,W5,W6,BC%,U%,D,MO): H6% = 1: GOTO 2850
	IF H6% = 91 THEN PRINT "Time is called": CALL DELAY: PRINT "Morganna strikes again!!": CALL DELAY: H6% = 1: GOTO 2850
	PRINT U$(1); " calls time to dust off the plate": CALL DELAY: H6% = 1: GOTO 2850

2098 CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 5: GOTO 2850

2099 CALL INJURYLENGTH (IJL%,D,PO%,U6,P,U9,P9,W5,W6): H6% = 1: GOTO 2850

2850 '
END SUB

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
