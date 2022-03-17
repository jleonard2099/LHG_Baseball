'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE FUNCTION FnExt$ (X$, Length)
DECLARE SUB BATTEREDIT ()
DECLARE SUB PITCHEREDIT ()
DECLARE SUB TITLE ()
DECLARE SUB PITCHINIT (i, Colr)
DECLARE SUB PITCHINIT2 (i, Colr)

DIM D$(3), ITEM$(15)
DIM K9(1) AS LONG
DIM Q$(2100), FS%(22, 4, 4)
DIM b%(22, 79), p%(21, 88), b$(22), p$(21), L%(13)
DIM BS$(22), PS$(21)
DIM B0%(22, 40), P0%(21, 41), BS%(22), PS%(32), T(22), T1(22)
DIM VV$(200), V%(200)
DIM TB%(11), TP%(33),INJ%(22),DYS%(21)
DIM SA%(24), TS%(11), BRS$(79)
DIM Y AS LONG

COMMON SHARED b%(), p%(), b$(), p$(), L%(), BRS$(), K9() AS LONG, SA%(), TS%()
COMMON SHARED YN$, T$, NN$, S$, M$, T, N$, INJ%(), DYS%()

SUB BATTEREDIT ()

200 COLOR 15, 0
	CLS
	PRINT "EDIT/ENTER BATTERS"
	X% = 2
	FOR i = 0 TO 10
	LOCATE i + 5, 5
	PRINT USING "##"; i + 1; : PRINT " "; b$(i); TAB(40);
	PRINT USING "##"; i + 13; : PRINT " "; b$(i + 12)
	NEXT
	LOCATE , 5: PRINT USING "##"; 12; : PRINT " "; b$(11)
	PRINT
	'FOR i = 0 TO 22
	'FOR i1 = 22 TO 25
	'IF (b%(i, i1) > 0 AND b%(i, i1 + 4) = 0) OR b%(i, 4) = 0 THEN PRINT b$(i)
	'NEXT
	'NEXT

855 LOCATE 22, 1
	PRINT "# TO ENTER/EDIT (1-23) (0 TO CONTINUE) "; : INPUT i
	IF i < 0 OR i > 23 THEN 855
	i = i - 1
	IF i < 0 THEN 300
' INITIALIZE BATTER SCREEN

COLOR 15, 1: CLS : LOCATE 1, 30: PRINT "EDIT AND INPUT BATTERS"
	LOCATE 3, 1: PRINT "NAME:"
	LOCATE 3, 24: PRINT "BATS:"
	LOCATE 3, 36: PRINT "ACTIVE:"
	LOCATE 3, 50: PRINT "INJ LENGTH:"
	LOCATE 5, 14: PRINT " G  AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
	LOCATE 6, 1: PRINT "SEASON:"
	LOCATE 7, 1: PRINT "VS LHP:"
	LOCATE 8, 1: PRINT "VS RHP:"
	LOCATE 10, 33: PRINT "FIELDING RATINGS"
	LOCATE 11, 31: PRINT "POS  FAVG  ARM RANGE"
	LOCATE 12, 19: PRINT "POSITION 1"
	LOCATE 13, 19: PRINT "POSITION 2"
	LOCATE 14, 19: PRINT "POSITION 3"
	LOCATE 15, 19: PRINT "POSITION 4"
	LOCATE 17, 12: PRINT "RUN (1-9):"
	LOCATE 17, 27: PRINT "BUNT (1-9):"
	LOCATE 17, 44: PRINT "G/A:"
	LOCATE 17, 54: PRINT "LEAGUE BAVG:"
	LOCATE 20, 29: PRINT "SPLIT STATS LEAGUE DATA"
	LOCATE 21, 26: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
	LOCATE 22, 13: PRINT "LHB vs LHP"
	LOCATE 23, 13: PRINT "LHB vs RHP"
	LOCATE 24, 13: PRINT "RHB vs LHP";
	LOCATE 25, 13: PRINT "RHB vs RHP";

' INITIALIZE BATTER FIELDS
	Colr = 112
	COLOR 0,7
	LOCATE 3, 7: QPrint FnExt$(b$(i), 15), FG, BG, -1
	IF b%(i, 0) = 2 THEN BATS$ = "RP"
	IF b%(i, 0) = -2 THEN BATS$ = "LP"
	IF b%(i, 0) = 1 THEN BATS$ = "R"
	IF b%(i, 0) = -1 THEN BATS$ = "L"
	IF b%(i, 0) = 0 THEN BATS$ = "S"
	LOCATE 3, 30: QPrint FnExt$(BATS$, 2), FG, BG, -1
	ACT$ = "Y"
	IF b%(i, 31) = 1 THEN ACT$ = "N"
	LOCATE 3, 44: PRINT ACT$
	LOCATE 3, 62: PRINT USING "###";INJ%(i)
	LOCATE 6, 12: PRINT USING "###";b%(i, 3)
	LOCATE 6, 17: PRINT USING "###";b%(i, 4)
	LOCATE 6, 22: PRINT USING "###";b%(i, 5)
	LOCATE 6, 27: PRINT USING "###";b%(i, 6)
	LOCATE 6, 32: PRINT USING "##"; b%(i, 7)
	LOCATE 6, 36: PRINT USING "##"; b%(i, 8)
	LOCATE 6, 40: PRINT USING "##"; b%(i, 9)
	LOCATE 6, 44: PRINT USING "###";b%(i, 10)
	LOCATE 6, 49: PRINT USING "###";b%(i, 11)
	LOCATE 6, 54: PRINT USING "###";b%(i, 12)
	LOCATE 6, 59: PRINT USING "###";b%(i, 13)
	LOCATE 6, 64: PRINT USING "##"; b%(i, 14)
	LOCATE 6, 68: PRINT USING "##"; b%(i, 15)
	LOCATE 7, 17: PRINT USING "###";b%(i, 42)
	LOCATE 7, 27: PRINT USING "###";b%(i, 43)
	LOCATE 7, 32: PRINT USING "##";b%(i, 44)
	LOCATE 7, 36: PRINT USING "##";b%(i, 45)
	LOCATE 7, 40: PRINT USING "##";b%(i, 46)
	LOCATE 7, 49: PRINT USING "###";b%(i, 47)
	LOCATE 7, 54: PRINT USING "###";b%(i, 48)
	LOCATE 8, 17: PRINT USING "###";b%(i, 49)
	LOCATE 8, 27: PRINT USING "###";b%(i, 50)
	LOCATE 8, 32: PRINT USING "##";b%(i, 51)
	LOCATE 8, 36: PRINT USING "##";b%(i, 52)
	LOCATE 8, 40: PRINT USING "##";b%(i, 53)
	LOCATE 8, 49: PRINT USING "###";b%(i, 54)
	LOCATE 8, 54: PRINT USING "###";b%(i, 55)
	LOCATE 12, 31: PRINT USING "##";b%(i, 22)
	LOCATE 12, 36: PRINT USING "####";b%(i, 26)
	LOCATE 12, 43: PRINT USING "#";b%(i, 33)
	LOCATE 12, 47: PRINT USING "#";b%(i, 34)
	LOCATE 13, 31: PRINT USING "##";b%(i, 23)
	LOCATE 13, 36: PRINT USING "####";b%(i, 27)
	LOCATE 13, 43: PRINT USING "#";b%(i, 35)
	LOCATE 13, 47: PRINT USING "#";b%(i, 36)
	LOCATE 14, 31: PRINT USING "##";b%(i, 24)
	LOCATE 14, 36: PRINT USING "####";b%(i, 28)
	LOCATE 14, 43: PRINT USING "#";b%(i, 37)
	LOCATE 14, 47: PRINT USING "#";b%(i, 38)
	LOCATE 15, 31: PRINT USING "##";b%(i, 25)
	LOCATE 15, 36: PRINT USING "####";b%(i, 29)
	LOCATE 15, 43: PRINT USING "#";b%(i, 39)
	LOCATE 15, 47: PRINT USING "#";b%(i, 40)
	LOCATE 17, 23: PRINT USING "#";b%(i, 16)
	LOCATE 17, 39: PRINT USING "#";b%(i, 30)
	LOCATE 17, 49: PRINT USING "##";b%(i, 18)
	LOCATE 17, 67: PRINT USING "###";b%(i, 20)
	LOCATE 22, 27: PRINT USING "###";b%(i, 56)
	LOCATE 22, 32: PRINT USING "###";b%(i, 57)
	LOCATE 22, 37: PRINT USING "###";b%(i, 58)
	LOCATE 22, 42: PRINT USING "###";b%(i, 59)
	LOCATE 22, 47: PRINT USING "###";b%(i, 60)
	LOCATE 22, 52: PRINT USING "###";b%(i, 61)
	LOCATE 23, 27: PRINT USING "###";b%(i, 62)
	LOCATE 23, 32: PRINT USING "###";b%(i, 63)
	LOCATE 23, 37: PRINT USING "###";b%(i, 64)
	LOCATE 23, 42: PRINT USING "###";b%(i, 65)
	LOCATE 23, 47: PRINT USING "###";b%(i, 66)
	LOCATE 23, 52: PRINT USING "###";b%(i, 67)
	LOCATE 24, 27: PRINT USING "###";b%(i, 68);
	LOCATE 24, 32: PRINT USING "###";b%(i, 69);
	LOCATE 24, 37: PRINT USING "###";b%(i, 70);
	LOCATE 24, 42: PRINT USING "###";b%(i, 71);
	LOCATE 24, 47: PRINT USING "###";b%(i, 72);
	LOCATE 24, 52: PRINT USING "###";b%(i, 73);
	LOCATE 25, 27: PRINT USING "###";b%(i, 74);
	LOCATE 25, 32: PRINT USING "###";b%(i, 75);
	LOCATE 25, 37: PRINT USING "###";b%(i, 76);
	LOCATE 25, 42: PRINT USING "###";b%(i, 77);
	LOCATE 25, 47: PRINT USING "###";b%(i, 78);
	LOCATE 25, 52: PRINT USING "###";b%(i, 79);

BName:
	LOCATE 3, 7
	Max = 15
	CALL TextIn(b$(i), Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO BName:
	IF XCode = 2 GOTO bDone

BATS:
	LOCATE 3, 30
	Max = 2
	CALL TextIn(BATS$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO BName:
	IF XCode = 2 GOTO bDone

BActive:
	LOCATE 3, 44
	Max = 1
	CALL TextIn(ACT$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO BATS:
	IF XCode = 2 GOTO bDone

BINJ:
	LOCATE 3, 62
	Max = 3
	CALL NumIn(INJ%(i), Max, XCode, Colr)
	locate 3,62:print using "###";inj%(i)
	IF XCode = 1 GOTO BActive:
	IF XCode = 2 GOTO bDone

BGAMES:
	LOCATE 6, 12
	Max = 3
	CALL NumIn(b%(i, 3), Max, XCode, Colr)
	LOCATE 6, 12:PRINT USING "###";B%(I,3)
	IF XCode = 1 GOTO BINJ:
	IF XCode = 2 GOTO bDone

BAB:
	LOCATE 6, 17
	Max = 3
	CALL NumIn(b%(i, 4), Max, XCode, Colr)
	LOCATE 6, 17:PRINT USING "###";B%(I,4)
	IF XCode = 1 GOTO BGAMES:
	IF XCode = 2 GOTO bDone

BR:
	LOCATE 6, 22
	Max = 3
	CALL NumIn(b%(i, 5), Max, XCode, Colr)
	LOCATE 6, 22:PRINT USING "###";B%(I,5)
	IF XCode = 1 GOTO BAB:
	IF XCode = 2 GOTO bDone

BH:
	LOCATE 6, 27
	Max = 3
	CALL NumIn(b%(i, 6), Max, XCode, Colr)
	LOCATE 6, 27:PRINT USING "###";B%(I,6)
	IF XCode = 1 GOTO BR:
	IF XCode = 2 GOTO bDone

B2B:
	LOCATE 6, 32
	Max = 2
	CALL NumIn(b%(i, 7), Max, XCode, Colr)
	LOCATE 6, 32:PRINT USING "##";B%(I,7)
	IF XCode = 1 GOTO BH:
	IF XCode = 2 GOTO bDone

B3B:
	LOCATE 6, 36
	Max = 2
	CALL NumIn(b%(i, 8), Max, XCode, Colr)
	LOCATE 6, 36:PRINT USING "##";B%(I,8)
	IF XCode = 1 GOTO B2B:
	IF XCode = 2 GOTO bDone

bHR:
	LOCATE 6, 40
	Max = 2
	CALL NumIn(b%(i, 9), Max, XCode, Colr)
	LOCATE 6, 40:PRINT USING "##";B%(I,9)
	IF XCode = 1 GOTO B3B:
	IF XCode = 2 GOTO bDone

BRBI:
	LOCATE 6, 44
	Max = 3
	CALL NumIn(b%(i, 10), Max, XCode, Colr)
	LOCATE 6, 44:PRINT USING "###";B%(I,10)
	IF XCode = 1 GOTO bHR:
	IF XCode = 2 GOTO bDone

bBB:
	LOCATE 6, 49
	Max = 3
	CALL NumIn(b%(i, 11), Max, XCode, Colr)
	LOCATE 6, 49:PRINT USING "###";B%(I,11)
	IF XCode = 1 GOTO BRBI:
	IF XCode = 2 GOTO bDone

bKO:
	LOCATE 6, 54
	Max = 3
	CALL NumIn(b%(i, 12), Max, XCode, Colr)
	LOCATE 6, 54:PRINT USING "###";B%(I,12)
	IF XCode = 1 GOTO bBB:
	IF XCode = 2 GOTO bDone

bSB:
	LOCATE 6, 59
	Max = 3
	CALL NumIn(b%(i, 13), Max, XCode, Colr)
	LOCATE 6, 59:PRINT USING "###";B%(I,13)
	IF XCode = 1 GOTO bKO:
	IF XCode = 2 GOTO bDone

bCS:
	LOCATE 6, 64
	Max = 2
	CALL NumIn(b%(i, 14), Max, XCode, Colr)
	LOCATE 6, 64:PRINT USING "##";B%(I,14)
	IF XCode = 1 GOTO bSB:
	IF XCode = 2 GOTO bDone

BSH:
	LOCATE 6, 68
	Max = 2
	CALL NumIn(b%(i, 15), Max, XCode, Colr)
	LOCATE 6, 68:PRINT USING "##";B%(I,15)
	IF XCode = 1 GOTO bCS:
	IF XCode = 2 GOTO bDone

BLAB:
	LOCATE 7, 17
	Max = 3
	CALL NumIn(b%(i, 42), Max, XCode, Colr)
	LOCATE 7, 17:PRINT USING "###";B%(I,42)
	IF XCode = 1 GOTO BSH:
	IF XCode = 2 GOTO bDone

BLH:
	LOCATE 7, 27
	Max = 3
	CALL NumIn(b%(i, 43), Max, XCode, Colr)
	LOCATE 7, 27:PRINT USING "###";B%(I,43)
	IF XCode = 1 GOTO BLAB:
	IF XCode = 2 GOTO bDone

BL2B:
	LOCATE 7, 32
	Max = 2
	CALL NumIn(b%(i, 44), Max, XCode, Colr)
	LOCATE 7, 32:PRINT USING "##";B%(I,44)
	IF XCode = 1 GOTO BLH:
	IF XCode = 2 GOTO bDone

BL3B:
	LOCATE 7, 36
	Max = 2
	CALL NumIn(b%(i, 45), Max, XCode, Colr)
	LOCATE 7, 36:PRINT USING "##";B%(I,45)
	IF XCode = 1 GOTO BL2B:
	IF XCode = 2 GOTO bDone

BLHR:
	LOCATE 7, 40
	Max = 2
	CALL NumIn(b%(i, 46), Max, XCode, Colr)
	LOCATE 7, 40:PRINT USING "##";B%(I,46)
	IF XCode = 1 GOTO BL3B:
	IF XCode = 2 GOTO bDone

bLBB:
	LOCATE 7, 49
	Max = 3
	CALL NumIn(b%(i, 47), Max, XCode, Colr)
	LOCATE 7, 49:PRINT USING "###";B%(I,47)
	IF XCode = 1 GOTO BLHR:
	IF XCode = 2 GOTO bDone

BLKO:
	LOCATE 7, 54
	Max = 3
	CALL NumIn(b%(i, 48), Max, XCode, Colr)
	LOCATE 7, 54:PRINT USING "###";B%(I,48)
	IF XCode = 1 GOTO bLBB:
	IF XCode = 2 GOTO bDone

BRAB:
	LOCATE 8, 17
	Max = 3
	CALL NumIn(b%(i, 49), Max, XCode, Colr)
	LOCATE 8, 17:PRINT USING "###";B%(I,49)
	IF XCode = 1 GOTO BLKO:
	IF XCode = 2 GOTO bDone

BRH:
	LOCATE 8, 27
	Max = 3
	CALL NumIn(b%(i, 50), Max, XCode, Colr)
	LOCATE 8, 27:PRINT USING "###";B%(I,50)
	IF XCode = 1 GOTO BRAB:
	IF XCode = 2 GOTO bDone

BR2B:
	LOCATE 8, 32
	Max = 2
	CALL NumIn(b%(i, 51), Max, XCode, Colr)
	LOCATE 8, 32:PRINT USING "##";B%(I,51)
	IF XCode = 1 GOTO BRH:
	IF XCode = 2 GOTO bDone

BR3B:
	LOCATE 8, 36
	Max = 2
	CALL NumIn(b%(i, 52), Max, XCode, Colr)
	LOCATE 8, 36:PRINT USING "##";B%(I,52)
	IF XCode = 1 GOTO BR2B:
	IF XCode = 2 GOTO bDone

BRHR:
	LOCATE 8, 40
	Max = 2
	CALL NumIn(b%(i, 53), Max, XCode, Colr)
	LOCATE 8, 40:PRINT USING "##";B%(I,53)
	IF XCode = 1 GOTO BR3B:
	IF XCode = 2 GOTO bDone

bRBB:
	LOCATE 8, 49
	Max = 3
	CALL NumIn(b%(i, 54), Max, XCode, Colr)
	LOCATE 8, 49:PRINT USING "###";B%(I,54)
	IF XCode = 1 GOTO BRHR:
	IF XCode = 2 GOTO bDone

BRKO:
	LOCATE 8, 54
	Max = 3
	CALL NumIn(b%(i, 55), Max, XCode, Colr)
	LOCATE 8, 54:PRINT USING "###";B%(I,55)
	IF XCode = 1 GOTO bRBB:
	IF XCode = 2 GOTO bDone

BP1:
	LOCATE 12, 31
	Max = 2
	CALL NumIn(b%(i, 22), Max, XCode, Colr)
	LOCATE 12, 31:PRINT USING "##";B%(I,22)
	IF XCode = 1 GOTO BRKO:
	IF XCode = 2 GOTO bDone

' STOP HERE

BF1:
	LOCATE 12, 36
	Max = 4
	CALL NumIn(b%(i, 26), Max, XCode, Colr)
	locate 12,36:print using "####";b%(i,26)
	IF XCode = 1 GOTO BP1:
	IF XCode = 2 GOTO bDone

bA1:
	LOCATE 12, 43
	Max = 1
	CALL NumIn(b%(i, 33), Max, XCode, Colr)
	locate 12,43:print using "#";b%(i, 33)
	IF XCode = 1 GOTO BF1:
	IF XCode = 2 GOTO bDone

BR1:
	LOCATE 12, 47
	Max = 1
	CALL NumIn(b%(i, 34), Max, XCode, Colr)
	locate 12,47:print using "#";b%(i,34)
	IF XCode = 1 GOTO bA1:
	IF XCode = 2 GOTO bDone

BP2:
	LOCATE 13, 31
	Max = 2
	CALL NumIn(b%(i, 23), Max, XCode, Colr)
	locate 13,31:print using "##";b%(i,23)
	IF XCode = 1 GOTO BR1:
	IF XCode = 2 GOTO bDone

BF2:
	LOCATE 13, 36
	Max = 4
	CALL NumIn(b%(i, 27), Max, XCode, Colr)
	locate 13,36:print using "####";b%(i,27)
	IF XCode = 1 GOTO BP2:
	IF XCode = 2 GOTO bDone

bA2:
	LOCATE 13, 43
	Max = 1
	CALL NumIn(b%(i, 35), Max, XCode, Colr)
	locate 13,43:print using "#";b%(i,35)
	IF XCode = 1 GOTO BF2:
	IF XCode = 2 GOTO bDone

BR2:
	LOCATE 13, 47
	Max = 1
	CALL NumIn(b%(i, 36), Max, XCode, Colr)
	locate 13,47:print using "#";b%(i,36)
	IF XCode = 1 GOTO bA2:
	IF XCode = 2 GOTO bDone

BP3:
	LOCATE 14, 31
	Max = 2
	CALL NumIn(b%(i, 24), Max, XCode, Colr)
	locate 14,31:print using "##";b%(i,24)
	IF XCode = 1 GOTO BR2:
	IF XCode = 2 GOTO bDone

BF3:
	LOCATE 14, 36
	Max = 4
	CALL NumIn(b%(i, 28), Max, XCode, Colr)
	locate 14,36:print using "####";b%(i,28)
	IF XCode = 1 GOTO BP3:
	IF XCode = 2 GOTO bDone

bA3:
	LOCATE 14, 43
	Max = 1
	CALL NumIn(b%(i, 37), Max, XCode, Colr)
	locate 14,43:print using "#";b%(i,37)
	IF XCode = 1 GOTO BF3:
	IF XCode = 2 GOTO bDone

BR3:
	LOCATE 14, 47
	Max = 1
	CALL NumIn(b%(i, 38), Max, XCode, Colr)
	locate 14,47:print using "#";b%(i,38)
	IF XCode = 1 GOTO bA3:
	IF XCode = 2 GOTO bDone

BP4:
	LOCATE 15, 31
	Max = 2
	CALL NumIn(b%(i, 25), Max, XCode, Colr)
	locate 15,31:print using "##";b%(i,25)
	IF XCode = 1 GOTO BR3:
	IF XCode = 2 GOTO bDone

BF4:
	LOCATE 15, 36
	Max = 4
	CALL NumIn(b%(i, 29), Max, XCode, Colr)
	locate 15,36:print using "####";b%(i,29)
	IF XCode = 1 GOTO BP4:
	IF XCode = 2 GOTO bDone

bA4:
	LOCATE 15, 43
	Max = 1
	CALL NumIn(b%(i, 39), Max, XCode, Colr)
	locate 15,43:print using "#";b%(i,39)
	IF XCode = 1 GOTO BF4:
	IF XCode = 2 GOTO bDone

bR4:
	LOCATE 15, 47
	Max = 1
	CALL NumIn(b%(i, 40), Max, XCode, Colr)
	locate 15,47:print using "#";b%(i,40)
	IF XCode = 1 GOTO bA4:
	IF XCode = 2 GOTO bDone

bRUN:
	LOCATE 17, 23
	Max = 1
	CALL NumIn(b%(i, 16), Max, XCode, Colr)
	locate 17,23:print using "#";b%(i,16)
	IF XCode = 1 GOTO bR4:
	IF XCode = 2 GOTO bDone

bBUNT:
	LOCATE 17, 39
	Max = 1
	CALL NumIn(b%(i, 30), Max, XCode, Colr)
	locate 17,39:print using "#";b%(i,30)
	IF XCode = 1 GOTO bRUN:
	IF XCode = 2 GOTO bDone

bGA:
	LOCATE 17, 49
	Max = 2
	CALL NumIn(b%(i, 18), Max, XCode, Colr)
	locate 17,49:print using "##";b%(i,18)
	IF XCode = 1 GOTO bBUNT:
	IF XCode = 2 GOTO bDone

bLA:
	LOCATE 17, 67
	Max = 3
	CALL NumIn(b%(i, 20), Max, XCode, Colr)
	locate 17,67:print using "###";b%(i,20)
	IF XCode = 1 GOTO bGA:
	IF XCode = 2 GOTO bDone

bLLBA:
	LOCATE 22, 27
	Max = 3
	CALL NumIn(b%(i, 56), Max, XCode, Colr)
	locate 22,27:print using "###";b%(i,56)
	IF XCode = 1 GOTO bLA:
	IF XCode = 2 GOTO bDone

bLL2B:
	LOCATE 22, 32
	Max = 3
	CALL NumIn(b%(i, 57), Max, XCode, Colr)
	locate 22,32:print using "###";b%(i,57)
	IF XCode = 1 GOTO bLLBA:
	IF XCode = 2 GOTO bDone

bLL3B:
	LOCATE 22, 37
	Max = 3
	CALL NumIn(b%(i, 58), Max, XCode, Colr)
	locate 22,37:print using "###";b%(i,58)
	IF XCode = 1 GOTO bLL2B:
	IF XCode = 2 GOTO bDone

bLLHR:
	LOCATE 22, 42
	Max = 3
	CALL NumIn(b%(i, 59), Max, XCode, Colr)
	locate 22,42:print using "###";b%(i,59)
	IF XCode = 1 GOTO bLL3B:
	IF XCode = 2 GOTO bDone

bLLBB:
	LOCATE 22, 47
	Max = 3
	CALL NumIn(b%(i, 60), Max, XCode, Colr)
	locate 22,47:print using "###";b%(i,60)
	IF XCode = 1 GOTO bLLHR:
	IF XCode = 2 GOTO bDone

bLLKO:
	LOCATE 22, 52
	Max = 3
	CALL NumIn(b%(i, 61), Max, XCode, Colr)
	locate 22,52:print using "###";b%(i,61)
	IF XCode = 1 GOTO bLLBB:
	IF XCode = 2 GOTO bDone

bLRBA:
	LOCATE 23, 27
	Max = 3
	CALL NumIn(b%(i, 62), Max, XCode, Colr)
	locate 23,27:print using "###";b%(i,62)
	IF XCode = 1 GOTO bLLKO:
	IF XCode = 2 GOTO bDone

bLR2B:
	LOCATE 23, 32
	Max = 3
	CALL NumIn(b%(i, 63), Max, XCode, Colr)
	locate 23,32:print using "###";b%(i,63)
	IF XCode = 1 GOTO bLRBA:
	IF XCode = 2 GOTO bDone

bLR3B:
	LOCATE 23, 37
	Max = 3
	CALL NumIn(b%(i, 64), Max, XCode, Colr)
	locate 23,37:print using "###";b%(i,64)
	IF XCode = 1 GOTO bLR2B:
	IF XCode = 2 GOTO bDone

bLRHR:
	LOCATE 23, 42
	Max = 3
	CALL NumIn(b%(i, 65), Max, XCode, Colr)
	locate 23,42:print using "###";b%(i,65)
	IF XCode = 1 GOTO bLR3B:
	IF XCode = 2 GOTO bDone

bLRBB:
	LOCATE 23, 47
	Max = 3
	CALL NumIn(b%(i, 66), Max, XCode, Colr)
	locate 23,47:print using "###";b%(i,66)
	IF XCode = 1 GOTO bLRHR:
	IF XCode = 2 GOTO bDone

bLRKO:
	LOCATE 23, 52
	Max = 3
	CALL NumIn(b%(i, 67), Max, XCode, Colr)
	locate 23,52:print using "###";b%(i,67)
	IF XCode = 1 GOTO bLRBB:
	IF XCode = 2 GOTO bDone

bRLBA:
	LOCATE 24, 27
	Max = 3
	CALL NumIn(b%(i, 68), Max, XCode, Colr)
	locate 24,27:print using "###";b%(i,68);
	IF XCode = 1 GOTO bLRKO:
	IF XCode = 2 GOTO bDone

bRL2B:
	LOCATE 24, 32
	Max = 3
	CALL NumIn(b%(i, 69), Max, XCode, Colr)
	locate 24,32:print using "###";b%(i,69);
	IF XCode = 1 GOTO bRLBA:
	IF XCode = 2 GOTO bDone

bRL3B:
	LOCATE 24, 37
	Max = 3
	CALL NumIn(b%(i, 70), Max, XCode, Colr)
	locate 24,37:print using "###";b%(i,70);
	IF XCode = 1 GOTO bRL2B:
	IF XCode = 2 GOTO bDone

bRLHR:
	LOCATE 24, 42
	Max = 3
	CALL NumIn(b%(i, 71), Max, XCode, Colr)
	locate 24,42:print using "###";b%(i,71);
	IF XCode = 1 GOTO bRL3B:
	IF XCode = 2 GOTO bDone

bRLBB:
	LOCATE 24, 47
	Max = 3
	CALL NumIn(b%(i, 72), Max, XCode, Colr)
	locate 24,47:print using "###";b%(i,72);
	IF XCode = 1 GOTO bRLHR:
	IF XCode = 2 GOTO bDone

bRLKO:
	LOCATE 24, 52
	Max = 3
	CALL NumIn(b%(i, 73), Max, XCode, Colr)
	locate 24,52:print using "###";b%(i,73);
	IF XCode = 1 GOTO bRLBB:
	IF XCode = 2 GOTO bDone

bRRBA:
	LOCATE 25, 27
	Max = 3
	CALL NumIn(b%(i, 74), Max, XCode, Colr)
	locate 25,27:print using "###";b%(i,74);
	IF XCode = 1 GOTO bRLKO:
	IF XCode = 2 GOTO bDone

bRR2B:
	LOCATE 25, 32
	Max = 3
	CALL NumIn(b%(i, 75), Max, XCode, Colr)
	locate 25,32:print using "###";b%(i,75);
	IF XCode = 1 GOTO bRRBA:
	IF XCode = 2 GOTO bDone

bRR3B:
	LOCATE 25, 37
	Max = 3
	CALL NumIn(b%(i, 76), Max, XCode, Colr)
	locate 25,37:print using "###";b%(i,76);
	IF XCode = 1 GOTO bRR2B:
	IF XCode = 2 GOTO bDone

bRRHR:
	LOCATE 25, 42
	Max = 3
	CALL NumIn(b%(i, 77), Max, XCode, Colr)
	locate 25,42:print using "###";b%(i,77);
	IF XCode = 1 GOTO bRR3B:
	IF XCode = 2 GOTO bDone

bRRBB:
	LOCATE 25, 47
	Max = 3
	CALL NumIn(b%(i, 78), Max, XCode, Colr)
	locate 25,47:print using "###";b%(i,78);
	IF XCode = 1 GOTO bRRHR:
	IF XCode = 2 GOTO bDone

bRRKO:
	LOCATE 25, 52
	Max = 3
	CALL NumIn(b%(i, 79), Max, XCode, Colr)
	locate 25,52:print using "###";b%(i,79);
	IF XCode = 1 GOTO bRRBB:

bDone:
	COLOR 15,1
	LOCATE 25, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	IF XCode = 1 GOTO bRRKO
	IF X$ = "N" GOTO BName
	IF X$ <> "Y" GOTO bDone               'insist on either Y or N or up-arrow
	b%(i, 0) = 0
	BATS$ = QPTrim$(BATS$)
	IF UCASE$(BATS$) = "L" THEN b%(i, 0) = -1
	IF UCASE$(BATS$) = "R" THEN b%(i, 0) = 1
	IF UCASE$(BATS$) = "LP" THEN b%(i, 0) = -2
	IF UCASE$(BATS$) = "RP" THEN b%(i, 0) = 2
	ACT$ = QPTrim$(ACT$)
	b%(i, 31) = 0
	IF UCASE$(ACT$) = "N" THEN b%(i, 31) = 1
	GOTO 200

300 '
END SUB

SUB EDITTEAM (YN$, T$, NN$, S$, M$, T)
COLOR 15, 1: CLS : LOCATE 1, 36: PRINT "TEAM DATA"
	LOCATE 3, 1: PRINT "TEAM NUMBER:"
	LOCATE 5, 1: PRINT "LEAGUE BATTING AVERAGE:"
	LOCATE 6, 1: PRINT "LEAGUE STRIKEOUT %    :"
	LOCATE 7, 1: PRINT "LEAGUE WALK PERCENTAGE:"
	LOCATE 8, 1: PRINT "LEAGUE HOME RUN %     :"
	LOCATE 10, 1: PRINT "DBL PLAYS/GAME X 100  :"
	LOCATE 14, 1: PRINT "PARK NAME:"
	LOCATE 15, 1: PRINT "PARK ID  :"
	LOCATE 16, 1: PRINT "DOUBLES RATING  :"
	LOCATE 17, 1: PRINT "TRIPLES RATING  :"
	LOCATE 18, 1: PRINT "HOME RUN RATING :"
	LOCATE 19, 1: PRINT "FOUL BALL RATING:"
	LOCATE 21, 1: PRINT "OUTDOOR(0)/INDOOR(1):"
	LOCATE 22, 1: PRINT "GRASS(0)/TURF(1)    :"
	LOCATE 24, 1: PRINT "MANAGER :";
	LOCATE 18, 29: PRINT "TEXT COLOR      :"
	LOCATE 19, 29: PRINT "BACKGROUND COLOR:"
	LOCATE 3, 25: PRINT "DISK ID:"
	LOCATE 3, 44: PRINT "TEAM NAME:"
	LOCATE 4, 44: PRINT "ABBREVIATION:"
	LOCATE 6, 51: PRINT "SPLIT STATS LEAGUE DATA"
	LOCATE 7, 50: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
	LOCATE 8, 38: PRINT "LHB vs LHP"
	LOCATE 9, 38: PRINT "LHB vs RHP"
	LOCATE 10, 38: PRINT "RHB vs LHP"
	LOCATE 11, 38: PRINT "RHB vs RHP"

' INITIALIZE THE FIELDS
	ColOr 0,7
	COLR = 112
	LOCATE 3, 14: PRINT USING "###";T
	LOCATE 3, 33: PRINT YN$
	LOCATE 3, 55: PRINT T$
	LOCATE 4, 58: PRINT NN$
	LOCATE 5, 25: PRINT USING "###";L%(1)
	LOCATE 6, 25: PRINT USING "###";L%(2)
	LOCATE 7, 25: PRINT USING "###";L%(3)
	LOCATE 8, 25: PRINT USING "###";L%(4)
	LOCATE 10, 25:PRINT USING "###";L%(5)
	LOCATE 14, 12: PRINT S$
	LOCATE 15, 12: PRINT USING "###";L%(13)
	LOCATE 16, 19: PRINT USING "###";L%(6)
	LOCATE 17, 19: PRINT USING "###";L%(7)
	LOCATE 18, 19: PRINT USING "######";K9(0)
	LOCATE 19, 19: PRINT USING "###";L%(8)
	LOCATE 21, 25: PRINT USING "#";L%(9)
	LOCATE 22, 25: PRINT USING "#";L%(10)
	LOCATE 24, 11: PRINT M$;
	LOCATE 18, 46: PRINT USING "##";L%(11)
	LOCATE 19, 46: PRINT USING "##";L%(12)
	LOCATE 8, 50: PRINT USING "###";SA%(1)
	LOCATE 8, 55: PRINT USING "###";SA%(2)
	LOCATE 8, 60: PRINT USING "###";SA%(3)
	LOCATE 8, 65: PRINT USING "###";SA%(4)
	LOCATE 8, 70: PRINT USING "###";SA%(5)
	LOCATE 8, 75: PRINT USING "###";SA%(6)
	LOCATE 9, 50: PRINT USING "###";SA%(7)
	LOCATE 9, 55: PRINT USING "###";SA%(8)
	LOCATE 9, 60: PRINT USING "###";SA%(9)
	LOCATE 9, 65: PRINT USING "###";SA%(10)
	LOCATE 9, 70: PRINT USING "###";SA%(11)
	LOCATE 9, 75: PRINT USING "###";SA%(12)
	LOCATE 10, 50:PRINT USING "###";SA%(13)
	LOCATE 10, 55:PRINT USING "###";SA%(14)
	LOCATE 10, 60:PRINT USING "###";SA%(15)
	LOCATE 10, 65:PRINT USING "###";SA%(16)
	LOCATE 10, 70:PRINT USING "###";SA%(17)
	LOCATE 10, 75:PRINT USING "###";SA%(18)
	LOCATE 11, 50:PRINT USING "###";SA%(19)
	LOCATE 11, 55:PRINT USING "###";SA%(20)
	LOCATE 11, 60:PRINT USING "###";SA%(21)
	LOCATE 11, 65:PRINT USING "###";SA%(22)
	LOCATE 11, 70:PRINT USING "###";SA%(23)
	LOCATE 11, 75:PRINT USING "###";SA%(24)
	LOCATE 25, 1: PRINT "ESC-WILL END EDITING";
	HR$ = QPTrim$(STR$(K9(0)))
TeamNum:
	LOCATE 3, 14
	Max = 3
	CALL NumIn(T, Max, XCode, Colr)
	LOCATE 3, 14:PRINT USING "###";T
	IF XCode = 1 GOTO TeamNum:
	IF XCode = 2 GOTO Done

DiskID:
	LOCATE 3, 33
	Max = 3
	CALL TextIn(YN$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO TeamNum:
	IF XCode = 2 GOTO Done

TeamName:
	LOCATE 3, 55
	Max = 15
	CALL TextIn(T$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO DiskID:
	IF XCode = 2 GOTO Done

Abbrev:
	LOCATE 4, 58
	Max = 5
	CALL TextIn(NN$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO TeamName:
	IF XCode = 2 GOTO Done

LgBA:
	LOCATE 5, 25
	Max = 3
	CALL NumIn(L%(1), Max, XCode, Colr)
	LOCATE 5, 25:PRINT USING "###";L%(1)
	IF XCode = 1 GOTO Abbrev:
	IF XCode = 2 GOTO Done

LgKO:
	LOCATE 6, 25
	Max = 3
	CALL NumIn(L%(2), Max, XCode, Colr)
	LOCATE 6, 25:PRINT USING "###";L%(2)
	IF XCode = 1 GOTO LgBA:
	IF XCode = 2 GOTO Done

LgBB:
	LOCATE 7, 25
	Max = 3
	CALL NumIn(L%(3), Max, XCode, Colr)
	LOCATE 7, 25:PRINT USING "###";L%(3)
	IF XCode = 1 GOTO LgKO:
	IF XCode = 2 GOTO Done

LgHR:
	LOCATE 8, 25
	Max = 3
	CALL NumIn(L%(4), Max, XCode, Colr)
	LOCATE 8, 25:PRINT USING "###";L%(4)
	IF XCode = 1 GOTO LgBB:
	IF XCode = 2 GOTO Done

LgDP:
	LOCATE 10, 25
	Max = 3
	CALL NumIn(L%(5), Max, XCode, Colr)
	LOCATE 10, 25:PRINT USING "###";L%(5)
	IF XCode = 1 GOTO LgHR:
	IF XCode = 2 GOTO Done

ParkName:
	LOCATE 14, 12
	Max = 40
	CALL TextIn(S$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO LgDP:
	IF XCode = 2 GOTO Done

ParkID:
	LOCATE 15, 12
	Max = 3
	CALL NumIn(L%(13), Max, XCode, Colr)
	LOCATE 15, 12:PRINT USING "###";L%(13)
	IF XCode = 1 GOTO ParkName:
	IF XCode = 2 GOTO Done

DoubleRating:
	LOCATE 16, 19
	Max = 3
	CALL NumIn(L%(6), Max, XCode, Colr)
	LOCATE 16, 19:PRINT USING "###";L%(6)
	IF XCode = 1 GOTO ParkID:
	IF XCode = 2 GOTO Done

TripleRating:
	LOCATE 17, 19
	Max = 3
	CALL NumIn(L%(7), Max, XCode, Colr)
	LOCATE 17, 19:PRINT USING "###";L%(7)
	IF XCode = 1 GOTO DoubleRating:
	IF XCode = 2 GOTO Done

Homer:
	LOCATE 18, 19
	Max = 6
	CALL TextIn(HR$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO TripleRating:
	IF XCode = 2 GOTO Done

Foul:
	LOCATE 19, 19
	Max = 3
	CALL NumIn(L%(8), Max, XCode, Colr)
	LOCATE 19, 19:PRINT USING "###";L%(8)
	IF XCode = 1 GOTO Homer:
	IF XCode = 2 GOTO Done

Outdoor:
	LOCATE 21, 25
	Max = 1
	CALL NumIn(L%(9), Max, XCode, Colr)
	LOCATE 21, 25:PRINT USING "#";L%(9)
	IF XCode = 1 GOTO Foul:
	IF XCode = 2 GOTO Done

Grass:
	LOCATE 22, 25
	Max = 1
	CALL NumIn(L%(10), Max, XCode, Colr)
	LOCATE 22, 25:PRINT USING "#";L%(10)
	IF XCode = 1 GOTO Outdoor:
	IF XCode = 2 GOTO Done

Manager:
	LOCATE 24, 11
	Max = 15
	CALL TextIn(M$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO Grass:
	IF XCode = 2 GOTO Done

Text:
	LOCATE 18, 46
	Max = 2
	CALL NumIn(L%(11), Max, XCode, Colr)
	LOCATE 18, 46:PRINT USING "##";L%(11)
	IF XCode = 1 GOTO Manager:
	IF XCode = 2 GOTO Done

Bgrnd:
	LOCATE 19, 46
	Max = 2
	CALL NumIn(L%(12), Max, XCode, Colr)
	LOCATE 19, 46:PRINT USING "##";L%(12)
	IF XCode = 1 GOTO Text:
	IF XCode = 2 GOTO Done

LOCATE 20, 29: COLOR L%(11), L%(12): PRINT "SAMPLE TEXT": COLOR 0,7

LLBA:
	LOCATE 8, 50
	Max = 3
	CALL NumIn(SA%(1), Max, XCode, Colr)
	LOCATE 8, 50:PRINT USING "###";SA%(1)
	IF XCode = 1 GOTO Bgrnd:
	IF XCode = 2 GOTO Done

LL2B:
	LOCATE 8, 55
	Max = 3
	CALL NumIn(SA%(2), Max, XCode, Colr)
	LOCATE 8, 55:PRINT USING "###";SA%(2)
	IF XCode = 1 GOTO LLBA:
	IF XCode = 2 GOTO Done

LL3B:
	LOCATE 8, 60
	Max = 3
	CALL NumIn(SA%(3), Max, XCode, Colr)
	LOCATE 8, 60:PRINT USING "###";SA%(3)
	IF XCode = 1 GOTO LL2B:
	IF XCode = 2 GOTO Done

LLHR:
	LOCATE 8, 65
	Max = 3
	CALL NumIn(SA%(4), Max, XCode, Colr)
	LOCATE 8, 65:PRINT USING "###";SA%(4)
	IF XCode = 1 GOTO LL3B:
	IF XCode = 2 GOTO Done

LLBB:
	LOCATE 8, 70
	Max = 3
	CALL NumIn(SA%(5), Max, XCode, Colr)
	LOCATE 8, 70:PRINT USING "###";SA%(5)
	IF XCode = 1 GOTO LLHR:
	IF XCode = 2 GOTO Done

LLKO:
	LOCATE 8, 75
	Max = 3
	CALL NumIn(SA%(6), Max, XCode, Colr)
	LOCATE 8, 75:PRINT USING "###";SA%(6)
	IF XCode = 1 GOTO LLBB:
	IF XCode = 2 GOTO Done

LRBA:
	LOCATE 9, 50
	Max = 3
	CALL NumIn(SA%(7), Max, XCode, Colr)
	LOCATE 9, 50:PRINT USING "###";SA%(7)
	IF XCode = 1 GOTO LLKO:
	IF XCode = 2 GOTO Done

LR2B:
	LOCATE 9, 55
	Max = 3
	CALL NumIn(SA%(8), Max, XCode, Colr)
	LOCATE 9, 55:PRINT USING "###";SA%(8)
	IF XCode = 1 GOTO LRBA:
	IF XCode = 2 GOTO Done

LR3B:
	LOCATE 9, 60
	Max = 3
	CALL NumIn(SA%(9), Max, XCode, Colr)
	LOCATE 9, 60:PRINT USING "###";SA%(9)
	IF XCode = 1 GOTO LR2B:
	IF XCode = 2 GOTO Done

LRHR:
	LOCATE 9, 65
	Max = 3
	CALL NumIn(SA%(10), Max, XCode, Colr)
	LOCATE 9, 65:PRINT USING "###";SA%(10)
	IF XCode = 1 GOTO LR3B:
	IF XCode = 2 GOTO Done

LRBB:
	LOCATE 9, 70
	Max = 3
	CALL NumIn(SA%(11), Max, XCode, Colr)
	LOCATE 9, 70:PRINT USING "###";SA%(11)
	IF XCode = 1 GOTO LRHR:
	IF XCode = 2 GOTO Done

LRKO:
	LOCATE 9, 75
	Max = 3
	CALL NumIn(SA%(12), Max, XCode, Colr)
	LOCATE 9, 75:PRINT USING "###";SA%(12)
	IF XCode = 1 GOTO LRBB:
	IF XCode = 2 GOTO Done

RLBA:
	LOCATE 10, 50
	Max = 3
	CALL NumIn(SA%(13), Max, XCode, Colr)
	LOCATE 10, 50:PRINT USING "###";SA%(13)
	IF XCode = 1 GOTO LRKO:
	IF XCode = 2 GOTO Done

RL2B:
	LOCATE 10, 55
	Max = 3
	CALL NumIn(SA%(14), Max, XCode, Colr)
	LOCATE 10, 55:PRINT USING "###";SA%(14)
	IF XCode = 1 GOTO RLBA:
	IF XCode = 2 GOTO Done

RL3B:
	LOCATE 10, 60
	Max = 3
	CALL NumIn(SA%(15), Max, XCode, Colr)
	LOCATE 10, 60:PRINT USING "###";SA%(15)
	IF XCode = 1 GOTO RL2B:
	IF XCode = 2 GOTO Done

RLHR:
	LOCATE 10, 65
	Max = 3
	CALL NumIn(SA%(16), Max, XCode, Colr)
	LOCATE 10, 65:PRINT USING "###";SA%(16)
	IF XCode = 1 GOTO RL3B:
	IF XCode = 2 GOTO Done

RLBB:
	LOCATE 10, 70
	Max = 3
	CALL NumIn(SA%(17), Max, XCode, Colr)
	LOCATE 10, 70:PRINT USING "###";SA%(17)
	IF XCode = 1 GOTO RLHR:
	IF XCode = 2 GOTO Done

RLKO:
	LOCATE 10, 75
	Max = 3
	CALL NumIn(SA%(18), Max, XCode, Colr)
	LOCATE 10, 75:PRINT USING "###";SA%(18)
	IF XCode = 1 GOTO RLBB:
	IF XCode = 2 GOTO Done

RRBA:
	LOCATE 11, 50
	Max = 3
	CALL NumIn(SA%(19), Max, XCode, Colr)
	LOCATE 11, 50:PRINT USING "###";SA%(19)
	IF XCode = 1 GOTO RLKO:
	IF XCode = 2 GOTO Done

RR2B:
	LOCATE 11, 55
	Max = 3
	CALL NumIn(SA%(20), Max, XCode, Colr)
	LOCATE 11, 55:PRINT USING "###";SA%(20)
	IF XCode = 1 GOTO RRBA:
	IF XCode = 2 GOTO Done

RR3B:
	LOCATE 11, 60
	Max = 3
	CALL NumIn(SA%(21), Max, XCode, Colr)
	LOCATE 11, 60:PRINT USING "###";SA%(21)
	IF XCode = 1 GOTO RR2B:
	IF XCode = 2 GOTO Done

RRHR:
	LOCATE 11, 65
	Max = 3
	CALL NumIn(SA%(22), Max, XCode, Colr)
	LOCATE 11, 65:PRINT USING "###";SA%(22)
	IF XCode = 1 GOTO RR3B:
	IF XCode = 2 GOTO Done

RRBB:
	LOCATE 11, 70
	Max = 3
	CALL NumIn(SA%(23), Max, XCode, Colr)
	LOCATE 11, 70:PRINT USING "###";SA%(23)
	IF XCode = 1 GOTO RRHR:
	IF XCode = 2 GOTO Done

RRKO:
	LOCATE 11, 75
	Max = 3
	CALL NumIn(SA%(24), Max, XCode, Colr)
	LOCATE 11, 75:PRINT USING "###";SA%(24)
	IF XCode = 1 GOTO RRBB:

Done:
	LOCATE 25, 1
	PRINT "Is this okay? (Y)es/(N)o/(A)bort ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	'ClearEOL 30
	IF XCode = 1 GOTO RRKO
	IF X$ = "N" GOTO TeamNum
	IF X$ = "A" THEN RUN "HELLO.EXE"
	IF X$ <> "Y" GOTO Done               'insist on either Y or N or up-arrow
	K9(0) = VAL(HR$)
END SUB

SUB PITCHEREDIT ()

1150 COLOR 15, 0:
	CLS
	PRINT "EDIT/ENTER PITCHERS"
	X% = 2
	FOR i = 0 TO 10
	LOCATE i + 5, 5: PRINT USING "##"; i + 1; : PRINT " "; p$(i); TAB(40);
	PRINT USING "##"; i + 12; : PRINT " "; p$(i + 11)
	NEXT

1175 LOCATE 22, 1
	PRINT "# TO EDIT/ENTER (1-22) (0 TO CONTINUE) ";
	INPUT i
	IF i < 0 OR i > 22 THEN 1175
	i = i - 1

1195 IF i < 0 THEN 1180

' INITIALIZE PITCHER SCREEN

COLOR 15, 1: CLS
	LOCATE 1, 1: PRINT "NAME:"
	LOCATE 1, 24: PRINT "THROWS:"
	LOCATE 1, 36: PRINT "ACTIVE:"
	LOCATE 1, 48: PRINT "INJ LENGTH:"
	LOCATE 1, 66: PRINT "HOLD (1-9):"
	LOCATE 3, 34: PRINT "PITCHING STATS"
	LOCATE 4, 11: PRINT "W   L  SV    G  GS  CG   IP    H   BB    K  WP   ERA  HRA"
	LOCATE 5, 1: PRINT "SEASON:"
	LOCATE 6, 27: PRINT "G/A:"
	LOCATE 6, 37: PRINT "LEAGUE BAVG:"
	LOCATE 8, 31: PRINT "SPLIT STATS AGAINST"
	LOCATE 9, 26: PRINT "AB    H  2B  3B  HR   BB    K"
	LOCATE 10, 1: PRINT "VS LHB:"
	LOCATE 11, 1: PRINT "VS RHB:"
	LOCATE 13, 30: PRINT "HITTING STATISTICS"
	LOCATE 14, 9: PRINT "RUN (1-9):"
	LOCATE 14, 24: PRINT "BUNT (1-9):"
	LOCATE 14, 39: PRINT "BATTING AVG:"
	LOCATE 14, 58: PRINT "FLDG AVG:"
	LOCATE 15, 14: PRINT "G   AB    R    H  2B  3B  HR  RBI   BB    K   SB  CS  SH"
	LOCATE 16, 1: PRINT "SEASON:"
	LOCATE 17, 1: PRINT "VS LHP:"
	LOCATE 18, 1: PRINT "VS RHP:"
	LOCATE 20, 29: PRINT "SPLIT STATS LEAGUE DATA"
	LOCATE 21, 26: PRINT "BAVG   K%  BB%  HR%  2B%  3B%"
	LOCATE 22, 13: PRINT "LHB vs LHP"
	LOCATE 23, 13: PRINT "LHB vs RHP"
	LOCATE 24, 13: PRINT "RHB vs LHP";
	LOCATE 25, 13: PRINT "RHB vs RHP";

' INITIALIZE PITCHER FIELDS
	Colr = 112
	LOCATE 1, 7: QPrint FnExt$(p$(i), 15), FG, BG, -1
	THROW$ = "R"
	IF p%(i, 0) = -1 THEN THROW$ = "L"
	LOCATE 1, 32: QPrint FnExt$(THROW$, 1), FG, BG, -1
	ACT$ = "Y"
	IF p%(i, 34) = 1 THEN ACT$ = "N"
	LOCATE 1, 44: QPrint FnExt$(ACT$, 1), FG, BG, -1
	CALL PITCHINIT(i, Colr)
	CALL PITCHINIT2(i, Colr)

PName:
	LOCATE 1, 7
	CapsOn = 0: NumOnly = 0: Max = 15
	CALL TextIn(p$(i), Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO PName:
	IF XCode = 2 GOTO PDone

THROW:
	LOCATE 1, 32
	Max = 1
	CALL TextIn(THROW$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO PName:
	IF XCode = 2 GOTO PDone

PActive:
	LOCATE 1, 44
	Max = 1
	CALL TextIn(ACT$, Max, NumOnly, CapsOn, XCode, Colr)
	IF XCode = 1 GOTO THROW:
	IF XCode = 2 GOTO PDone

PINJ:
	LOCATE 1, 60
	Max = 3
	CALL NumIn(p%(i, 35), Max, XCode, Colr)
	LOCATE 1,60:PRINT USING "###";P%(I,35)
	IF XCode = 1 GOTO PActive:
	IF XCode = 2 GOTO PDone

PHOLD:
	LOCATE 1, 78
	Max = 1
	CALL NumIn(p%(i, 18), Max, XCode, Colr)
	LOCATE 1,78:PRINT USING "#";P%(I,18)
	IF XCode = 1 GOTO PINJ:
	IF XCode = 2 GOTO PDone

PWINS:
	LOCATE 5, 10
	Max = 2
	CALL NumIn(p%(i, 1), Max, XCode, Colr)
	LOCATE 5, 10:PRINT USING "##";P%(I,1)
	IF XCode = 1 GOTO PHOLD:
	IF XCode = 2 GOTO PDone

PLOSSES:
	LOCATE 5, 14
	Max = 2
	CALL NumIn(p%(i, 2), Max, XCode, Colr)
	LOCATE 5, 14:PRINT USING "##";P%(I,2)
	IF XCode = 1 GOTO PWINS:
	IF XCode = 2 GOTO PDone

PSAVES:
	LOCATE 5, 18
	Max = 2
	CALL NumIn(p%(i, 3), Max, XCode, Colr)
	LOCATE 5, 18:PRINT USING "##";P%(I,3)
	IF XCode = 1 GOTO PLOSSES:
	IF XCode = 2 GOTO PDone

PGAMES:
	LOCATE 5, 22
	Max = 3
	CALL NumIn(p%(i, 4), Max, XCode, Colr)
	LOCATE 5, 22:PRINT USING "###";P%(I,4)
	IF XCode = 1 GOTO PSAVES:
	IF XCode = 2 GOTO PDone

PSTARTS:
	LOCATE 5, 27
	Max = 2
	CALL NumIn(p%(i, 5), Max, XCode, Colr)
	LOCATE 5, 27:PRINT USING "##";P%(I,5)
	IF XCode = 1 GOTO PGAMES:
	IF XCode = 2 GOTO PDone

PCG:
	LOCATE 5, 31
	Max = 2
	CALL NumIn(p%(i, 16), Max, XCode, Colr)
	LOCATE 5, 31:PRINT USING "##";P%(I,16)
	IF XCode = 1 GOTO PSTARTS:
	IF XCode = 2 GOTO PDone

PIP:
	LOCATE 5, 35
	Max = 3
	CALL NumIn(p%(i, 6), Max, XCode, Colr)
	LOCATE 5, 35:PRINT USING "###";P%(I,6)
	IF XCode = 1 GOTO PCG:
	IF XCode = 2 GOTO PDone

PH:
	LOCATE 5, 40
	Max = 3
	CALL NumIn(p%(i, 7), Max, XCode, Colr)
	LOCATE 5, 40:PRINT USING "###";P%(I,7)
	IF XCode = 1 GOTO PIP:
	IF XCode = 2 GOTO PDone

PBB:
	LOCATE 5, 45
	Max = 3
	CALL NumIn(p%(i, 8), Max, XCode, Colr)
	LOCATE 5, 45:PRINT USING "###";P%(I,8)
	IF XCode = 1 GOTO PH:
	IF XCode = 2 GOTO PDone

PKO:
	LOCATE 5, 50
	Max = 3
	CALL NumIn(p%(i, 9), Max, XCode, Colr)
	LOCATE 5, 50:PRINT USING "###";P%(I,9)
	IF XCode = 1 GOTO PBB:
	IF XCode = 2 GOTO PDone

PWP:
	LOCATE 5, 55
	Max = 2
	CALL NumIn(p%(i, 33), Max, XCode, Colr)
	LOCATE 5, 55:PRINT USING "##";P%(I,33)
	IF XCode = 1 GOTO PKO:
	IF XCode = 2 GOTO PDone

PERA:
	LOCATE 5, 59
	Max = 4
	CALL NumIn(p%(i, 10), Max, XCode, Colr)
	LOCATE 5, 59:PRINT USING "####";P%(I,10)
	IF XCode = 1 GOTO PWP:
	IF XCode = 2 GOTO PDone

PHRA:
	LOCATE 5, 66
	Max = 2
	CALL NumIn(p%(i, 13), Max, XCode, Colr)
	LOCATE 5, 66:PRINT USING "##";P%(I,13)
	IF XCode = 1 GOTO PERA:
	IF XCode = 2 GOTO PDone

PGA:
	LOCATE 6, 32
	Max = 2
	CALL NumIn(p%(i, 12), Max, XCode, Colr)
	LOCATE 6, 32:PRINT USING "##";P%(I,12)
	IF XCode = 1 GOTO PHRA:
	IF XCode = 2 GOTO PDone

PLBA:
	LOCATE 6, 50
	Max = 3
	CALL NumIn(p%(i, 14), Max, XCode, Colr)
	LOCATE 6, 50:PRINT USING "###";P%(I,14)
	IF XCode = 1 GOTO PHRA:
	IF XCode = 2 GOTO PDone

PLAB:
	LOCATE 10, 25
	Max = 3
	CALL NumIn(p%(i, 51), Max, XCode, Colr)
	LOCATE 10, 25:PRINT USING "###";P%(I,51)
	IF XCode = 1 GOTO PLBA:
	IF XCode = 2 GOTO PDone

PLH:
	LOCATE 10, 30
	Max = 3
	CALL NumIn(p%(i, 52), Max, XCode, Colr)
	LOCATE 10, 30:PRINT USING "###";P%(I,52)
	IF XCode = 1 GOTO PLAB:
	IF XCode = 2 GOTO PDone

PL2B:
	LOCATE 10, 35
	Max = 2
	CALL NumIn(p%(i, 53), Max, XCode, Colr)
	LOCATE 10, 35:PRINT USING "##";P%(I,53)
	IF XCode = 1 GOTO PLH:
	IF XCode = 2 GOTO PDone

PL3B:
	LOCATE 10, 39
	Max = 2
	CALL NumIn(p%(i, 54), Max, XCode, Colr)
	LOCATE 10, 39:PRINT USING "##";P%(I,54)
	IF XCode = 1 GOTO PL2B:
	IF XCode = 2 GOTO PDone

PLHR:
	LOCATE 10, 43
	Max = 2
	CALL NumIn(p%(i, 55), Max, XCode, Colr)
	LOCATE 10, 43:PRINT USING "##";P%(I,55)
	IF XCode = 1 GOTO PL3B:
	IF XCode = 2 GOTO PDone

PLBB:
	LOCATE 10, 47
	Max = 3
	CALL NumIn(p%(i, 56), Max, XCode, Colr)
	LOCATE 10, 47:PRINT USING "###";P%(I,56)
	IF XCode = 1 GOTO PLHR:
	IF XCode = 2 GOTO PDone

PLKO:
	LOCATE 10, 52
	Max = 3
	CALL NumIn(p%(i, 57), Max, XCode, Colr)
	LOCATE 10, 52:PRINT USING "###";P%(I,57)
	IF XCode = 1 GOTO PLBB:
	IF XCode = 2 GOTO PDone

PRAB:
	LOCATE 11, 25
	Max = 3
	CALL NumIn(p%(i, 58), Max, XCode, Colr)
	LOCATE 11, 25:PRINT USING "###";P%(I,58)
	IF XCode = 1 GOTO PLKO:
	IF XCode = 2 GOTO PDone

PRH:
	LOCATE 11, 30
	Max = 3
	CALL NumIn(p%(i, 59), Max, XCode, Colr)
	LOCATE 11, 30:PRINT USING "###";P%(I,59)
	IF XCode = 1 GOTO PRAB:
	IF XCode = 2 GOTO PDone

PR2B:
	LOCATE 11, 35
	Max = 2
	CALL NumIn(p%(i, 60), Max, XCode, Colr)
	LOCATE 11, 35:PRINT USING "##";P%(I,60)
	IF XCode = 1 GOTO PRH:
	IF XCode = 2 GOTO PDone

PR3B:
	LOCATE 11, 39
	Max = 2
	CALL NumIn(p%(i, 61), Max, XCode, Colr)
	LOCATE 11, 39:PRINT USING "##";P%(I,61)
	IF XCode = 1 GOTO PR2B:
	IF XCode = 2 GOTO PDone

PRHR:
	LOCATE 11, 43
	Max = 2
	CALL NumIn(p%(i, 62), Max, XCode, Colr)
	LOCATE 11, 43:PRINT USING "##";P%(I,62)
	IF XCode = 1 GOTO PR3B:
	IF XCode = 2 GOTO PDone

PRBB:
	LOCATE 11, 47
	Max = 3
	CALL NumIn(p%(i, 63), Max, XCode, Colr)
	LOCATE 11, 47:PRINT USING "###";P%(I,63)
	IF XCode = 1 GOTO PRHR:
	IF XCode = 2 GOTO PDone

PRKO:
	LOCATE 11, 52
	Max = 3
	CALL NumIn(p%(i, 64), Max, XCode, Colr)
	LOCATE 11, 52:PRINT USING "###";P%(I,64)
	IF XCode = 1 GOTO PRBB:
	IF XCode = 2 GOTO PDone

PRUN:
	LOCATE 14, 20
	Max = 1
	CALL NumIn(p%(i, 31), Max, XCode, Colr)
	LOCATE 14, 20:PRINT USING "#";P%(I,31)
	IF XCode = 1 GOTO PRKO:
	IF XCode = 2 GOTO PDone

PBUNT:
	LOCATE 14, 36
	Max = 1
	CALL NumIn(p%(i, 32), Max, XCode, Colr)
	LOCATE 14, 36:PRINT USING "#";P%(I,32)
	IF XCode = 1 GOTO PRKO:
	IF XCode = 2 GOTO PDone

PBAVG:
	LOCATE 14, 52
	Max = 3
	CALL NumIn(p%(i, 11), Max, XCode, Colr)
	LOCATE 14, 52:PRINT USING "###";P%(I,11)
	IF XCode = 1 GOTO PBUNT:
	IF XCode = 2 GOTO PDone

PFAVG:
	LOCATE 14, 68
	Max = 4
	CALL NumIn(p%(i, 17), Max, XCode, Colr)
	LOCATE 14, 68:PRINT USING "####";P%(I,17)
	IF XCode = 1 GOTO PBAVG:
	IF XCode = 2 GOTO PDone

PBGAMES:
	LOCATE 16, 12
	Max = 3
	CALL NumIn(p%(i, 19), Max, XCode, Colr)
	LOCATE 16, 12:PRINT USING "###";P%(I,19)
	IF XCode = 1 GOTO PFAVG:
	IF XCode = 2 GOTO PDone

PAB:
	LOCATE 16, 17
	Max = 3
	CALL NumIn(p%(i, 20), Max, XCode, Colr)
	LOCATE 16, 17:PRINT USING "###";P%(I,20)
	IF XCode = 1 GOTO PBGAMES:
	IF XCode = 2 GOTO PDone

PR:
	LOCATE 16, 22
	Max = 3
	CALL NumIn(p%(i, 21), Max, XCode, Colr)
	LOCATE 16, 22:PRINT USING "###";P%(I,21)
	IF XCode = 1 GOTO PAB:
	IF XCode = 2 GOTO PDone

PBH:
	LOCATE 16, 27
	Max = 3
	CALL NumIn(p%(i, 22), Max, XCode, Colr)
	LOCATE 16, 27:PRINT USING "###";P%(I,22)
	IF XCode = 1 GOTO PR:
	IF XCode = 2 GOTO PDone

P2B:
	LOCATE 16, 32
	Max = 2
	CALL NumIn(p%(i, 23), Max, XCode, Colr)
	LOCATE 16, 32:PRINT USING "##";P%(I,23)
	IF XCode = 1 GOTO PBH:
	IF XCode = 2 GOTO PDone

P3B:
	LOCATE 16, 36
	Max = 2
	CALL NumIn(p%(i, 24), Max, XCode, Colr)
	LOCATE 16, 36:PRINT USING "##";P%(I,24)
	IF XCode = 1 GOTO P2B:
	IF XCode = 2 GOTO PDone

PHR:
	LOCATE 16, 40
	Max = 2
	CALL NumIn(p%(i, 25), Max, XCode, Colr)
	LOCATE 16, 40:PRINT USING "##";P%(I,25)
	IF XCode = 1 GOTO P3B:
	IF XCode = 2 GOTO PDone

PRBI:
	LOCATE 16, 44
	Max = 3
	CALL NumIn(p%(i, 26), Max, XCode, Colr)
	LOCATE 16, 44:PRINT USING "###";P%(I,26)
	IF XCode = 1 GOTO PHR:
	IF XCode = 2 GOTO PDone

PBBB:
	LOCATE 16, 49
	Max = 3
	CALL NumIn(p%(i, 27), Max, XCode, Colr)
	LOCATE 16, 49:PRINT USING "###";P%(I,27)
	IF XCode = 1 GOTO PRBI:
	IF XCode = 2 GOTO PDone

PBKO:
	LOCATE 16, 54
	Max = 3
	CALL NumIn(p%(i, 28), Max, XCode, Colr)
	LOCATE 16, 54:PRINT USING "###";P%(I,28)
	IF XCode = 1 GOTO PBBB:
	IF XCode = 2 GOTO PDone

PSB:
	LOCATE 16, 59
	Max = 3
	CALL NumIn(p%(i, 29), Max, XCode, Colr)
	LOCATE 16, 59:PRINT USING "###";P%(I,29)
	IF XCode = 1 GOTO PBKO:
	IF XCode = 2 GOTO PDone

PCS:
	LOCATE 16, 64
	Max = 2
	CALL NumIn(p%(i, 30), Max, XCode, Colr)
	LOCATE 16, 64:PRINT USING "##";P%(I,30)
	IF XCode = 1 GOTO PSB:
	IF XCode = 2 GOTO PDone

PSH:
	LOCATE 16, 68
	Max = 2
	CALL NumIn(p%(i, 36), Max, XCode, Colr)
	LOCATE 16, 68:PRINT USING "##";P%(I,36)
	IF XCode = 1 GOTO PCS:
	IF XCode = 2 GOTO PDone

PPLAB:
	LOCATE 17, 17
	Max = 3
	CALL NumIn(p%(i, 37), Max, XCode, Colr)
	LOCATE 17, 17:PRINT USING "###";P%(I,37)
	IF XCode = 1 GOTO PSH:
	IF XCode = 2 GOTO PDone

PPLH:
	LOCATE 17, 27
	Max = 3
	CALL NumIn(p%(i, 38), Max, XCode, Colr)
	LOCATE 17, 27:PRINT USING "###";P%(I,38)
	IF XCode = 1 GOTO PPLAB:
	IF XCode = 2 GOTO PDone

PPL2B:
	LOCATE 17, 32
	Max = 2
	CALL NumIn(p%(i, 39), Max, XCode, Colr)
	LOCATE 17, 32:PRINT USING "##";P%(I,39)
	IF XCode = 1 GOTO PPLH:
	IF XCode = 2 GOTO PDone

PPL3B:
	LOCATE 17, 36
	Max = 2
	CALL NumIn(p%(i, 40), Max, XCode, Colr)
	LOCATE 17, 36:PRINT USING "##";P%(I,40)
	IF XCode = 1 GOTO PPL2B:
	IF XCode = 2 GOTO PDone

PPLHR:
	LOCATE 17, 40
	Max = 2
	CALL NumIn(p%(i, 41), Max, XCode, Colr)
	LOCATE 17, 40:PRINT USING "##";P%(I,41)
	IF XCode = 1 GOTO PPL3B:
	IF XCode = 2 GOTO PDone

PPLBB:
	LOCATE 17, 49
	Max = 3
	CALL NumIn(p%(i, 42), Max, XCode, Colr)
	LOCATE 17, 49:PRINT USING "###";P%(I,42)
	IF XCode = 1 GOTO PPLHR:
	IF XCode = 2 GOTO PDone

PPLKO:
	LOCATE 17, 54
	Max = 3
	CALL NumIn(p%(i, 43), Max, XCode, Colr)
	LOCATE 17, 54:PRINT USING "###";P%(I,43)
	IF XCode = 1 GOTO PPLBB:
	IF XCode = 2 GOTO PDone

PPRAB:
	LOCATE 18, 17
	Max = 3
	CALL NumIn(p%(i, 44), Max, XCode, Colr)
	LOCATE 18, 17:PRINT USING "###";P%(I,44)
	IF XCode = 1 GOTO PPLKO:
	IF XCode = 2 GOTO PDone

PPRH:
	LOCATE 18, 27
	Max = 3
	CALL NumIn(p%(i, 45), Max, XCode, Colr)
	LOCATE 18, 27:PRINT USING "###";P%(I,45)
	IF XCode = 1 GOTO PPRAB:
	IF XCode = 2 GOTO PDone

PPR2B:
	LOCATE 18, 32
	Max = 2
	CALL NumIn(p%(i, 46), Max, XCode, Colr)
	LOCATE 18, 32:PRINT USING "##";P%(I,46)
	IF XCode = 1 GOTO PPRH:
	IF XCode = 2 GOTO PDone

PPR3B:
	LOCATE 18, 36
	Max = 2
	CALL NumIn(p%(i, 47), Max, XCode, Colr)
	LOCATE 18, 36:PRINT USING "##";P%(I,47)
	IF XCode = 1 GOTO PPR2B:
	IF XCode = 2 GOTO PDone

PPRHR:
	LOCATE 18, 40
	Max = 2
	CALL NumIn(p%(i, 48), Max, XCode, Colr)
	LOCATE 18, 40:PRINT USING "##";P%(I,48)
	IF XCode = 1 GOTO PPR3B:
	IF XCode = 2 GOTO PDone

PPRBB:
	LOCATE 18, 49
	Max = 3
	CALL NumIn(p%(i, 49), Max, XCode, Colr)
	LOCATE 18, 49:PRINT USING "###";P%(I,49)
	IF XCode = 1 GOTO PPRHR:
	IF XCode = 2 GOTO PDone

PPRKO:
	LOCATE 18, 54
	Max = 3
	CALL NumIn(p%(i, 50), Max, XCode, Colr)
	LOCATE 18, 54:PRINT USING "###";P%(I,50)
	IF XCode = 1 GOTO PPRBB:
	IF XCode = 2 GOTO PDone

PLLBA:
	LOCATE 22, 27
	Max = 3
	CALL NumIn(p%(i, 65), Max, XCode, Colr)
	LOCATE 22, 27:PRINT USING "###";P%(I,65)
	IF XCode = 1 GOTO PPRKO:
	IF XCode = 2 GOTO PDone

PLL2B:
	LOCATE 22, 32
	Max = 3
	CALL NumIn(p%(i, 66), Max, XCode, Colr)
	LOCATE 22, 32:PRINT USING "###";P%(I,66)
	IF XCode = 1 GOTO PLLBA:
	IF XCode = 2 GOTO PDone

PLL3B:
	LOCATE 22, 37
	Max = 3
	CALL NumIn(p%(i, 67), Max, XCode, Colr)
	LOCATE 22, 37:PRINT USING "###";P%(I,67)
	IF XCode = 1 GOTO PLL2B:
	IF XCode = 2 GOTO PDone

PLLHR:
	LOCATE 22, 42
	Max = 3
	CALL NumIn(p%(i, 68), Max, XCode, Colr)
	LOCATE 22, 42:PRINT USING "###";P%(I,68)
	IF XCode = 1 GOTO PLL3B:
	IF XCode = 2 GOTO PDone

PLLBB:
	LOCATE 22, 47
	Max = 3
	CALL NumIn(p%(i, 69), Max, XCode, Colr)
	LOCATE 22, 47:PRINT USING "###";P%(I,69)
	IF XCode = 1 GOTO PLLHR:
	IF XCode = 2 GOTO PDone

PLLKO:
	LOCATE 22, 52
	Max = 3
	CALL NumIn(p%(i, 70), Max, XCode, Colr)
	LOCATE 22, 52:PRINT USING "###";P%(I,70)
	IF XCode = 1 GOTO PLLBB:
	IF XCode = 2 GOTO PDone

PLRBA:
	LOCATE 23, 27
	Max = 3
	CALL NumIn(p%(i, 71), Max, XCode, Colr)
	LOCATE 23, 27:PRINT USING "###";P%(I,71)
	IF XCode = 1 GOTO PLLKO:
	IF XCode = 2 GOTO PDone

PLR2B:
	LOCATE 23, 32
	Max = 3
	CALL NumIn(p%(i, 72), Max, XCode, Colr)
	LOCATE 23, 32:PRINT USING "###";P%(I,72)
	IF XCode = 1 GOTO PLRBA:
	IF XCode = 2 GOTO PDone

PLR3B:
	LOCATE 23, 37
	Max = 3
	CALL NumIn(p%(i, 73), Max, XCode, Colr)
	LOCATE 23, 37:PRINT USING "###";P%(I,73)
	IF XCode = 1 GOTO PLR2B:
	IF XCode = 2 GOTO PDone

PLRHR:
	LOCATE 23, 42
	Max = 3
	CALL NumIn(p%(i, 74), Max, XCode, Colr)
	LOCATE 23, 42:PRINT USING "###";P%(I,74)
	IF XCode = 1 GOTO PLR3B:
	IF XCode = 2 GOTO PDone

PLRBB:
	LOCATE 23, 47
	Max = 3
	CALL NumIn(p%(i, 75), Max, XCode, Colr)
	LOCATE 23, 47:PRINT USING "###";P%(I,75)
	IF XCode = 1 GOTO PLRHR:
	IF XCode = 2 GOTO PDone

PLRKO:
	LOCATE 23, 52
	Max = 3
	CALL NumIn(p%(i, 76), Max, XCode, Colr)
	LOCATE 23, 52:PRINT USING "###";P%(I,76)
	IF XCode = 1 GOTO PLRBB:
	IF XCode = 2 GOTO PDone

PRLBA:
	LOCATE 24, 27
	Max = 3
	CALL NumIn(p%(i, 77), Max, XCode, Colr)
	LOCATE 24, 27:PRINT USING "###";P%(I,77);
	IF XCode = 1 GOTO PLRKO:
	IF XCode = 2 GOTO PDone

PRL2B:
	LOCATE 24, 32
	Max = 3
	CALL NumIn(p%(i, 78), Max, XCode, Colr)
	LOCATE 24, 32:PRINT USING "###";P%(I,78);
	IF XCode = 1 GOTO PRLBA:
	IF XCode = 2 GOTO PDone

PRL3B:
	LOCATE 24, 37
	Max = 3
	CALL NumIn(p%(i, 79), Max, XCode, Colr)
	LOCATE 24, 37:PRINT USING "###";P%(I,79);
	IF XCode = 1 GOTO PRL2B:
	IF XCode = 2 GOTO PDone

PRLHR:
	LOCATE 24, 42
	Max = 3
	CALL NumIn(p%(i, 80), Max, XCode, Colr)
	LOCATE 24, 42:PRINT USING "###";P%(I,80);
	IF XCode = 1 GOTO PRL3B:
	IF XCode = 2 GOTO PDone

PRLBB:
	LOCATE 24, 47
	Max = 3
	CALL NumIn(p%(i, 81), Max, XCode, Colr)
	LOCATE 24, 47:PRINT USING "###";P%(I,81);
	IF XCode = 1 GOTO PRLHR:
	IF XCode = 2 GOTO PDone

PRLKO:
	LOCATE 24, 52
	Max = 3
	CALL NumIn(p%(i, 82), Max, XCode, Colr)
	LOCATE 24, 52:PRINT USING "###";P%(I,82);
	IF XCode = 1 GOTO PRLBB:
	IF XCode = 2 GOTO PDone

PRRBA:
	LOCATE 25, 27
	Max = 3
	CALL NumIn(p%(i, 83), Max, XCode, Colr)
	LOCATE 25, 27:PRINT USING "###";P%(I,83);
	IF XCode = 1 GOTO PRLKO:
	IF XCode = 2 GOTO PDone

PRR2B:
	LOCATE 25, 32
	Max = 3
	CALL NumIn(p%(i, 84), Max, XCode, Colr)
	LOCATE 25, 32:PRINT USING "###";P%(I,84);
	IF XCode = 1 GOTO PRRBA:
	IF XCode = 2 GOTO PDone

PRR3B:
	LOCATE 25, 37
	Max = 3
	CALL NumIn(p%(i, 85), Max, XCode, Colr)
	LOCATE 25, 37:PRINT USING "###";P%(I,85);
	IF XCode = 1 GOTO PRR2B:
	IF XCode = 2 GOTO PDone

PRRHR:
	LOCATE 25, 42
	Max = 3
	CALL NumIn(p%(i, 86), Max, XCode, Colr)
	LOCATE 25, 42:PRINT USING "###";P%(I,86);
	IF XCode = 1 GOTO PRR3B:
	IF XCode = 2 GOTO PDone

PRRBB:
	LOCATE 25, 47
	Max = 3
	CALL NumIn(p%(i, 87), Max, XCode, Colr)
	LOCATE 25, 47:PRINT USING "###";P%(I,87);
	IF XCode = 1 GOTO PRRHR:
	IF XCode = 2 GOTO PDone

PRRKO:
	LOCATE 25, 52
	Max = 3
	CALL NumIn(p%(i, 88), Max, XCode, Colr)
	LOCATE 25, 52:PRINT USING "###";P%(I,88);
	IF XCode = 1 GOTO PRRBB:

PDone:
	LOCATE 25, 1
	PRINT "Is this okay? (Yes/No) ";
	X$ = ""
	CALL YesNoB(X$, XCode, Colr)
	LOCATE , 1
	'ClearEOL 30
	IF XCode = 1 GOTO PRRKO
	IF X$ = "N" GOTO PName
	IF X$ <> "Y" GOTO PDone               'insist on either Y or N or up-arrow
	p%(i, 0) = 1
	THROW$ = QPTrim$(THROW$)
	IF UCASE$(THROW$) = "L" THEN p%(i, 0) = -1
	ACT$ = QPTrim$(ACT$)
	p%(i, 34) = 0
	IF UCASE$(ACT$) = "N" THEN p%(i, 34) = 1
	 GOTO 1150

1180 END SUB

SUB PITCHINIT (i, Colr)
	COLOR 0,7
	LOCATE 1, 60: PRINT USING "###";p%(i, 35)
	LOCATE 1, 78: PRINT USING "#";p%(i, 18)
	LOCATE 5, 10: PRINT USING "##";p%(i, 1)
	LOCATE 5, 14: PRINT USING "##";p%(i, 2)
	LOCATE 5, 18: PRINT USING "##";p%(i, 3)
	LOCATE 5, 22: PRINT USING "###";p%(i, 4)
	LOCATE 5, 27: PRINT USING "##";p%(i, 5)
	LOCATE 5, 31: PRINT USING "##";p%(i, 16)
	LOCATE 5, 35: PRINT USING "###";p%(i, 6)
	LOCATE 5, 40: PRINT USING "###";p%(i, 7)
	LOCATE 5, 45: PRINT USING "###";p%(i, 8)
	LOCATE 5, 50: PRINT USING "###";p%(i, 9)
	LOCATE 5, 55: PRINT USING "##";p%(i, 33)
	LOCATE 5, 59: PRINT USING "####";p%(i, 10)
	LOCATE 5, 66: PRINT USING "##";p%(i, 13)
	LOCATE 6, 32: PRINT USING "##";p%(i, 12)
	LOCATE 6, 50: PRINT USING "###";p%(i, 14)
	LOCATE 10, 25: PRINT USING "###";p%(i, 51)
	LOCATE 10, 30: PRINT USING "###";p%(i, 52)
	LOCATE 10, 35: PRINT USING "##";p%(i, 53)
	LOCATE 10, 39: PRINT USING "##";p%(i, 54)
	LOCATE 10, 43: PRINT USING "##";p%(i, 55)
	LOCATE 10, 47: PRINT USING "###";p%(i, 56)
	LOCATE 10, 52: PRINT USING "###";p%(i, 57)
	LOCATE 11, 25: PRINT USING "###";p%(i, 58)
	LOCATE 11, 30: PRINT USING "###";p%(i, 59)
	LOCATE 11, 35: PRINT USING "##";p%(i, 60)
	LOCATE 11, 39: PRINT USING "##";p%(i, 61)
	LOCATE 11, 43: PRINT USING "##";p%(i, 62)
	LOCATE 11, 47: PRINT USING "###";p%(i, 63)
	LOCATE 11, 52: PRINT USING "###";p%(i, 64)
	LOCATE 14, 20: PRINT USING "#";p%(i, 31)
	LOCATE 14, 36: PRINT USING "#";p%(i, 32)
	LOCATE 14, 52: PRINT USING "###";p%(i, 11)
	LOCATE 14, 68: PRINT USING "####";p%(i, 17)
	LOCATE 16, 12: PRINT USING "###";p%(i, 19)
	LOCATE 16, 17: PRINT USING "###";p%(i, 20)
	LOCATE 16, 22: PRINT USING "###";p%(i, 21)
	LOCATE 16, 27: PRINT USING "###";p%(i, 22)
	LOCATE 16, 32: PRINT USING "##";p%(i, 23)
	LOCATE 16, 36: PRINT USING "##";p%(i, 24)
	LOCATE 16, 40: PRINT USING "##";p%(i, 25)
	LOCATE 16, 44: PRINT USING "###";p%(i, 26)
	LOCATE 16, 49: PRINT USING "###";p%(i, 27)
	LOCATE 16, 54: PRINT USING "###";p%(i, 28)
	LOCATE 16, 59: PRINT USING "###";p%(i, 29)
	LOCATE 16, 64: PRINT USING "##";p%(i, 30)
	LOCATE 16, 68: PRINT USING "##";p%(i, 36)
	LOCATE 17, 17: PRINT USING "###";p%(i, 37)
	LOCATE 17, 27: PRINT USING "###";p%(i, 38)
	LOCATE 17, 32: PRINT USING "##";p%(i, 39)
	LOCATE 17, 36: PRINT USING "##";p%(i, 40)
	LOCATE 17, 40: PRINT USING "##";p%(i, 41)
	LOCATE 17, 49: PRINT USING "###";p%(i, 42)
	LOCATE 17, 54: PRINT USING "###";p%(i, 43)
	LOCATE 18, 17: PRINT USING "###";p%(i, 44)
	LOCATE 18, 27: PRINT USING "###";p%(i, 45)
	LOCATE 18, 32: PRINT USING "##";p%(i, 46)
	LOCATE 18, 36: PRINT USING "##";p%(i, 47)
	LOCATE 18, 40: PRINT USING "##";p%(i, 48)
	LOCATE 18, 49: PRINT USING "###";p%(i, 49)
	LOCATE 18, 54: PRINT USING "###";p%(i, 50)
END SUB

SUB PITCHINIT2 (i, Colr)
	COLOR 0,7
	LOCATE 22, 27: PRINT USING "###";p%(i, 65)
	LOCATE 22, 32: PRINT USING "###";p%(i, 66)
	LOCATE 22, 37: PRINT USING "###";p%(i, 67)
	LOCATE 22, 42: PRINT USING "###";p%(i, 68)
	LOCATE 22, 47: PRINT USING "###";p%(i, 69)
	LOCATE 22, 52: PRINT USING "###";p%(i, 70)
	LOCATE 23, 27: PRINT USING "###";p%(i, 71)
	LOCATE 23, 32: PRINT USING "###";p%(i, 72)
	LOCATE 23, 37: PRINT USING "###";p%(i, 73)
	LOCATE 23, 42: PRINT USING "###";p%(i, 74)
	LOCATE 23, 47: PRINT USING "###";p%(i, 75)
	LOCATE 23, 52: PRINT USING "###";p%(i, 76)
	LOCATE 24, 27: PRINT USING "###";p%(i, 77);
	LOCATE 24, 32: PRINT USING "###";p%(i, 78);
	LOCATE 24, 37: PRINT USING "###";p%(i, 79);
	LOCATE 24, 42: PRINT USING "###";p%(i, 80);
	LOCATE 24, 47: PRINT USING "###";p%(i, 81);
	LOCATE 24, 52: PRINT USING "###";p%(i, 82);
	LOCATE 25, 27: PRINT USING "###";p%(i, 83);
	LOCATE 25, 32: PRINT USING "###";p%(i, 84);
	LOCATE 25, 37: PRINT USING "###";p%(i, 85);
	LOCATE 25, 42: PRINT USING "###";p%(i, 86);
	LOCATE 25, 47: PRINT USING "###";p%(i, 87);
	LOCATE 25, 52: PRINT USING "###";p%(i, 88);
END SUB

FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION

'$INCLUDE: 'QPProEqu.BM'
