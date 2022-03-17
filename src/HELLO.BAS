'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

DEFINT A-Z
DECLARE FUNCTION FnExt$ (X$, Length)
DECLARE SUB TITLE ()

DIM D$(3), ITEM$(15)

'CALL INITMOUSE(X)
_MouseShow "DEFAULT"

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
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 7
	Column% = 25

CALL box0(6, 24, 22, 57, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN END
	 IF SELECTION% = 1 THEN RUN "LOADER.EXE"
	IF SELECTION% = 2 THEN RUN "BINPUT.EXE"
	IF SELECTION% = 3 THEN RUN "PROGMAN.EXE"
	IF SELECTION% = 4 THEN RUN "SCHEDULE.EXE"
	IF SELECTION% = 5 THEN RUN "DRAFT.EXE"
	IF SELECTION% = 6 THEN RUN "TRADE.EXE"
	IF SELECTION% = 7 THEN RUN "LOOKY.EXE"
	IF SELECTION% = 8 THEN RUN "SINPUT.EXE"
	IF SELECTION% = 9 THEN 700
	IF SELECTION% = 10 THEN 800
	IF SELECTION% = 11 THEN RUN "ACTIVATE.EXE"
	IF SELECTION% = 12 THEN RUN "GROUND.EXE"
	IF SELECTION% = 13 THEN RUN "HDCOPY.EXE"
	IF SELECTION% = 14 THEN 505
	IF SELECTION% = 15 THEN CLS : END

307 RETURN

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
	LOCATE , 16: QPrint FnExt$(D$(0), 30), FG, BG, -1
	LOCATE 9, 1: QPrint "Path for Stats ", 30, -1
	LOCATE , 16: QPrint FnExt$(D$(1), 30), FG, BG, -1
	LOCATE 11, 1: QPrint "Path for Leaders ", 30, -1
	LOCATE , 18: QPrint FnExt$(D$(2), 30), FG, BG, -1
	LOCATE 13, 1: QPrint "Path for Manager Profiles ", 30, -1
	LOCATE , 27: QPrint FnExt$(D$(3), 30), FG, BG, -1

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
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 23
CALL box0(7, 22, 16, 60, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "CREATE.EXE"
	IF SELECTION% = 2 THEN RUN "LEAGCMPL.EXE"
	IF SELECTION% = 3 THEN RUN "NEWLDR.EXE"
	IF SELECTION% = 4 THEN RUN "SEELEAG.EXE"
	IF SELECTION% = 5 THEN RUN "PRTLEAG.EXE"
	IF SELECTION% = 6 THEN RUN "BLANK.EXE"
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
	FileFGColr% = 15:FileBGColr% = 1
	HiLiteFGColr% = 0:HiLiteBGColr% = 7
	Spacing% = 1
	Row% = 8
	Column% = 23
CALL box0(7, 22, 14, 56, 2, 31)
_MouseShow "DEFAULT"
MMenuVert VARPTR(ITEM$(1)), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

	COLOR , 0
	IF SCANCODE% = 27 THEN 1
	IF SELECTION% = 1 THEN RUN "COMPARE.EXE"
	IF SELECTION% = 2 THEN RUN "MERGE.EXE"
	IF SELECTION% = 3 THEN RUN "REC.EXE"
	IF SELECTION% = 4 THEN RUN "CAREER.EXE"
	IF SELECTION% = 5 THEN RUN "POS.EXE"
	IF SELECTION% = 6 THEN 1

FUNCTION FnExt$ (X$, Length) 
'Essentially this pads space to the right of the string
	X$ = X$ + SPACE$(Length - LEN(X$))  'for display only
END FUNCTION


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

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
