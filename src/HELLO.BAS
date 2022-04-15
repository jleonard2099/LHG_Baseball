'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Menu"
$ExeIcon:'./lhg_fcbase.ico'
_Icon
_AllowFullScreen

DefInt A-Z
DECLARE FUNCTION PadRight$ (X$, Length)
DECLARE SUB TITLE ()

Dim D$(3), menuItems$(16)

'CALL INITMOUSE(X)
_MouseShow "DEFAULT"

Open "DEV.BAS" For Input As #1
For I = 0 To 3
    Input #1, D$(I)
Next
Close 1

1 Call TITLE
Erase menuItems$
menuItems$(1) = "PLAY A GAME                     "
menuItems$(2) = "ENTER NEW/CHANGE EXISTING TEAMS "
menuItems$(3) = "PROGRAMMABLE MANAGER            "
menuItems$(4) = "AUTO SCHEDULER/BOXSCORE OPTIONS "
menuItems$(5) = "DRAFT A TEAM                    "
menuItems$(6) = "TRADE PLAYERS                   "
menuItems$(7) = "VIEW/PRINT COMPILED STATS       "
menuItems$(8) = "MODIFY REPLAY STATS             "
menuItems$(9) = "LEAGUE LEADERS AND STANDINGS    "
menuItems$(10) = "LEAGUE UTILITIES                "
menuItems$(11) = "ACTIVATE/DEACTIVATE PLAYERS     "
menuItems$(12) = "GROUNDBALL/FLYOUT CALCS         "
menuItems$(13) = "HARD DRIVE INSTALLATION         "
menuItems$(14) = "CONFIGURE DRIVES FOR TEAMS,STATS"
menuItems$(15) = "EXIT TO DOS                     "
SELECTION% = 1
Count% = 15
Start% = 1
SCANCODE% = 15
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 7
Column% = 25

Call box0(6, 24, 22, 57, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then End
If SELECTION% = 1 Then Run "LOADER.EXE"
If SELECTION% = 2 Then Run "BINPUT.EXE"
If SELECTION% = 3 Then Run "PROGMAN.EXE"
If SELECTION% = 4 Then Run "SCHEDULE.EXE"
If SELECTION% = 5 Then Run "DRAFT.EXE"
If SELECTION% = 6 Then Run "TRADE.EXE"
If SELECTION% = 7 Then Run "LOOKY.EXE"
If SELECTION% = 8 Then Run "SINPUT.EXE"
If SELECTION% = 9 Then 700
If SELECTION% = 10 Then 800
If SELECTION% = 11 Then Run "ACTIVATE.EXE"
If SELECTION% = 12 Then Run "GROUND.EXE"
'IF SELECTION% = 13 THEN RUN "HDCOPY.EXE"
If SELECTION% = 14 Then 505
If SELECTION% = 15 Then Cls: End

307 Return

505 Color 15, 1
FG = 0
BG = 7
For I = 0 To 3
    D$(I) = Left$(D$(I), Len(D$(I)) - 1)
Next
Colr = OneColor%(FG, BG) 'pack FG and BG into a single byte
Cls: Locate , 16: Print "SPECIFY PATHS FOR YOUR TEAMS AND COMPILED STATS": Print

510 Locate , 31: Print "PATHNAME EXAMPLES:": Print

515 Locate , 31: Print "A:"

520 Locate , 31: Print "C:\FULL\TEAMS"
Locate 7, 1: QPrint "Path for Teams ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(0), 30), FG, BG, -1
Locate 9, 1: QPrint "Path for Stats ", 14, 1, -1
Locate , 16: QPrint PadRight$(D$(1), 30), FG, BG, -1
Locate 11, 1: QPrint "Path for Leaders ", 14, 1, -1
Locate , 18: QPrint PadRight$(D$(2), 30), FG, BG, -1
Locate 13, 1: QPrint "Path for Manager Profiles ", 14, 1, -1
Locate , 27: QPrint PadRight$(D$(3), 30), FG, BG, -1

GetDiv1:
Locate 7, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(0), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(0) = _Trim$(D$(0))
GetDiv2:
Locate 9, 16 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(1), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv1: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(1) = _Trim$(D$(1))
GetDiv3:
Locate 11, 18 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(2), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv2: 'can't back up any more, stay here
If XCode = 2 GoTo Done 'all done entering data
D$(2) = _Trim$(D$(2))
GetDiv4:
Locate 13, 27 'location of beginning of field
CapsOn = 0: NumOnly = 0: Max = 30 'specify field parameters
Call TextIn(D$(3), Max, NumOnly, CapsOn, XCode, FG, BG) 'get input
If XCode = 1 GoTo GetDiv3: 'can't back up any more, stay here
D$(3) = _Trim$(D$(3))
Done:
Locate 23, 1
Print "Is this okay? (Yes/No) ";
X$ = ""
Call YesNoB(X$, XCode, FG, BG)
Locate , 1
ClearEOL 14, 1
If XCode = 1 GoTo GetDiv4
If X$ = "N" GoTo GetDiv1
If X$ <> "Y" GoTo Done 'insist on either Y or N or up-arrow

580 Open "DEV.BAS" For Output As #1

585 For R = 0 To 3: D$(R) = D$(R) + "\": Next R

590 Print #1, D$(0)

595 Print #1, D$(1)
Print #1, D$(2)
Print #1, D$(3)

600 Close #1

620 GoTo 1

700 Call TITLE
Erase menuItems$
menuItems$(1) = "CREATE/EDIT A LEAGUE                 "
menuItems$(2) = "UPDATE STANDINGS/LEADERS FOR A LEAGUE"
menuItems$(3) = "EXPANDED LEADERS AND STANDINGS       "
menuItems$(4) = "VIEW RESULTS FOR A LEAGUE            "
menuItems$(5) = "PRINT RESULTS FOR A LEAGUE           "
menuItems$(6) = "DELETE ALL STATS FOR A LEAGUE        "
menuItems$(7) = "RETURN TO FULL COUNT MAIN MENU       "
menuItems$(8) = "EXIT TO DOS                          "
SELECTION% = 1
Count% = 8
Start% = 1
SCANCODE% = 8
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 23
Call box0(7, 22, 16, 60, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "CREATE.EXE"
If SELECTION% = 2 Then Run "LEAGCMPL.EXE"
If SELECTION% = 3 Then Run "NEWLDR.EXE"
If SELECTION% = 4 Then Run "SEELEAG.EXE"
If SELECTION% = 5 Then Run "PRTLEAG.EXE"
If SELECTION% = 6 Then Run "BLANK.EXE"
If SELECTION% = 7 Then 1
If SELECTION% = 8 Then Cls: End

800 Call TITLE
Erase menuItems$
menuItems$(1) = "COMPARE COMPILED STATS VS. ACTUAL"
menuItems$(2) = "MERGE TWO STAT FILES             "
menuItems$(3) = "VIEW TEAM HEAD TO HEAD RESULTS   "
menuItems$(4) = "CAREER FILE OPTIONS              "
menuItems$(5) = "SORT PLAYERS BY POSITION         "
menuItems$(6) = "RETURN TO FULL COUNT MAIN MENU   "
SELECTION% = 1
Count% = 6
Start% = 1
SCANCODE% = 6
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Spacing% = 1
Row% = 8
Column% = 23
Call box0(7, 22, 14, 56, 2, 15, 1)
_MouseShow "DEFAULT"
MMenuVert menuItems$(), SELECTION%, Start%, SCANCODE%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Count%, Row%, Column%
_MouseHide

Color , 0
If SCANCODE% = 27 Then 1
If SELECTION% = 1 Then Run "COMPARE.EXE"
If SELECTION% = 2 Then Run "MERGE.EXE"
If SELECTION% = 3 Then Run "REC.EXE"
If SELECTION% = 4 Then Run "CAREER.EXE"
If SELECTION% = 5 Then Run "POS.EXE"
If SELECTION% = 6 Then 1


'------------------------------
'      PadRight Function
'------------------------------
' Pads string to the right with spaces
Function PadRight$ (X$, Length)
    PadRight$ = X$ + Space$(Length - Len(X$)) 'for display only
End Function


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 30: Color 12
    Print "FULL COUNT BASEBALL 6.51"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1997"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "     FOR TECHNICAL SUPPORT, CALL (800) 477-7032 OR SportGames@AOL.COM"
    Print "      LANCE HAFFNER GAMES ON THE INTERNET AT  HTTP://WWW.LHGAMES.COM"
    Locate 12, 1
    Print "LANCE HAFFNER GAMES"
    Print "P.O. BOX 100594"
    Print "NASHVILLE, TN 37214"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
