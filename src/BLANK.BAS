'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Delete League Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z

Dim O1(40), TM$(40), JS(46, 42) As Single, ZM%(46), NM$(46)
Dim H1$(40), PS$(40), D$(2)
Dim BA$(40), DFT(40, 20) As Single, ER$(40), OFT(40, 20) As Single
Dim O2(40), Z2$(255), Z3$(255), Z1(255, 2) As Single, BAT(24), PIT(23)
Dim J As Single

4 Open "DEV.BAS" For Input As #2
For I = 0 To 2
    Input #2, D$(I)
Next
Close 2

5 Cls

APP = 0

4000 Spec$ = D$(2) + "*.LIB"
null$ = READDIR$(Spec$) 'use a file spec ONCE to find the last file name listed
Count% = DirCount%
If Count% = 0 Then 4500
ReDim libFiles$(0 To Count%)
For X = 1 To Count%
    libFiles$(X) = Space$(12)
Next
libFiles$(0) = Spec$
Call ReadFile(libFiles$())
'CALL SortStr(VARPTR(libFiles$(1)), Count%, 0)

1310 Cls
Color , 1
Cls
Locate , 28
Color 14
Print "SELECT LEAGUE TO ERASE": Color 15
SELECTION% = 1
Start% = 1
FileFGColr% = 15: FileBGColr% = 1
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Locate 25, 1
Color 14
Print "ENTER";: Color 15
Print "-SELECTS LEAGUE   ";
Color 14
Print "HIT ESCAPE KEY TO EXIT";
Color 15

_MouseShow "DEFAULT"
MAMenu libFiles$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then 103
targetFile$ = libFiles$(SELECTION%)
Locate 2, 33: Print targetFile$
Locate , 33: Print "ANY CHANGE (YN)";
Color 15, 0
3910 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 1310

3911 targetFile$ = RTrim$(targetFile$)
WE$ = Left$(targetFile$, Len(targetFile$) - 4)
LH$ = Left$(WE$, 8)

15 Cls
Locate 10, 8
Call CSMsgBox("WARNING!!!::: THIS WILL ERASE ALL COMPILED STATS AND LEAGUE FILES FOR THIS LEAGUE. IF YOU STILL WISH TO ERASE ALL LEAGUE FILES AND STATS, HIT 'Y' FOR YES", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
If I$ <> "Y" Then 1310

30 Open D$(2) + LH$ + ".TMS" For Input As #2
Input #2, LG$, D1$, D2$, D3$, D4$
For XW = 1 To 40

    35 Input #2, H1$(XW)
Next
Close 2

71 For XW = 1 To 40
    If H1$(XW) = Chr$(42) Then 74

    801 Print "DELETING "; H1$(XW); " STATS"
    Kill (D$(1) + H1$(XW))
    Kill (D$(1) + H1$(XW) + "R")
    Kill (D$(1) + H1$(XW) + "S")
    Kill (D$(1) + H1$(XW) + "D")

74 Next
Cls
GoTo 4000

Cls

103 Run "HELLO.EXE"

4500 Locate 10, 8
Call CSMsgBox("NO LEAGUE FILES WERE FOUND PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 103


'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
