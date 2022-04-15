'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Umpire Manager"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

10 Dim U$(50), U%(50), D$(2)

15 Open "DEV.BAS" For Input As #1

16 For I = 0 To 2
    Input #1, D$(I)
Next

17 Close #1

20 Cls: Color 12, 0: Print Tab(33); "UMPIRE MANAGEMENT": Print: Print
Color 14: Print "(1) ";: Color 15: Print "ENTER A NEW UMPIRE FILE": Print
Color 14: Print "(2) ";: Color 15: Print "EDIT EXISTING UMPIRE FILE": Print
Color 14: Print "(3) ";: Color 15: Print "RETURN TO MAIN MENU"
30 I$ = GetKeyPress$

40 I = Val(I$): If I < 1 Or I > 3 Then 20

50 If I = 3 Then 500

60 If I = 2 Then 300

100 For X = 1 To 50

    110 Cls
    Print "UMPIRE #"; X
    Input "ENTER UMPIRE NAME (END TO QUIT) "; U$(X)

    120 If UCase$(U$(X)) = "END" Then For I = X To 50: U$(I) = "": Next: GoTo 150
    Input "ENTER RATING (0-9) "; U%(X)

130 Next

150 Cls
Print "IS THIS UMPIRING CREW (1)-AL OR (2)-NL ??"
160 I$ = GetKeyPress$

170 I = Val(I$): If I < 1 Or I > 2 Then 150

175 If I = 1 Then LG$ = "AL": GoTo 180

176 LG$ = "NL"

180 Open D$(0) + "UMPIRES." + LG$ For Output As #1

190 For X = 1 To 50: Print #1, U$(X): Print #1, U%(X): Next: Close #1

200 For X = 1 To 50: U$(X) = "": U%(X) = 5: Next: GoTo 20

300 Cls
Print "IS THIS UMPIRING CREW (1)-AL OR (2)-NL ??"
301 I$ = GetKeyPress$
I = Val(I$): If I < 1 Or I > 2 Then 301
If I = 1 Then LG$ = "AL"
If I = 2 Then LG$ = "NL"
If Not _FileExists(D$(0) + "UMPIRES." + LG$) Then 315
Open D$(0) + "UMPIRES." + LG$ For Input As #1
For X = 1 To 50: Input #1, U$(X): Input #1, U%(X): Next: Close #1

315 Cls
For I = 1 To 10
    Locate I + 5, 1: Print Using "##"; I;: Print " "; U$(I);
    Locate , 17: Print Using "##"; I + 10;: Print " "; U$(I + 10);
    Locate , 33: Print Using "##"; I + 20;: Print " "; U$(I + 20);
    Locate , 50: Print Using "##"; I + 30;: Print " "; U$(I + 30);
    Locate , 67: Print Using "##"; I + 40;: Print " "; U$(I + 40)
Next

302 Locate 18, 5: Print "SELECT UMPIRE TO EDIT (0 TO QUIT) ";
Input I
If I < 0 Or I > 50 Then 302
If I = 0 Then 180
Cls
Locate 1, 30: Print "UMPIRE-"; I: Locate , 1
Locate 1: Print "1. NAME "; U$(I)
Locate 2: Print "2. RATING "; U%(I)
C1 = 1: GoTo 310

305 Locate 1: Input "1. NAME "; U$(I): If C1 = 1 Then 310

306 Locate 2: Input "2. RATING "; U%(I): If C1 = 1 Then 310

310 C1 = 1: Locate 5: Input "# TO CHANGE (0 TO CONTINUE) "; B9
If B9 < 1 Or B9 > 2 Then 315
On B9 GOTO 305, 306

500 Run "HELLO.EXE"

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
