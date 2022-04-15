'$INCLUDE: 'Config.BI'
'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Create Position Search"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim K9(1) As Long, D$(4), Q$(1844), FS%(22, 4, 4)
Dim B%(22, 41), P%(21, 36), B$(22), P$(21), L%(13), BS$(22), PS$(21), B0%(22, 40)
Dim P0%(21, 41), BS%(22), PS%(32), T(22), T1(22), N$(22), NP$(21)
Dim TM(40), T$(40), TYN$(40), teamItems$(200), V%(200)
Dim Y As Long
For I = 1 To 40
    TM(I) = 0
    T$(I) = ""
    TYN$(I) = ""
Next

Open "DEV.BAS" For Input As #2
For I = 0 To 3
    Input #2, D$(I)
Next
16 Close #2

21 Cls

30 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 1
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER NAME FOR POSITION FILE: ": Print
Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
Locate , 12: Print "WILL AUTOMATICALLY ADD .POS"
Locate , 12: Print "SAMPLE NAMES ARE AL-1965, DRAFT, ETC"
Locate 7, 41: Input LGE$
If LGE$ = "" Then 21
Locate 13, 12: Print "ANY CHANGE (YN) "
22 I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 30
Color 15, 0

36 Cls

50 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " Ö"; String$(61, 196); "· ";: For II = 1 To JJ: Locate 6 + II, 8
Print " º"; String$(61, 32); "º ";: Next
Locate 7 + JJ, 8
Print " Ó"; String$(61, 196); "½ ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$ = NY$
Erase teamItems$

1441 GoSub 1105

182 If BO% = 1 Then BO% = 0: GoTo 50

183 Locate 2, 33: Color 15, 0: Color L%(11), L%(12): Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 50

112 J = J + 1

200 TM(J) = T: T$(J) = T$: TYN$(J) = YN$

300 Color 15, 0
Cls
Locate 7: Color 12: Print "1.  ";: Color 7: Print T$(1);
Locate , 27: Color 12: Print "2.  ";: Color 7: Print T$(2);
Locate , 54: Color 12: Print "3.  ";: Color 7: Print T$(3)
Locate 8: Color 12: Print "4.  ";: Color 7: Print T$(4);
Locate , 27: Color 12: Print "5.  ";: Color 7: Print T$(5);
Locate , 54: Color 12: Print "6.  ";: Color 7: Print T$(6)
Locate 9: Color 12: Print "7.  ";: Color 7: Print T$(7);
Locate , 27: Color 12: Print "8.  ";: Color 7: Print T$(8);
Locate , 54: Color 12: Print "9.  ";: Color 7: Print T$(9)
Locate 10: Color 12: Print "10. ";: Color 7: Print T$(10);
Locate , 27: Color 12: Print "11. ";: Color 7: Print T$(11);
Locate , 54: Color 12: Print "12. ";: Color 7: Print T$(12)
Locate 11: Color 12: Print "13. ";: Color 7: Print T$(13);
Locate , 27: Color 12: Print "14. ";: Color 7: Print T$(14);
Locate , 54: Color 12: Print "15. ";: Color 7: Print T$(15)
Locate 12: Color 12: Print "16. ";: Color 7: Print T$(16);
Locate , 27: Color 12: Print "17. ";: Color 7: Print T$(17);
Locate , 54: Color 12: Print "18. ";: Color 7: Print T$(18)
Locate 13: Color 12: Print "19. ";: Color 7: Print T$(19);
Locate , 27: Color 12: Print "20. ";: Color 7: Print T$(20);
Locate , 54: Color 12: Print "21. ";: Color 7: Print T$(21)
Locate 14: Color 12: Print "22. ";: Color 7: Print T$(22);
Locate , 27: Color 12: Print "23. ";: Color 7: Print T$(23);
Locate , 54: Color 12: Print "24. ";: Color 7: Print T$(24)
Locate 15: Color 12: Print "25. ";: Color 7: Print T$(25);
Locate , 27: Color 12: Print "26. ";: Color 7: Print T$(26);
Locate , 54: Color 12: Print "27. ";: Color 7: Print T$(27)
Locate 16: Color 12: Print "28. ";: Color 7: Print T$(28);
Locate , 27: Color 12: Print "29. ";: Color 7: Print T$(29);
Locate , 54: Color 12: Print "30. ";: Color 7: Print T$(30)
Locate 17: Color 12: Print "31. ";: Color 7: Print T$(31);
Locate , 27: Color 12: Print "32. ";: Color 7: Print T$(32);
Locate , 54: Color 12: Print "33. ";: Color 7: Print T$(33)
Locate 18: Color 12: Print "34. ";: Color 7: Print T$(34);
Locate , 27: Color 12: Print "35. ";: Color 7: Print T$(35);
Locate , 54: Color 12: Print "36. ";: Color 7: Print T$(36)
Locate 19: Color 12: Print "37. ";: Color 7: Print T$(37);
Locate , 27: Color 12: Print "38. ";: Color 7: Print T$(38);
Locate , 54: Color 12: Print "39. ";: Color 7: Print T$(39)
Locate 20: Color 12: Print "40. ";: Color 7: Print T$(40)

500 Locate 22: Print "ENTER ANOTHER TEAM (Y/N)"
501 I$ = GetKeyPress$

502 If UCase$(I$) = "Y" Then 50

550 Open D$(0) + LGE$ + ".POS" For Output As #2
551 For J = 1 To 40: Write #2, TM(J), T$(J), TYN$(J): Next
552 Close #2

605 Run "POS.EXE"

4500 Locate 10, 8
Call CSMsgBox("NO TEAM FILE FOUND MATCHING ID... PRESS ANY KEY TO RETURN TO MAIN MENU", 26, Pass)
I$ = GetKeyPress$
Call CSMsgBox("", 0, Pass)
GoTo 605

1105 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$) Then BO% = 1: Return
Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1103
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1103 Next I1
Close 1

1102 SELECTION% = 1
Count% = NT
Start% = 1
FileFGColr% = 15: FileBGColr% = 4
HiLiteFGColr% = 0: HiLiteBGColr% = 7
Rows% = 20
Columns% = 4
Spacing% = 6
Row% = 4
Column% = 1
Color , 4: Cls: Locate , 33: Color 14: Print "TEAM SELECTION": Color 15
Locate 25, 1: Color 14: Print "ESC";: Color 15: Print "-SELECT NEW ID  PGDN  PGUP  ";: Color 14: Print "ENTER";: Color 15: Print "-SELECTS TEAM";

_MouseShow "DEFAULT"
MAMenu teamItems$(), SELECTION%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
_MouseHide

If ExitCode% = 27 Then BO% = 1: Return
T = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."

5500 Open D$(0) + "FCTEAMS." + YN$ For Random As #1 Len = 4342
Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$
For I = 0 To 22
    Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
    Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
    Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
    Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
Next

For I = 0 To 21
FIELD #1, 2322 + I * 89 AS X$, 15 AS Q$(I * 38 + 1004), 2 AS Q$(I * 38 + 1005), 2 AS Q$(I * 38 + 1006), 2 AS Q$(I * 38 + 1007), 2 AS Q$(I * 38 + 1008), 2 AS Q$(I * 38 + 1009), 2 AS Q$(I * 38 + 1010), 2 AS Q$(I * 38 + 1011), 2 AS Q$(I * 38 + 1012), 2 _
AS Q$(I * 38 + 1013), 2 AS Q$(I * 38 + 1014), 2 AS Q$(I * 38 + 1015), 2020 - 37 - I * 89 AS X$
FIELD #1, 2359 + I * 89 AS X$, 2 AS Q$(I * 38 + 1016), 2 AS Q$(I * 38 + 1017), 2 AS Q$(I * 38 + 1018), 2 AS Q$(I * 38 + 1019), 2 AS Q$(I * 38 + 1020), 2 AS Q$(I * 38 + 1021), 2 AS Q$(I * 38 + 1022), 2 AS Q$(I * 38 + 1023), 2 AS Q$(I * 38 + 1024), 2  _
AS Q$(I * 38 + 1025), 2 AS Q$(I * 38 + 1026), 2 AS Q$(I * 38 + 1027), 2020 - 61 - I * 89 AS X$
FIELD #1, 2383 + I * 89 AS X$, 2 AS Q$(I * 38 + 1028), 2 AS Q$(I * 38 + 1029), 2 AS Q$(I * 38 + 1030), 2 AS Q$(I * 38 + 1031), 2 AS Q$(I * 38 + 1032), 2 AS Q$(I * 38 + 1033), 2 AS Q$(I * 38 + 1034), 2 AS Q$(I * 38 + 1035), 2 AS Q$(I * 38 + 1036), 2  _
AS Q$(I * 38 + 1037), 2 AS Q$(I * 38 + 1038), 2 AS Q$(I * 38 + 1039), 2020 - 85 - I * 89 AS X$
    Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
Next

Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)
Get #1, T
T$ = RTrim$(Q$(0))

For I = 1 To 13: L%(I) = CVI(Q$(I)): Next: K9(0) = CVS(Q$(14))

For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
    For I1 = 0 To 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
Next: Next

For I = 0 To 21: P$(I) = RTrim$(Q$(I * 38 + 1004))
    For I1 = 0 To 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next

S$ = RTrim$(Q$(1840)): M$ = RTrim$(Q$(1841)): NN$ = RTrim$(Q$(1842)): CK = CVI(Q$(1843))
Close 1
Return


'------------------------------
'      TITLE Subroutine
'------------------------------
' Displays window title / game copyright
Sub TITLE ()
    Color , 0: KEY Off: Cls
    Color , 1: Locate 1, 1: Print Tab(80);: Locate , 30: Color 12
    Print "FULL COUNT BASEBALL 6.0"
    Locate 24, 1: Print Tab(80);: Locate , 1
    Print "(C) LANCE HAFFNER/SHANNON LYNN 1987-1994"; Tab(70); Left$(Time$, 5);
    Locate 3, 1: Color 2, 0
    Color 15: Print "        PROGRAMMED AND DEVELOPED BY LANCE HAFFNER AND SHANNON LYNN"
    Print "              FOR TECHNICAL SUPPORT, CALL (615) 366-8088"
    Color 10
End Sub

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
