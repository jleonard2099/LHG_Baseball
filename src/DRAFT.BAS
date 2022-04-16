'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Team Draft"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

DefInt A-Z
DECLARE SUB TITLE ()

Dim L%(13), L1%(13), B%(22, 79), B1%(22, 79), P%(21, 88), P1%(21, 88)
Dim SA%(24), SA1%(24)
Dim TS%(11), TS1%(11), YN$(5)
Dim B$(22), B1$(22), P$(21), P1$(21), Q$(2056), teamItems$(200), V%(200), VV%(200), V$(200)
Dim K9(1) As Long, E6 As Long
Dim D$(2), Y As Long

Open "DEV.BAS" For Input As #1
For I = 0 To 2
    Input #1, D$(I)
Next I
Close #1

10 Cls
For I = 0 To 22
    B$(I) = "XXX"
    B1$(I) = "XXX"
    For I1 = 0 To 79
        B%(I, I1) = 0
        B1%(I, I1) = 0
Next: Next
For I = 0 To 21
    P$(I) = "XXX"
    P1$(I) = "XXX"
    For I1 = 0 To 88
        P%(I, I1) = 0
        P1%(I, I1) = 0
Next: Next
For I = 0 To 11
    TS%(I) = 0
    TS1%(I) = 0
Next
For I = 1 To 24
    SA%(I) = 0
    SA1%(I) = 0
Next

12 Cls: Color 12, 0: Print Tab(31); "DRAFT TEAM UTILITY": Print
Color 14: Print "(0) ";: Color 15: Print "DRAFT A NEW TEAM": Print
Color 14: Print "(1) ";: Color 15: Print "DRAFT TO AN EXISTING TEAM": Print
Color 14: Print "(2) ";: Color 15: Print "RETURN TO GAME MENU"
13 I$ = GetKeyPress$

14 I = Val(I$): If I < 0 Or I > 2 Then 13

15 If I = 0 Then DNT% = 0: GoTo 305

16 If I = 2 Then 1009

17 If I = 1 Then DNT% = 1: GoTo 2000

305 Cls: Print "INPUT TEAM INFORMATION"

315 Locate 2: Input "1. DISK ID "; YN$(0): If C1 = 1 Then 415

325 Locate 3: Input "2. TEAM NAME "; T$: If C1 = 1 Then 415

335 Locate 4: Input "3. LGE BA (WHL #) "; L%(1): If C1 = 1 Then 415

345 Locate 5: Input "4. LGE SO % (WHL #) "; L%(2): If C1 = 1 Then 415

355 Locate 6: Input "5. LGE BB % (WHL #) "; L%(3): If C1 = 1 Then 415

365 Locate 7: Input "6. LGE HR % (WHL #) "; L%(4): If C1 = 1 Then 415

375 Locate 8: Input "7. TM DP/G X 10 "; L%(5): If C1 = 1 Then 415

385 Locate 9: Input "8. PARK DOUBLE ADJUSTMENT (WHL #) "; L%(6): If C1 = 1 Then 415

395 Locate 10: Input "9. PARK TRIPLE ADJUSTMENT (WHL #) "; L%(7): If C1 = 1 Then 415

405 Locate 11: Input "10. PARK HOME RUN ADJUSTMENT (WHL #) "; K9(0): If C1 = 1 Then 415

406 Locate 12: Input "11. PARK FOUL GROUND ADJUSTMENT (WHL #) "; L%(8): If C1 = 1 Then 415

407 Locate 13: Input "12. OUTDOOR = 0 / INDOOR = 1 "; L%(9): If C1 = 1 Then 415

408 Locate 14: Input "13. GRASS = 0 / TURF = 1 "; L%(10): If C1 = 1 Then 415

409 Locate 15: Input "14. STADIUM NAME "; S$: If C1 = 1 Then 415

410 Locate 16: Input "15. PARK ID NUMBER "; L%(13): If C1 = 1 Then 415

411 Locate 17: Input "16. MANAGER NAME "; M$: If C1 = 1 Then 415

412 Locate 18: Input "17. TEAM TEXT COLOR (0-15) "; L%(11): If C1 = 1 Then 415

413 Locate 19: Input "18. TEAM BACKGROUND (0-8) "; L%(12): If C1 = 1 Then 415

414 Locate 20: Input "19. TEAM ABBREVIATION "; NN$: If C1 = 1 Then 415

415 Locate 21, 1: Color L%(11), L%(12): Print "SAMPLE TEXT APPEARANCE": Color 15, 0
C1 = 1: Locate 22: Input "# TO CHANGE (0 TO CONTINUE) "; B9

416 If B9 < 0 Or B9 > 19 Then 415

418 On B9 GOTO 315, 325, 335, 345, 355, 365, 375, 385, 395, 405, 406, 407, 408, 409, 410, 411, 412, 413, 414

41 C1 = 0

50 Cls: Color L%(11), L%(12): Print T$;: Color 15, 0: Print "     (DRAFTING)"

51 Print: Print "BATTERS";: Locate , 40: Print "PITCHERS"
For I = 0 To 10
    Locate I + 5, 1: Print Using "##"; I;: Print " "; B$(I); Tab(20);: Print Using "##"; I + 12;: Print " "; B$(I + 12)
Next
Locate , 1: Print Using "##"; 11;: Print " "; B$(11)
Print
For I = 0 To 10
    Locate I + 5, 40: Print Using "##"; I;: Print " "; P$(I); Tab(60);: Print Using "##"; I + 11;: Print " "; P$(I + 11)
Next

54 Locate 18, 1
Color 14: Print "(1) ";: Color 15: Print "DRAFT BATTER"
Color 14: Print "(2) ";: Color 15: Print "DRAFT PITCHER"
Color 14: Print "(3) ";: Color 15: Print "SAVE DRAFT"
55 I$ = GetKeyPress$
I1 = Val(I$): If I1 < 1 Or I1 > 3 Then 55

56 If I1 = 3 Then 1000

57 If I1 = 1 Then 70

58 Locate 22, 40: Print "PITCHER # (0-21) ";

59 N = 2: Input P

60 If P < 0 Or P > 21 Then 58

61 GoTo 100

70 Locate 21, 1: Print "BATTER # (0-22) ";

71 N = 1: Input B

72 If B < 0 Or B > 22 Then 70

100 Color 15, 0
Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1102: GoTo 182
YN$(1) = NY$
Erase teamItems$
For I = 1 To 200: V%(I) = 1: Next
GoSub 1100

182 If BO% = 1 Then BO% = 0: GoTo 100

183 Locate 2, 33: Color L1%(11), L1%(12): Print T1$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 100
Color 15, 0
GoTo 150

105 Open D$(0) + "FCTEAMS." + YN$(1) For Random As #1 Len = 4342
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
Get #1, D
T1$ = RTrim$(Q$(0))
For I = 1 To 13: L1%(I) = CVI(Q$(I)): Next: E6 = CVS(Q$(14))
For I = 0 To 22: B1$(I) = RTrim$(Q$(I * 43 + 15))
    For I1 = 0 To 41: B1%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
Next: Next
For I = 0 To 21: P1$(I) = RTrim$(Q$(I * 38 + 1004))
    For I1 = 0 To 36: P1%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next
S1$ = RTrim$(Q$(1840))
M1$ = RTrim$(Q$(1841))
NN1$ = RTrim$(Q$(1842))
Close 1
If Not _FileExists(D$(0) + "LR." + YN$(1)) Then 110
Open D$(0) + "LR." + YN$(1) For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
Get #1, D
For X = 0 To 11
    TS1%(X) = CVI(Q$(X + 1))
Next
For X = 0 To 22
    For X1 = 1 To 14
        B1%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P1%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P1%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
Next: Next
For X = 1 To 24
    SA1%(X) = CVI(Q$(X + 950))
Next
For X = 0 To 22
    For X1 = 1 To 24
        B1%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        P1%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
Next: Next
Close 1
If TS1%(0) = 999 Then 110
For X = 0 To 22
    For X1 = 42 To 79
        B1%(X, X1) = 0
Next: Next
For X = 0 To 21
    For X1 = 37 To 88
        P1%(X, X1) = 0
Next: Next
For X = 1 To 24
    SA1%(X) = 0
Next
For X = 0 To 11
    TS1%(X) = 0
Next

110 Return

150 Cls: Color L1%(11), L1%(12): Print T1$: Color 15, 0

151 Print: Color 14: Print "BATTERS";: Locate , 40: Print "PITCHERS": Color 15, 0
For I = 0 To 10
    Locate I + 5, 1: Print Using "##"; I;: Print " "; B1$(I); Tab(20);: Print Using "##"; I + 12;: Print " "; B1$(I + 12)
Next
Locate , 1: Print Using "##"; 11;: Print " "; B1$(11)
Print
For I = 0 To 10
    Locate I + 5, 40: Print Using "##"; I;: Print " "; P1$(I); Tab(60);: Print Using "##"; I + 11;: Print " "; P1$(I + 11)
Next

154 If N = 1 Then 160

155 Locate 21, 40: Print "PITCHER # TO DRAFT ";

156 Input P1

157 If P1 < 0 Or P1 > 21 Then 155

158 GoTo 200

160 Locate 21, 1: Print "BATTER # TO DRAFT ";

161 Input B1

162 If B1 < 0 Or B1 > 22 Then 160

171 B$(B) = B1$(B1)

172 For I = 0 To 79: B%(B, I) = B1%(B1, I): Next I
If TS%(0) = 999 Then 174
If TS1%(0) <> 999 Then 174
For I = 0 To 11
    TS%(I) = TS1%(I)
Next
For I = 1 To 24
    SA%(I) = SA1%(I)
Next

174 GoTo 50

200 P$(P) = P1$(P1)

201 For I = 0 To 88: P%(P, I) = P1%(P1, I): Next I
If TS%(0) = 999 Then 203
If TS1%(0) <> 999 Then 203
For I = 0 To 11
    TS%(I) = TS1%(I)
Next
For I = 1 To 24
    SA%(I) = SA1%(I)
Next

203 GoTo 50

1000 Open D$(0) + "FCTEAMS." + YN$(0) For Random As #1 Len = 4342
Y = LOF(1)
If DNT% = 0 Then T = Y / 4342 + 1
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
LSet Q$(0) = T$: For I = 1 To 13: LSet Q$(I) = MKI$(L%(I)): Next: LSet Q$(14) = MKS$(K9(0))
For I = 0 To 22: LSet Q$(I * 43 + 15) = B$(I)
    For I1 = 0 To 41: LSet Q$(I * 43 + I1 + 16) = MKI$(B%(I, I1))
Next: Next
For I = 0 To 21: LSet Q$(I * 38 + 1004) = P$(I)
    For I1 = 0 To 36: LSet Q$(I * 38 + I1 + 1005) = MKI$(P%(I, I1))
Next: Next
LSet Q$(1840) = S$: LSet Q$(1841) = M$: LSet Q$(1842) = NN$: LSet Q$(1843) = MKI$(999)
Put #1, T: Close 1

1386 SCK = 0
For X = 1 To 24
    SCK = SCK + SA%(X)
Next
If SCK <= 0 Then 1006
TS%(0) = 999
For X = 1 To 11
    TS%(X) = -1
Next
Open D$(0) + "LR." + YN$(0) For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 11: LSet Q$(X + 1) = MKI$(TS%(X)): Next
For X = 0 To 22
    For X1 = 1 To 14
        LSet Q$(12 + X1 + X * 14) = MKI$(B%(X, X1 + 41))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        LSet Q$(334 + X1 + X * 14) = MKI$(P%(X, X1 + 36))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        LSet Q$(642 + X1 + X * 14) = MKI$(P%(X, X1 + 50))
Next: Next
For X = 1 To 24
    LSet Q$(X + 950) = MKI$(SA%(X))
Next
For X = 0 To 22
    For X1 = 1 To 24
        LSet Q$(975 + X1 + X * 24) = MKI$(B%(X, X1 + 55))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        LSet Q$(1528 + X1 + X * 24) = MKI$(P%(X, X1 + 64))
Next: Next
Put #1, T: Close 1

1006 Cls
GoSub 4500
U$ = "DRAFT ANOTHER TEAM (YN)"
Locate , (80 - Len(U$)) / 2
Print U$
Color 15, 0
1009 I$ = GetKeyPress$

1010 If UCase$(I$) = "Y" Then 10

1011 Cls
GoSub 4500

1013 Run "HELLO.EXE"
End

4500 JJ = 2
Locate 6, 8: Color 15, 1
Print " "; Chr$(201); String$(61, 205); Chr$(187); " ";
For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";
Next
Locate 7 + JJ, 8
Print " "; Chr$(200); String$(61, 205); Chr$(188); " ";: Locate 7
Return

1100 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$(1)) Then BO% = 1: GoSub 4500: Return
Open D$(0) + "FCTEAMS." + YN$(1) For Random As #1 Len = 4342
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
D = V%(SELECTION%)
Locate 2, 33: Color 31: Print "LOADING..."
GoTo 105

1200 NT = 0
If Not _FileExists(D$(0) + "FCTEAMS." + YN$(0)) Then BO% = 1: GoSub 4500: Return
Open D$(0) + "FCTEAMS." + YN$(0) For Random As #1 Len = 4342
Y = LOF(1)
Field #1, 15 As Q$(0)
For I1 = 1 To Y / 4342
    Get #1, I1
    For I2 = 1 To 15
        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then 1203
    Next I2
    NT = NT + 1
    teamItems$(NT) = Q$(0): V%(NT) = I1

1203 Next I1
Close 1

1202 SELECTION% = 1
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
GoTo 2500

2000 Color 15, 0: Cls
JJ = 7
Locate 6, 8: Color 15, 4
Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
Locate 7 + JJ, 8
Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";: Locate 7, 32
Locate , 12: Print "ENTER DISK ID: ": Print
Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
Locate 7, 27: Input NY$
If NY$ = "" Then GoSub 1202: GoTo 2001
YN$(0) = NY$
For I = 1 To 200: V$(I) = "": Next
GoSub 1200

2001 If BO% = 1 Then BO% = 0: GoTo 2000

2002 Locate 2, 33: Color L%(11), L%(12): Print T$: Color 15, 4
Locate , 33: Print "ANY CHANGE (YN)";
I$ = GetKeyPress$
If UCase$(I$) = "Y" Then 2000
Color 15, 0
GoTo 50

2500 Open D$(0) + "FCTEAMS." + YN$(0) For Random As #1 Len = 4342
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
For I = 1 To 13
    L%(I) = CVI(Q$(I))
Next
K9(0) = CVS(Q$(14))
For I = 0 To 22: B$(I) = RTrim$(Q$(I * 43 + 15))
    For I1 = 0 To 41: B%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
Next: Next
For I = 0 To 21: P$(I) = RTrim$(Q$(I * 38 + 1004))
    For I1 = 0 To 36: P%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
Next: Next
S$ = RTrim$(Q$(1840))
M$ = RTrim$(Q$(1841))
NN$ = RTrim$(Q$(1842))
CK = CVI(Q$(1843))
Close 1
If Not _FileExists(D$(0) + "LR." + YN$(0)) Then 4000
Open D$(0) + "LR." + YN$(0) For Random As #1 Len = 4108
For X = 0 To 11
    Field #1, X * 2 As X$, 2 As Q$(X + 1), 4108 - 2 - 2 * X As X$
Next
For X = 0 To 22
    For X1 = 1 To 14
        Field #1, 24 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(12 + X1 + X * 14), 4108 - 2 - (24 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 668 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(334 + X1 + X * 14), 4108 - 2 - (668 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 14
        Field #1, 1284 + (X1 - 1) * 2 + X * 14 * 2 As X$, 2 As Q$(642 + X1 + X * 14), 4108 - 2 - (1284 + (X1 - 1) * 2 + X * 14 * 2) As X$
    Next
Next
For X = 1 To 24
    Field #1, 1900 + (X - 1) * 2 As X$, 2 As Q$(950 + X), 4108 - 2 - (X - 1) * 2 - 1900 As X$
Next
For X = 0 To 22
    For X1 = 1 To 24
        Field #1, 1948 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(975 + X1 + X * 24), 4108 - 2 - (1948 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
For X = 0 To 21
    For X1 = 1 To 24
        Field #1, 3052 + (X1 - 1) * 2 + X * 24 * 2 As X$, 2 As Q$(1528 + X1 + X * 24), 4108 - 2 - (3052 + (X1 - 1) * 2 + X * 24 * 2) As X$
    Next
Next
Get #1, T
For X = 0 To 11
    TS%(X) = CVI(Q$(X + 1))
Next
For X = 0 To 22
    For X1 = 1 To 14
        B%(X, X1 + 41) = CVI(Q$(12 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(X, X1 + 36) = CVI(Q$(334 + X1 + X * 14))
Next: Next
For X = 0 To 21
    For X1 = 1 To 14
        P%(X, X1 + 50) = CVI(Q$(642 + X1 + X * 14))
Next: Next
For X = 1 To 24
    SA%(X) = CVI(Q$(X + 950))
Next
For X = 0 To 22
    For X1 = 1 To 24
        B%(X, X1 + 55) = CVI(Q$(975 + X1 + X * 24))
Next: Next
For X = 0 To 21
    For X1 = 1 To 24
        P%(X, X1 + 64) = CVI(Q$(1528 + X1 + X * 24))
Next: Next
Close 1
If TS%(0) = 999 Then 4000
For X = 0 To 22
    For X1 = 42 To 79
        B%(X, X1) = 0
Next: Next
For X = 0 To 21
    For X1 = 37 To 88
        P%(X, X1) = 0
Next: Next
For X = 1 To 24
    SA%(X) = 0
Next
For X = 0 To 11
    TS%(X) = 0
Next

4000 Return

Return

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
