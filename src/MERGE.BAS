'$INCLUDE: 'KeyInput.BI'

_Title "Full Count Baseball - Merge Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

4 Cls

5 Cls: Color 12: Print "STATISTICS MERGE/ALIGN MENU": Print

6 Color 14: Print "(1) ";: Color 15: Print "MERGE STAT FILES": Print

7 Color 14: Print "(2) ";: Color 15: Print "ALIGN STAT FILES": Print

8 Color 14: Print "(3) ";: Color 15: Print "RETURN TO UTILITIES MENU"

9 I$ = GetKeyPress$
10 I = Val(I$): If I < 1 Or I > 3 Then 9

11 If I = 1 Then GoTo 16

12 If I = 2 Then Run "ALIGN.EXE"

13 If I = 3 Then GoTo 990

14 Dim B$(22), P$(21), B0%(22, 22), P0%(21, 41), T(22), T1(22)

15 Dim H1$(40), P%(21), B%(22), H%(162), H$(162), V%(162), PL$(162), W$(162), L$(162), D$(2), I%(162)

16 Dim BR$(22), PR$(21), B0R%(22, 22), P0R%(21, 41), TR(22), TR1(22)

17 Dim H1R$(40), PR%(21), BR%(22), HR%(162), HR$(162), VR%(162), PLR$(162), WR$(162), LR$(162), DR$(2), IR%(162)
Dim S0%(22, 6), SR%(22, 6)

22 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"

23 Color 15: Locate 4, 5: Print "EXAMPLE - A:"

24 Locate , 5: Print "          C:\HAFFNER"

25 Locate , 5: Print "          C:\HAFFNER\1960"

26 Locate , 5: Print "          D:\BASEBALL"

27 For J = 10 To 13: Locate J

28 Print "                                                            ": Next

35 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

36 Locate , 5: Locate , 5: Line Input "CURRENT STATS:  "; PC$

37 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"

38 I$ = GetKeyPress$
39 If UCase$(I$) = "Y" Then 27

45 Locate 15, 5: Print "                                                       "

46 Locate 16, 5: Print "                                                       "

47 Color 11: Locate 15, 5: Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.61)"; Z$

50 Locate , 5: Print "ANY CHANGE (YN)"

60 I$ = GetKeyPress$
70 If UCase$(I$) = "Y" Then 45
If Len(Z$) > 11 Then Z$ = Left$(Z$, 8) + Right$(Z$, 3)

100 Rem

110 If Not _FileExists(PR$ + "\" + Z$) Then 900

120 Open PR$ + "\" + Z$ For Input As #1

121 Input #1, A$: Input #1, WR2%: Input #1, LR2%

122 For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I

123 For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I

124 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I

125 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

126 Input #1, D0R%: Input #1, D1R%
For I = 0 To 21: Input #1, TR(I): Next I
For I = 0 To 22: Input #1, TR1(I): Next I

160 Close #1
If Not _FileExists(PR$ + "\" + Z$ + "R") Then 165
Open PR$ + "\" + Z$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
        Input #1, SR%(I, I1)
Next: Next
Close 1

165 Open PR$ + "\" + Z$ + "S" For Input As #1
Input #1, YR%
For I = 1 To 162: Input #1, HR%(I): Next I
For I = 1 To 162: Input #1, HR$(I): Next I
For I = 1 To 162: Input #1, VR%(I): Next I
For I = 1 To 162: Input #1, PLR$(I): Next I
For I = 1 To 162: Input #1, WR$(I): Next I
For I = 1 To 162: Input #1, LR$(I): Next I
For I = 1 To 162: Input #1, IR%(I): Next I
Close #1

210 If Not _FileExists(PC$ + "\" + Z$) Then 900

220 Open PC$ + "\" + Z$ For Input As #1

221 Input #1, A$: Input #1, W2%: Input #1, L2%

222 For I = 0 To 22: Input #1, B$(I): Input #1, B%(I): Next I

223 For I = 0 To 21: Input #1, P$(I): Input #1, P%(I): Next I

224 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I

225 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

226 Input #1, D0%: Input #1, D1%
For I = 0 To 21: Input #1, T(I): Next I
For I = 0 To 22: Input #1, T1(I): Next I

260 Close #1
If Not _FileExists(PC$ + "\" + Z$ + "R") Then 265
Open PC$ + "\" + Z$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
        Input #1, S0%(I, I1)
Next: Next
Close 1

265 Open PC$ + "\" + Z$ + "S" For Input As #1
Input #1, Y%
For I = 1 To 162: Input #1, H%(I): Next I
For I = 1 To 162: Input #1, H$(I): Next I
For I = 1 To 162: Input #1, V%(I): Next I
For I = 1 To 162: Input #1, PL$(I): Next I
For I = 1 To 162: Input #1, W$(I): Next I
For I = 1 To 162: Input #1, L$(I): Next I
For I = 1 To 162: Input #1, I%(I): Next I
Close #1
GoTo 1200

319 W2% = W2% + WR2%: L2% = L2% + LR2%

320 Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

324 For I = 0 To 22
    For I1 = 0 To 6
        S0%(I, I1) = S0%(I, I1) + SR%(I, I1)
    Next

    325 For I1 = 0 To 11: B0%(I, I1) = B0%(I, I1) + B0R%(I, I1): Next I1

326 For I1 = 14 To 21: B0%(I, I1) = B0%(I, I1) + B0R%(I, I1): Next I1: Next I

327 For I = 0 To 21

    328 For I1 = 0 To 34: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1

329 For I1 = 37 To 41: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1: Next I

330 D0% = D0% + DR0%: D1% = D1% + DR1%

350 For I = 0 To 21: T(I) = T(I) + TR(I): Next

355 For I = 0 To 22: T1(I) = T1(I) + TR1(I): Next

420 Open PC$ + "\" + Z$ For Output As #1

421 Write #1, A$: Write #1, W2%: Write #1, L2%

422 For I = 0 To 22: Write #1, B$(I): Write #1, B%(I): Next I

423 For I = 0 To 21: Write #1, P$(I): Write #1, P%(I): Next I

424 For I = 0 To 22: For I1 = 0 To 21: Write #1, B0%(I, I1): Next I1: Next I

425 For I = 0 To 21: For I1 = 0 To 41: Write #1, P0%(I, I1): Next I1: Next I

426 Write #1, D0%: Write #1, D1%
For I = 0 To 21: Write #1, T(I): Next I
For I = 0 To 22: Write #1, T1(I): Next I

460 Close #1
Open PC$ + "\" + Z$ + "R" For Output As #1
For I = 0 To 22
    For I1 = 0 To 6
        Write #1, S0%(I, I1)
Next: Next
Close 1

495 GoTo 800

700 For K = 1 To YR%

    701 Y% = Y% + 1

    705 H%(Y%) = HR%(K)

    710 H$(Y%) = HR$(K)

    715 V%(Y%) = VR%(K)

    720 PL$(Y%) = PLR$(K)

    725 W$(Y%) = WR$(K)

    730 L$(Y%) = LR$(K)

    735 I%(Y%) = IR%(K)

750 Next

790 GoTo 319

800 Open PC$ + "\" + Z$ + "S" For Output As #1

804 Write #1, Y%

805 For I = 1 To 162: Write #1, H%(I): Next I

806 For I = 1 To 162: Write #1, H$(I): Next I

807 For I = 1 To 162: Write #1, V%(I): Next I

808 For I = 1 To 162: Write #1, PL$(I): Next I

809 For I = 1 To 162: Write #1, W$(I): Next I

810 For I = 1 To 162: Write #1, L$(I): Next I

811 For I = 1 To 162: Write #1, I%(I): Next I

812 Close #1

850 Y = 0

860 Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

861 For J = 1 To 5000: Next J

875 GoTo 4

900 Cls
Locate 2, 10
Color 7
Print "NON-_FILEEXISTSANT PATH OR INCORRECT FILE NAME"

951 Print
Locate , 10
Print "TRY AGAIN (YN) ";

952 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then 990
Print "HIT ENTER TO RE-ENTER INFORMATION"

955 I$ = GetKeyPress$
GoTo 12

990 Run "HELLO.EXE"

1200 SEQ = 0

1201 For K = 0 To 22

    1202 If UCase$(BR$(K)) = UCase$(B$(K)) Then 1220 Else GoTo 1210

    1210 SEQ = SEQ + 1

1220 Next

1230 For K = 0 To 21

    1231 If UCase$(PR$(K)) = UCase$(P$(K)) Then 1250 Else GoTo 1240

    1240 SEQ = SEQ + 1

1250 Next

1260 If SEQ = 0 Then 700 Else GoTo 1270

1270 Cls: Color 14: Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$

1275 Color 15: Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"

1276 Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"

1277 Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."

1278 Locate 11, 24: Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"

1279 Locate , 31: Print "ENTER STATS MANUALLY."

1280 Locate 15, 29: Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

1290 GoTo 5

'$INCLUDE: 'KeyInput.BM'
