'$INCLUDE: 'KeyInput.BI'
'$INCLUDE: 'QPProEqu.BI'

_Title "Full Count Baseball - Align Stats"
''$ExeIcon:'./lhg_fcbase.ico'
'_Icon
_AllowFullScreen

3 Cls

4 Dim B$(23), P$(22), B0%(23, 22), P0%(22, 42), T(22), T1(23)

5 Dim P%(22), B%(23)

6 Dim BR$(23), PR$(22), B0R%(23, 22), P0R%(22, 42), TR(22), TR1(23)

7 Dim PR%(22), BR%(23), S0R%(22, 6), S0N%(22, 6), S0%(22, 6)

8 Dim BN$(23), PN$(22), B0N%(23, 22), P0N%(22, 42), TN(22), TN1(23)

9 Dim PN%(22), BN%(23)

12 Color 12, 0: Cls: Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO ALIGN"

13 Color 15: Locate 4, 5: Print "EXAMPLE - A:"

14 Locate , 5: Print "          C:\HAFFNER"

15 Locate , 5: Print "          C:\HAFFNER\1960"

16 Locate , 5: Print "          D:\BASEBALL"

20 For J = 10 To 13: Locate J

21 Print "                                                              ": Next

25 Color 10: Locate 10, 5: Line Input "ROAD STATS:     "; PR$

26 Locate , 5: Line Input "CURRENT STATS:  "; PC$

27 Color 14: Locate 13, 5: Print "ANY CHANGE (Y/N)"
28 I$ = GetKeyPress$

29 If UCase$(I$) = "Y" Then 20

45 Locate 15, 5: Print "                                                       "

46 Locate 16, 5: Print "                                                       "

47 Color 11: Locate 15, 5: Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.61)"; Z$

50 Locate , 5: Print "ANY CHANGE (YN)"
60 I$ = GetKeyPress$

70 If UCase$(I$) = "Y" Then 45
If Len(Z$) > 11 Then Z$ = Left$(Z$, 8) + Right$(Z$, 3)

80 Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

110 If Not _FileExists(PR$ + "\" + Z$) Then 950

111 Cls
Color 15

120 Open PR$ + "\" + Z$ For Input As #1

121 Input #1, AR$: Input #1, W2R%: Input #1, L2R%

122 For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I

123 For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I

124 For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I

125 For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

126 Input #1, DR0%: Input #1, DR1%
For I = 0 To 21: Input #1, TR(I): Next I
For I = 0 To 22: Input #1, TR1(I): Next I

160 Close #1
If Not _FileExists(PR$ + "\" + Z$ + "R") Then 210
Open PR$ + "\" + Z$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
        Input #1, S0R%(I, I1)
Next: Next
Close #1

210 If Not _FileExists(PC$ + "\" + Z$) Then 950

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
If Not _FileExists(PC$ + "\" + Z$ + "R") Then 400
Open PC$ + "\" + Z$ + "R" For Input As #1
For I = 0 To 22
    For I1 = 0 To 6
        Input #1, S0%(I, I1)
Next: Next
Close #1

400 GoTo 1000

404 N = 0

405 For J = 0 To 22

    410 If UCase$(B$(N)) = UCase$(BR$(J)) Then 420 Else GoTo 423

    420 BN$(N) = BR$(J): BN%(N) = BR%(J)

    421 For K = 0 To 21
        B0N%(N, K) = B0R%(J, K)

    422 Next K
    For K = 0 To 6
        S0N%(N, K) = S0R%(J, K)
    Next K

423 Next J

424 N = N + 1

425 If N = 23 GoTo 500 Else GoTo 405

500 N = 0

505 For J = 0 To 21

    510 If UCase$(P$(N)) = UCase$(PR$(J)) Then 520 Else GoTo 523

    520 PN$(N) = PR$(J): PN%(N) = PR%(J)

    521 For K = 0 To 41: P0N%(N, K) = P0R%(J, K)

    522 Next K

523 Next J

524 N = N + 1

525 If N = 22 GoTo 820 Else GoTo 505

820 KKK = 1: GoTo 1300

824 Open PR$ + "\" + Z$ For Output As #1

825 Write #1, AR$: Write #1, W2R%: Write #1, L2R%

832 For I = 0 To 22: Write #1, BN$(I): Write #1, BN%(I): Next I

833 For I = 0 To 21: Write #1, PN$(I): Write #1, PN%(I): Next I

834 For I = 0 To 22: For I1 = 0 To 21: Write #1, B0N%(I, I1): Next I1: Next I

835 For I = 0 To 21: For I1 = 0 To 41: Write #1, P0N%(I, I1): Next I1: Next I

836 Write #1, DR0%: Write #1, DR1%
For I = 0 To 21: Write #1, TR(I): Next I
For I = 0 To 22: Write #1, TR1(I): Next I

837 Close #1
Open PR$ + "\" + Z$ + "R" For Output As #1
For I = 0 To 22
    For I1 = 0 To 6
        Print #1, S0N%(I, I1)
    Next
Next
Close 1

880 Cls: Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"

881 Print: Locate 12, 20: Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

900 Cls

910 Run "MERGE.EXE"

950 Cls: Color 15: Locate 2, 10: Color 7
Print "NON-EXISTANT PATH OR INCORRECT FILE NAME"

951 Print
Locate , 10
Print "TRY AGAIN (YN) ";
952 I$ = GetKeyPress$
If UCase$(I$) <> "Y" Then 900
Print "HIT ENTER TO RE-ENTER INFORMATION"
955 I$ = GetKeyPress$
GoTo 12

1000 Cls: Locate 1, 34: Color 12: Print "STAT ROSTERS"

1001 Locate , 33: Color 15: Print "CURRENT  ";: Color 7: Print " ROAD"

1002 For I = 0 To 22: Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(B$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BR$(I), 10);: Next
For I = 0 To 21: Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(P$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PR$(I), 10);: Next

1003 Color 11: Locate 7, 33: Print "HIT ANY KEY TO"

1004 Locate 8, 37: Print "ALIGN"
1005 I$ = GetKeyPress$

1006 GoTo 1209

1010 Cls: Locate 1, 34: Color 12: Print "STAT ROSTERS"

1011 Locate , 33: Color 15: Print "CURRENT  ";: Color 7: Print " ROAD"

1012 For I = 0 To 22: Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(B$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BN$(I), 10);: Next
For I = 0 To 21: Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(P$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PN$(I), 10);: Next

1013 Color 11: Locate 7, 33: Print "HIT ANY KEY TO"

1014 Locate 8, 36: Print "CONTINUE"
1015 I$ = GetKeyPress$

1016 GoTo 824

1209 Color 11: Locate 7, 33: Print "ALIGNING FILES"

1210 Locate 8, 37: Print "       "

1211 GoTo 404

1300 SEQ = 0

1301 For J = 0 To 22

    1303 If UCase$(BN$(J)) = UCase$(B$(J)) Then 1309 Else GoTo 1304

    1304 SEQ = SEQ + 1

1309 Next J

1310 For J = 0 To 21

    1312 If UCase$(PN$(J)) = UCase$(P$(J)) Then 1319 Else GoTo 1314

    1314 SEQ = SEQ + 1

1319 Next J

1320 If SEQ = 0 Then 1010 Else GoTo 1400

1400 Cls: Locate 5: Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"

1402 Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."

1405 Locate 23, 30: Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

1406 GoTo 900

'$INCLUDE: 'KeyInput.BM'
'$INCLUDE: 'QPProEqu.BM'
