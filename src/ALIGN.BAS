Sub ALIGN ()

    _Title "Full Count Baseball - Align Stats"

    Dim B$(23), BN$(23), BR$(23)
    Dim P$(22), PN$(22), PR$(22)

    Dim B0%(23, 22), B0N%(23, 22), B0R%(23, 22)
    Dim B%(23), BN%(23), BR%(23)
    Dim P0%(22, 42), P0N%(22, 42), P0R%(22, 42)
    Dim P%(22), PN%(22), PR%(22)
    Dim S0%(22, 6), S0N%(22, 6), S0R%(22, 6)

    Dim TR(22), TR1(23)
    'Dim TN(22), TN1(23)

    'Dim T%(0 to 21), T1%(0 to 22)
    Shared T%(), T1%()

	Do

		Color 12, 0
		Cls
		
		Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO ALIGN"
		Color 15
		Locate 4, 5: Print "EXAMPLE - A:"
		Locate , 5: Print "          C:\HAFFNER"
		Locate , 5: Print "          C:\HAFFNER\1960"
		Locate , 5: Print "          D:\BASEBALL"

		For J = 10 To 13
			Locate J
			Print Space$(62)
		Next

		Color 10

		Do

			Locate 10, 5: Line Input "ROAD STATS:     "; PR$
			Locate , 5: Line Input "CURRENT STATS:  "; PC$

			Color 14

			Locate 13, 5: Print "ANY CHANGE (Y/N)"
			I$ = GetKeyPress$

		Loop Until UCase$(I$) = "N"

		Locate 15, 5: Print Space$(55)
		Locate 16, 5: Print Space$(55)

		Color 11

		Do
			Locate 15, 5
			Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.61)"; Z$

			Locate , 5: Print "ANY CHANGE (YN)"
			I$ = GetKeyPress$

		Loop Until UCase$(I$) = "N"

		If Len(Z$) > 11 Then Z$ = Left$(Z$, 8) + Right$(Z$, 3)

		Locate 18, 5: Color 12: Print "SEARCHING FOR FILES . . ."

		If _FileExists(PR$ + "\" + Z$) Then

			Cls
			Color 15

			Open PR$ + "\" + Z$ For Input As #1

			Input #1, AR$
			Input #1, W2R%
			Input #1, L2R%

			For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I

			For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I

			For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I

			For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

			Input #1, DR0%
			Input #1, DR1%
			For I = 0 To 21: Input #1, TR(I): Next I
			For I = 0 To 22: Input #1, TR1(I): Next I

			Close #1

			If _FileExists(PR$ + "\" + Z$ + "R") Then

				Open PR$ + "\" + Z$ + "R" For Input As #1
				For I = 0 To 22
					For I1 = 0 To 6
						Input #1, S0R%(I, I1)
					Next
				Next
				Close #1

			End If

			If _FileExists(PC$ + "\" + Z$) Then

				Open PC$ + "\" + Z$ For Input As #1

				Input #1, A$: Input #1, W2%: Input #1, L2%

				For I = 0 To 22: Input #1, B$(I): Input #1, B%(I): Next I

				For I = 0 To 21: Input #1, P$(I): Input #1, P%(I): Next I

				For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I

				For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

				Input #1, D0%: Input #1, D1%
				For I = 0 To 21: Input #1, T%(I): Next I
				For I = 0 To 22: Input #1, T1%(I): Next I

				Close #1

				If Not _FileExists(PC$ + "\" + Z$ + "R") Then
					Cls
					Locate 2, 10: Color 7
					Print "NON-EXISTANT PATH OR INCORRECT FILE NAME"
					Print
					Locate , 10
					Line Input "HIT ENTER TO RE-ENTER INFORMATION"; DUMMY$

				Else

					Open PC$ + "\" + Z$ + "R" For Input As #1
					For I = 0 To 22
						For I1 = 0 To 6
							Input #1, S0%(I, I1)
						Next
					Next
					Close #1

				End If

				'Do

				Cls
				Locate 1, 34: Color 12: Print "STAT ROSTERS"
				Locate , 33: Color 15: Print "CURRENT  ";
				Color 7: Print " ROAD"

				For I = 0 To 22: 
					Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(B$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BR$(I), 10);
				Next

				For I = 0 To 21: 
					Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(P$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PR$(I), 10);
				Next

				Color 11
				Locate 7, 33: Print "HIT ANY KEY TO"
				Locate 8, 37: Print "ALIGN"
				I$ = GetKeyPress$

				Color 11
				Locate 7, 33: Print "ALIGNING FILES"
				Locate 8, 37: Print Space$(7)

				N = 0

				Do

					For J = 0 To 22

						If UCase$(B$(N)) = UCase$(BR$(J)) Then
							BN$(N) = BR$(J): BN%(N) = BR%(J)

							For K = 0 To 21
								B0N%(N, K) = B0R%(J, K)
							Next K

							For K = 0 To 6
								S0N%(N, K) = S0R%(J, K)
							Next K

						End If

					Next J

					N = N + 1

				Loop Until N = 23

				N = 0

				Do
					For J = 0 To 21

						If UCase$(P$(N)) = UCase$(PR$(J)) Then

							PN$(N) = PR$(J)
							PN%(N) = PR%(J)

							For K = 0 To 41
								P0N%(N, K) = P0R%(J, K)
							Next K

						End If

					Next J

					N = N + 1

				Loop Until N = 22

				KKK = 1

				SEQ = 0

				'Check batters
				For J = 0 To 22
					If UCase$(BN$(J)) <> UCase$(B$(J)) Then
						SEQ = SEQ + 1
					End If
				Next J

				'Check pitchers
				For J = 0 To 21
					If UCase$(PN$(J)) <> UCase$(P$(J)) Then
						SEQ = SEQ + 1
					End If
				Next J

				If SEQ = 0 Then

					Cls
					Locate 1, 34
					Color 12: Print "STAT ROSTERS"
					Locate , 33
					Color 15: Print "CURRENT  ";
					Color 7: Print " ROAD"

					For I = 0 To 22: Color 12: Locate I + 1, 1: Print Using "##"; I;: Print " ";: Color 15: Print Left$(B$(I), 10);: Color 12: Print Tab(17); Using "##"; I;: Print " ";: Color 7: Print Left$(BN$(I), 10);: Next

					For I = 0 To 21: Color 12: Locate I + 1, 48: Print Using "##"; I;: Print " ";: Color 15: Print Left$(P$(I), 10);: Color 12: Print Tab(62); Using "##"; I;: Print " ";: Color 7: Print Left$(PN$(I), 10);: Next

					Locate 7, 33
					Color 11: Print "HIT ANY KEY TO"
					Locate 8, 36: Print "CONTINUE"
					I$ = GetKeyPress$

					Open PR$ + "\" + Z$ For Output As #1

					Write #1, AR$
					Write #1, W2R%
					Write #1, L2R%

					For I = 0 To 22: Write #1, BN$(I): Write #1, BN%(I): Next I
					For I = 0 To 21: Write #1, PN$(I): Write #1, PN%(I): Next I
					For I = 0 To 22: For I1 = 0 To 21: Write #1, B0N%(I, I1): Next I1: Next I
					For I = 0 To 21: For I1 = 0 To 41: Write #1, P0N%(I, I1): Next I1: Next I

					Write #1, DR0%
					Write #1, DR1%
					For I = 0 To 21: Write #1, TR(I): Next I
					For I = 0 To 22: Write #1, TR1(I): Next I

					Close #1

					Open PR$ + "\" + Z$ + "R" For Output As #1
					For I = 0 To 22
						For I1 = 0 To 6
							Print #1, S0N%(I, I1)
						Next
					Next
					Close 1

					Cls

					Locate 8, 20: Print "RE-ALIGNMENT COMPLETED"
					Print
					Locate 12, 20
					Color 12: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

					Cls

				Else

					Cls
					Locate 5
					Color 14: Print "WARNING !! - - ROAD STAT FILE CONTAINS PLAYERS NOT IN EXISTING STATS.  YOU MUST"
					Print "CHANGE ROAD STAT FILE USING OPTION #6 ON GAME MENU BEFORE ATTEMPTING STAT MERGE."
					Locate 23, 30
					Color 13: Line Input "HIT ENTER TO CONTINUE"; DUMMY$

				End If 'Done checking SEQ

				'Loop Until DUMMY$ <> ""
				'End If 'Done checking stat files

			End If

		End If

    Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"

End Sub
