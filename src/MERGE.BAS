Sub MERGE ()

    _Title "Full Count Baseball - Merge Stats"

    Do
        Cls
        Color 12: Print "STATISTICS MERGE/ALIGN MENU"
        Print

        Color 14: Print "(1) ";: Color 15: Print "MERGE STAT FILES"
        Print
        Color 14: Print "(2) ";: Color 15: Print "ALIGN STAT FILES"
        Print
        Color 14: Print "(3) ";: Color 15: Print "RETURN TO UTILITIES MENU"

        Do
            I$ = GetKeyPress$
            I = Val(I$)
        Loop Until I >= 1 Or I <= 3

        Select Case I
            Case 1: Call MergeIt
            Case 2: Call ALIGN
            Case 3: Cls
        End Select

    Loop Until I = 3

End Sub

'----------------------------------------
'       MergeIt Subroutine
'----------------------------------------
'...explanation...
Sub MergeIt ()
    Shared B$(), P$()

    Dim BR$(22), DR$(2)
    Dim H$(162), H1$(40), H1R$(40), HR$(162)
    Dim L$(162), LR$(162)
    Dim PL$(162), PLR$(162), PR$(21)
    Dim W$(162), WR$(162)

    Dim mergeB%(22), B0%(22, 22), B0R%(22, 22), BR%(22)
    Dim H%(162), HR%(162), I%(162), IR%(162)
    Dim mergeP%(21), P0%(21, 41), P0R%(21, 41), PR%(21)
    Dim S0%(22, 6), SR%(22, 6)
    Dim V%(162), VR%(162)

    Dim TR(22), TR1(22)

    'Dim T%(0 to 21), T1%(0 to 22)
    Shared T%(), T1%()

	Do

		Color 12, 0
		Cls

		Locate 2, 5: Print "ENTER COMPLETE PATH FOR TEAM STATS TO MERGE"
		Color 15
		Locate 4, 5: Print "EXAMPLE - A:"
		Locate , 5: Print "          C:\HAFFNER"
		Locate , 5: Print "          C:\HAFFNER\1960"
		Locate , 5: Print "          D:\BASEBALL"

		For J = 10 To 13: 
			Locate J
			Print Space$(60) 
		Next

        Do
            Color 10
            Locate 10, 5: Line Input "ROAD STATS:     "; PR$
            Locate , 5
            Locate , 5: Line Input "CURRENT STATS:  "; PC$

			Color 14

			Locate 13, 5: Print "ANY CHANGE (Y/N)"
			I$ = GetKeyPress$

        Loop Until UCase$(I$) = "N"

        Do
            Locate 15, 5: Print Space$(55)
            Locate 16, 5: Print Space$(55)

            Color 11
            Locate 15, 5: Input "ENTER NAME OF STAT FILE (EX.- MINNESOT.61)"; Z$

			Do
				Locate , 5: Print "ANY CHANGE (YN)"
				I$ = GetKeyPress$

			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        Loop Until UCase$(I$) = "N"

        If Len(Z$) > 11 Then Z$ = Left$(Z$, 8) + Right$(Z$, 3)

        If Not _FileExists(PR$ + "\" + Z$) Then 
			Cls
			Locate 2, 10
			Color 7
			Print "NON-EXISTENT PATH OR INCORRECT FILE NAME"
			Print

            Do
                Locate , 10
                Print "TRY AGAIN (YN) "
                I$ = GetKeyPress$
            Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            If UCase$(I$) = "Y" Then 
				Print
                Print "HIT ENTER TO RE-ENTER INFORMATION"
                I$ = GetKeyPress$
            End If

        Else

            Open PR$ + "\" + Z$ For Input As #1
            Input #1, A$: Input #1, WR2%: Input #1, LR2%

            For I = 0 To 22: Input #1, BR$(I): Input #1, BR%(I): Next I
            For I = 0 To 21: Input #1, PR$(I): Input #1, PR%(I): Next I
            For I = 0 To 22: For I1 = 0 To 21: Input #1, B0R%(I, I1): Next I1: Next I
            For I = 0 To 21: For I1 = 0 To 41: Input #1, P0R%(I, I1): Next I1: Next I

            Input #1, D0R%: Input #1, D1R%
            For I = 0 To 21: Input #1, TR(I): Next I
            For I = 0 To 22: Input #1, TR1(I): Next I

            Close #1

            If _FileExists(PR$ + "\" + Z$ + "R") Then
                Open PR$ + "\" + Z$ + "R" For Input As #1
                For I = 0 To 22
                    For I1 = 0 To 6
                        Input #1, SR%(I, I1)
                    Next
                Next
                Close 1
            End If

            Open PR$ + "\" + Z$ + "S" For Input As #1
            Input #1, YR%
            For I = 1 To 162: Input #1, HR%(I): Next I
            For I = 1 To 162: Input #1, HR$(I): Next I
            For I = 1 To 162: Input #1, VR%(I): Next I
            For I = 1 To 162: Input #1, PLR$(I): Next I
            For I = 1 To 162: Input #1, WR$(I): Next I
            For I = 1 To 162: Input #1, LR$(I): Next I
            For I = 1 To 162: Input #1, IR%(I): Next I
            Close #1

            If Not _FileExists(PC$ + "\" + Z$) Then
				Cls
				Locate 2, 10
				Color 7
				Print "NON-EXISTENT PATH OR INCORRECT FILE NAME"
				Print

                Do
                    Locate , 10
                    Print "TRY AGAIN (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

                If UCase$(I$) = "Y" Then 
					Print
                    Print "HIT ENTER TO RE-ENTER INFORMATION"
                    I$ = GetKeyPress$
                End If

            Else

                Open PC$ + "\" + Z$ For Input As #1

                Input #1, A$: Input #1, W2%: Input #1, L2%

                For I = 0 To 22: Input #1, B$(I): Input #1, mergeB%(I): Next I
                For I = 0 To 21: Input #1, P$(I): Input #1, mergeP%(I): Next I
                For I = 0 To 22: For I1 = 0 To 21: Input #1, B0%(I, I1): Next I1: Next I
                For I = 0 To 21: For I1 = 0 To 41: Input #1, P0%(I, I1): Next I1: Next I

                Input #1, D0%: Input #1, D1%
                For I = 0 To 21: Input #1, T%(I): Next I
                For I = 0 To 22: Input #1, T1%(I): Next I

                Close #1

                If  _FileExists(PC$ + "\" + Z$ + "R") Then
                    Open PC$ + "\" + Z$ + "R" For Input As #1
                    For I = 0 To 22
                        For I1 = 0 To 6
                            Input #1, S0%(I, I1)
                    Next: Next
                    Close 1
                End If

                Open PC$ + "\" + Z$ + "S" For Input As #1
                Input #1, Y%
                For I = 1 To 162: Input #1, H%(I): Next I
                For I = 1 To 162: Input #1, H$(I): Next I
                For I = 1 To 162: Input #1, V%(I): Next I
                For I = 1 To 162: Input #1, PL$(I): Next I
                For I = 1 To 162: Input #1, W$(I): Next I
                For I = 1 To 162: Input #1, L$(I): Next I
                For I = 1 To 162: Input #1, I%(I): Next I
                Close #1

                SEQ = 0

                For K = 0 To 22
                    If UCase$(BR$(K)) <> UCase$(B$(K)) Then
                        SEQ = SEQ + 1
                    End If
                Next

                For K = 0 To 21
                    If UCase$(PR$(K)) <> UCase$(P$(K)) Then
                        SEQ = SEQ + 1
                    End If
                Next

                If SEQ = 0 Then

                    For K = 1 To YR%
                        Y% = Y% + 1
                        H%(Y%) = HR%(K)
                        H$(Y%) = HR$(K)
                        V%(Y%) = VR%(K)
                        PL$(Y%) = PLR$(K)
                        W$(Y%) = WR$(K)
                        L$(Y%) = LR$(K)
                        I%(Y%) = IR%(K)
                    Next

                    W2% = W2% + WR2%: L2% = L2% + LR2%

                    Locate 18, 20: Color 12: Print "MERGING STAT FILE AND SCHEDULE RESULTS . . . ."

                    For I = 0 To 22
                        For I1 = 0 To 6
                            S0%(I, I1) = S0%(I, I1) + SR%(I, I1)
                        Next

                        For I1 = 0 To 11: B0%(I, I1) = B0%(I, I1) + B0R%(I, I1): Next I1
                        For I1 = 14 To 21: B0%(I, I1) = B0%(I, I1) + B0R%(I, I1): Next I1
                    Next I

                    For I = 0 To 21
                        For I1 = 0 To 34: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1
                        For I1 = 37 To 41: P0%(I, I1) = P0%(I, I1) + P0R%(I, I1): Next I1
                    Next I

                    D0% = D0% + DR0%: D1% = D1% + DR1%

                    For I = 0 To 21: T%(I) = T%(I) + TR(I): Next
                    For I = 0 To 22: T1%(I) = T1%(I) + TR1(I): Next

                    Open PC$ + "\" + Z$ For Output As #1

                    Write #1, A$: Write #1, W2%: Write #1, L2%

                    For I = 0 To 22: Write #1, B$(I): Write #1, mergeB%(I): Next I
                    For I = 0 To 21: Write #1, P$(I): Write #1, mergeP%(I): Next I
                    For I = 0 To 22: For I1 = 0 To 21: Write #1, B0%(I, I1): Next I1: Next I
                    For I = 0 To 21: For I1 = 0 To 41: Write #1, P0%(I, I1): Next I1: Next I

                    Write #1, D0%: Write #1, D1%

                    For I = 0 To 21: Write #1, T%(I): Next I
                    For I = 0 To 22: Write #1, T1%(I): Next I

                    Close #1

                    Open PC$ + "\" + Z$ + "R" For Output As #1
                    For I = 0 To 22
                        For I1 = 0 To 6
                            Write #1, S0%(I, I1)
                        Next
                    Next
                    Close 1

                    Open PC$ + "\" + Z$ + "S" For Output As #1
                    Write #1, Y%
                    For I = 1 To 162: Write #1, H%(I): Next I
                    For I = 1 To 162: Write #1, H$(I): Next I
                    For I = 1 To 162: Write #1, V%(I): Next I
                    For I = 1 To 162: Write #1, PL$(I): Next I
                    For I = 1 To 162: Write #1, W$(I): Next I
                    For I = 1 To 162: Write #1, L$(I): Next I
                    For I = 1 To 162: Write #1, I%(I): Next I
                    Close #1

                    Y = 0

                    Locate 20, 20: Color 12: Print "RESULTS SUCCESSFULLY MERGED!!"

                Else

                    Cls
                    Color 14
                    Locate 5, 27: Print "PLAYERS NOT IN SEQUENCE !!"; DUMMY$
                    Color 15
                    Locate 7, 20: Print "STATS WILL BE DISTORTED UNLESS THE ROAD"
                    Locate , 19: Print "FILE LISTS THE PLAYERS IN THE SAME SEQUENCE"
                    Locate , 24: Print "AS YOUR CURRENT PLAYER STAT FILE."
                    Locate 11, 24
                    Color 12: Print "YOU MUST REALIGN ROAD STAT FILE OR"
                    Locate , 31: Print "ENTER STATS MANUALLY."
                    Locate 15, 29
                    Color 13: Line Input "HIT ENTER TO GO TO MENU"; DUMMY$

                End If 'Done checking for SEQ = 0

            End If 'Done checking for stat file

        End If 'Done checking for road file

	Loop Until DUMMY$ <> "" Or UCase$(I$) = "N"

End Sub
