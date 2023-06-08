Sub POSCR ()

    _Title "Full Count Baseball - Create Position Search"

    Dim TM(1 To 40)
    Dim T$(1 To 40), TYN$(1 To 40)

    Do

        Do
            
			Color 15, 0
			Cls

            JJ = 7
            Locate 6, 8: Color 15, 1
            Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
            Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
            Locate 7 + JJ, 8
            Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
			
			Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR POSITION FILE: ": Print
            Locate , 12: Print "THE NAME IS 8 CHARACTERS LONG WITH NO PERIODS OR"
            Locate , 12: Print "SPACES. DO NOT ADD A FILE EXTENSION, THE SCHEDULER"
            Locate , 12: Print "WILL AUTOMATICALLY ADD .POS"
            Locate , 12: Print "SAMPLE NAMES ARE AL-1965, DRAFT, ETC"
            Locate 7, 41: Input LGE$

            If LGE$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE (YN) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "N"

    Loop Until LGE$ <> ""


    Do

        Color 15, 0
        Cls

        Call GetTeam(YN$, NTMS, T)
        team$ = RTrim$(Q$(0))

        J = J + 1

        TM(J) = T
        T$(J) = team$
        TYN$(J) = YN$

        Color 15, 0
        Cls
        Locate 7: Color 12
		Print "1.  ";
		Color 7: Print T$(1);
        Locate , 27: Color 12
		Print "2.  ";
		Color 7: Print T$(2);
        Locate , 54: Color 12
		Print "3.  ";
		Color 7: Print T$(3)
        Locate 8: Color 12
		Print "4.  ";
		Color 7: Print T$(4);
        Locate , 27: Color 12
		Print "5.  ";
		Color 7: Print T$(5);
        Locate , 54: Color 12
		Print "6.  ";
		Color 7: Print T$(6)
        Locate 9: Color 12
		Print "7.  ";
		Color 7: Print T$(7);
        Locate , 27: Color 12
		Print "8.  ";
		Color 7: Print T$(8);
        Locate , 54: Color 12
		Print "9.  ";
		Color 7: Print T$(9)
        Locate 10: Color 12
		Print "10. ";
		Color 7: Print T$(10);
        Locate , 27: Color 12
		Print "11. ";
		Color 7: Print T$(11);
        Locate , 54: Color 12
		Print "12. ";
		Color 7: Print T$(12)
        Locate 11: Color 12
		Print "13. ";
		Color 7: Print T$(13);
        Locate , 27: Color 12
		Print "14. ";
		Color 7: Print T$(14);
        Locate , 54: Color 12
		Print "15. ";
		Color 7: Print T$(15)
        Locate 12: Color 12
		Print "16. ";
		Color 7: Print T$(16);
        Locate , 27: Color 12
		Print "17. ";
		Color 7: Print T$(17);
        Locate , 54: Color 12
		Print "18. ";
		Color 7: Print T$(18)
        Locate 13: Color 12
		Print "19. ";
		Color 7: Print T$(19);
        Locate , 27: Color 12
		Print "20. ";
		Color 7: Print T$(20);
        Locate , 54: Color 12
		Print "21. ";
		Color 7: Print T$(21)
        Locate 14: Color 12
		Print "22. ";
		Color 7: Print T$(22);
        Locate , 27: Color 12
		Print "23. ";
		Color 7: Print T$(23);
        Locate , 54: Color 12
		Print "24. ";
		Color 7: Print T$(24)
        Locate 15: Color 12
		Print "25. ";
		Color 7: Print T$(25);
        Locate , 27: Color 12
		Print "26. ";
		Color 7: Print T$(26);
        Locate , 54: Color 12
		Print "27. ";
		Color 7: Print T$(27)
        Locate 16: Color 12
		Print "28. ";
		Color 7: Print T$(28);
        Locate , 27: Color 12
		Print "29. ";
		Color 7: Print T$(29);
        Locate , 54: Color 12
		Print "30. ";
		Color 7: Print T$(30)
        Locate 17: Color 12
		Print "31. ";
		Color 7: Print T$(31);
        Locate , 27: Color 12
		Print "32. ";
		Color 7: Print T$(32);
        Locate , 54: Color 12
		Print "33. ";
		Color 7: Print T$(33)
        Locate 18: Color 12
		Print "34. ";
		Color 7: Print T$(34);
        Locate , 27: Color 12
		Print "35. ";
		Color 7: Print T$(35);
        Locate , 54: Color 12
		Print "36. ";
		Color 7: Print T$(36)
        Locate 19: Color 12
		Print "37. ";
		Color 7: Print T$(37);
        Locate , 27: Color 12
		Print "38. ";
		Color 7: Print T$(38);
        Locate , 54: Color 12
		Print "39. ";
		Color 7: Print T$(39)
        Locate 20: Color 12
		Print "40. ";
		Color 7: Print T$(40)

        Do
            Locate 22
			Print "ENTER ANOTHER TEAM (Y/N)"
            I$ = GetKeyPress$
        Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

    Loop Until UCase$(I$) = "N"

    Open diskPaths$(0) + LGE$ + ".POS" For Output As #2
    For J = 1 To 40: 
		Write #2, TM(J), T$(J), TYN$(J)
	Next

    Close #2

End Sub

'----------------------------------------
'           GetTeam Subroutine
'----------------------------------------
'...explanation...
Sub GetTeam (yearNumber$, numberTeams, teamIndex%)

    Shared ratings%()

    Do

        Color 15, 0
		Cls

        JJ = 7
        Locate 6, 8: Color 15, 4
        Print " " + Chr$(214); String$(61, 196); Chr$(183) + " ";: For II = 1 To JJ: Locate 6 + II, 8
        Print " " + Chr$(186); String$(61, 32); Chr$(186) + " ";: Next
        Locate 7 + JJ, 8
        Print " " + Chr$(211); String$(61, 196); Chr$(189) + " ";
		
		Locate 7, 32

        Locate , 12: Print "ENTER DISK ID: ": Print
        Locate , 12: Print "THE DISK ID IS USUALLY THE LAST TWO DIGITS OF THE"
        Locate , 12: Print "SEASON DISK YOU ARE PLAYING FROM (IE. 89, 76, 67)."
        Locate , 12: Print "HIT <ENTER> TO USE PREVIOUS ID"
        Locate 7, 27: Input NY$

		If NY$ = "" And yearNumber$ <> "" Then
            Call SelectTeam_POSCR(yearNumber$, numberTeams, teamIndex%)
        Else
            yearNumber$ = RTrim$(NY$)
            Erase teamNames$
            numberTeams = 0

            If _FileExists(diskPaths$(0) + "FCTEAMS." + yearNumber$) Then

                Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

                fileLength& = LOF(1)

                Field #1, 15 As Q$(0)

                For I1 = 1 to fileLength& / 4342
                    Get #1, I1
                    For I2 = 1 To 15
                        If Left$(Q$(0), 2) = "XX" Or Left$(Q$(0), 1) = " " Or Asc(Mid$(Q$(0), I2, 1)) < 32 Or Asc(Mid$(Q$(0), I2, 1)) > 123 Or UCase$(Left$(Q$(0), 3)) = "NOT" Or UCase$(Left$(Q$(0), 3)) = "UN" Then nullTeam = 1: Exit For
                    Next I2
                    If nullTeam <> 1 Then
                        numberTeams = numberTeams + 1
                        teamNames$(numberTeams) = Q$(0)
                        teamIndex%(numberTeams) = I1
                    End If
                Next I1

                Close 1

                Call SelectTeam_POSCR(yearNumber$, numberTeams, teamIndex%)
            Else
                BO% = 1
            End If
        End If

        targetTeam$ = RTrim$(Q$(0))

        If BO% = 1 Then
            BO% = 0
        Else
            Locate 2, 33
            Color ratings%(11), ratings%(12): Print targetTeam$

			Do
				Color 15, 4
				Locate 3, 33: Print "ANY CHANGE (YN)";
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

        End If

    Loop Until UCase$(I$) = "N"

End Sub


'----------------------------------------
'    SelectTeam_POSCR Subroutine
'----------------------------------------
'...explanation...
Sub SelectTeam_POSCR (yearNumber$, numberTeams, teamIndex%)
    Selection% = 1
    Count% = numberTeams
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
    MAMenu teamNames$(), Selection%, Start%, Count%, ExitCode%, FileFGColr%, FileBGColr%, HiLiteFGColr%, HiLiteBGColr%, Rows%, Columns%, Spacing%, Row%, Column%
    _MouseHide

    If ExitCode% <> 27 Then

        teamIndex% = teamIndex%(Selection%)
		teamYear$ = RTrim$(yearNumber$)
		
        Locate 2, 33: Color 3
		Print "LOADING.."

        Call ReadTeam_POSCR(teamYear$, teamIndex%)

    Else
        BO% = 1
    End If

End Sub

'----------------------------------------
'       ReadTeam_POSCR Subroutine
'----------------------------------------
'...explanation...
Sub ReadTeam_POSCR (yearNumber$, teamIndex%)

    Shared K9&()
    Shared statB$(), statP$()
    Shared draftB%(), ratings%(), draftP%()

    Open diskPaths$(0) + "FCTEAMS." + yearNumber$ For Random As #1 Len = 4342

    Field #1, 15 As Q$(0), 2 As Q$(1), 2 As Q$(2), 2 As Q$(3), 2 As Q$(4), 2 As Q$(5), 2 As Q$(6), 2 As Q$(7), 2 As Q$(8), 2 As Q$(9), 2 As Q$(10), 2 As Q$(11), 2 As Q$(12), 2 As Q$(13), 4 As Q$(14), 4297 As X$

    For I = 0 To 22
        Field #1, 45 + I * 99 As X$, 15 As Q$(I * 43 + 15), 2 As Q$(I * 43 + 16), 2 As Q$(I * 43 + 17), 2 As Q$(I * 43 + 18), 2 As Q$(I * 43 + 19), 2 As Q$(I * 43 + 20), 2 As Q$(I * 43 + 21), 2 As Q$(I * 43 + 22), 2 As Q$(I * 43 + 23), 2 As Q$(I * 43 + 24), 2 As Q$(I * 43 + 25), 2 As Q$(I * 43 + 26), 4297 - 37 - I * 99 As X$
        Field #1, 82 + I * 99 As X$, 2 As Q$(I * 43 + 27), 2 As Q$(I * 43 + 28), 2 As Q$(I * 43 + 29), 2 As Q$(I * 43 + 30), 2 As Q$(I * 43 + 31), 2 As Q$(I * 43 + 32), 2 As Q$(I * 43 + 33), 2 As Q$(I * 43 + 34), 2 As Q$(I * 43 + 35), 2 As Q$(I * 43 + 36), 2 As Q$(I * 43 + 37), 2 As Q$(I * 43 + 38), 4297 - 61 - I * 99 As X$
        Field #1, 106 + I * 99 As X$, 2 As Q$(I * 43 + 39), 2 As Q$(I * 43 + 40), 2 As Q$(I * 43 + 41), 2 As Q$(I * 43 + 42), 2 As Q$(I * 43 + 43), 2 As Q$(I * 43 + 44), 2 As Q$(I * 43 + 45), 2 As Q$(I * 43 + 46), 2 As Q$(I * 43 + 47), 2 As Q$(I * 43 + 48), 2 As Q$(I * 43 + 49), 2 As Q$(I * 43 + 50), 4297 - 85 - I * 99 As X$
        Field #1, 130 + I * 99 As X$, 2 As Q$(I * 43 + 51), 2 As Q$(I * 43 + 52), 2 As Q$(I * 43 + 53), 2 As Q$(I * 43 + 54), 2 As Q$(I * 43 + 55), 2 As Q$(I * 43 + 56), 2 As Q$(I * 43 + 57), 4297 - 99 - I * 99 As X$
    Next

    For I = 0 To 21
        Field #1, 2322 + I * 89 As X$, 15 As Q$(I * 38 + 1004), 2 As Q$(I * 38 + 1005), 2 As Q$(I * 38 + 1006), 2 As Q$(I * 38 + 1007), 2 As Q$(I * 38 + 1008), 2 As Q$(I * 38 + 1009), 2 As Q$(I * 38 + 1010), 2 As Q$(I * 38 + 1011), 2 As Q$(I * 38 + 1012), 2 As Q$(I * 38 + 1013), 2 As Q$(I * 38 + 1014), 2 As Q$(I * 38 + 1015), 2020 - 37 - I * 89 As X$
        Field #1, 2359 + I * 89 As X$, 2 As Q$(I * 38 + 1016), 2 As Q$(I * 38 + 1017), 2 As Q$(I * 38 + 1018), 2 As Q$(I * 38 + 1019), 2 As Q$(I * 38 + 1020), 2 As Q$(I * 38 + 1021), 2 As Q$(I * 38 + 1022), 2 As Q$(I * 38 + 1023), 2 As Q$(I * 38 + 1024), 2 As Q$(I * 38 + 1025), 2 As Q$(I * 38 + 1026), 2 As Q$(I * 38 + 1027), 2020 - 61 - I * 89 As X$
        Field #1, 2383 + I * 89 As X$, 2 As Q$(I * 38 + 1028), 2 As Q$(I * 38 + 1029), 2 As Q$(I * 38 + 1030), 2 As Q$(I * 38 + 1031), 2 As Q$(I * 38 + 1032), 2 As Q$(I * 38 + 1033), 2 As Q$(I * 38 + 1034), 2 As Q$(I * 38 + 1035), 2 As Q$(I * 38 + 1036), 2 As Q$(I * 38 + 1037), 2 As Q$(I * 38 + 1038), 2 As Q$(I * 38 + 1039), 2020 - 85 - I * 89 As X$
        Field #1, 2407 + I * 89 As X$, 2 As Q$(I * 38 + 1040), 2 As Q$(I * 38 + 1041), 2020 - 89 - I * 89 As X$
    Next

    Field #1, 4280 As X$, 40 As Q$(1840), 15 As Q$(1841), 5 As Q$(1842), 2 As Q$(1843)

    Get #1, teamIndex%

    For I = 1 To 13: ratings%(I) = CVI(Q$(I)): Next

    K9&(0) = CVS(Q$(14))

    For I = 0 To 22:
        statB$(I) = RTrim$(Q$(I * 43 + 15))
        For I1 = 0 To 41:
            draftB%(I, I1) = CVI(Q$(I * 43 + I1 + 16))
        Next
    Next

    For I = 0 To 21:
        statP$(I) = RTrim$(Q$(I * 38 + 1004))
        For I1 = 0 To 36
            draftP%(I, I1) = CVI(Q$(I * 38 + I1 + 1005))
        Next
    Next

    S$ = RTrim$(Q$(1840))
    M$ = RTrim$(Q$(1841))
    NN$ = RTrim$(Q$(1842))
    CK = CVI(Q$(1843))
    Close 1

End Sub
