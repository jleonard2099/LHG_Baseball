'----------------------------------------
'            POSCR Subroutine
'----------------------------------------
'Create position file that we can later
'use to sort / search
Sub POSCR ()

    _Title "Full Count Baseball - Create Position Search"

    Shared posTeamIdx()
	Shared memberName$(), posTeamYr$()

    Do

		LGE$ = _InputBox$("Postion File Name","Enter name for position file")
		
		If LGE$ <> "" Then

			Color 15, 0
			Cls

			teamNbr = 0
			Call ChooseTeam (YN$, NTMS, teamNbr, team$)

			If teamNbr <> 0 Then

				J = J + 1

				posTeamIdx(J) = teamNbr
				memberName$(J) = team$
				posTeamYr$(J) = YN$

				Color 15, 0
				Cls
				Locate 7: Color 12
				Print "1.  ";
				Color 7: Print memberName$(1);
				Locate , 27: Color 12
				Print "2.  ";
				Color 7: Print memberName$(2);
				Locate , 54: Color 12
				Print "3.  ";
				Color 7: Print memberName$(3)
				Locate 8: Color 12
				Print "4.  ";
				Color 7: Print memberName$(4);
				Locate , 27: Color 12
				Print "5.  ";
				Color 7: Print memberName$(5);
				Locate , 54: Color 12
				Print "6.  ";
				Color 7: Print memberName$(6)
				Locate 9: Color 12
				Print "7.  ";
				Color 7: Print memberName$(7);
				Locate , 27: Color 12
				Print "8.  ";
				Color 7: Print memberName$(8);
				Locate , 54: Color 12
				Print "9.  ";
				Color 7: Print memberName$(9)
				Locate 10: Color 12
				Print "10. ";
				Color 7: Print memberName$(10);
				Locate , 27: Color 12
				Print "11. ";
				Color 7: Print memberName$(11);
				Locate , 54: Color 12
				Print "12. ";
				Color 7: Print memberName$(12)
				Locate 11: Color 12
				Print "13. ";
				Color 7: Print memberName$(13);
				Locate , 27: Color 12
				Print "14. ";
				Color 7: Print memberName$(14);
				Locate , 54: Color 12
				Print "15. ";
				Color 7: Print memberName$(15)
				Locate 12: Color 12
				Print "16. ";
				Color 7: Print memberName$(16);
				Locate , 27: Color 12
				Print "17. ";
				Color 7: Print memberName$(17);
				Locate , 54: Color 12
				Print "18. ";
				Color 7: Print memberName$(18)
				Locate 13: Color 12
				Print "19. ";
				Color 7: Print memberName$(19);
				Locate , 27: Color 12
				Print "20. ";
				Color 7: Print memberName$(20);
				Locate , 54: Color 12
				Print "21. ";
				Color 7: Print memberName$(21)
				Locate 14: Color 12
				Print "22. ";
				Color 7: Print memberName$(22);
				Locate , 27: Color 12
				Print "23. ";
				Color 7: Print memberName$(23);
				Locate , 54: Color 12
				Print "24. ";
				Color 7: Print memberName$(24)
				Locate 15: Color 12
				Print "25. ";
				Color 7: Print memberName$(25);
				Locate , 27: Color 12
				Print "26. ";
				Color 7: Print memberName$(26);
				Locate , 54: Color 12
				Print "27. ";
				Color 7: Print memberName$(27)
				Locate 16: Color 12
				Print "28. ";
				Color 7: Print memberName$(28);
				Locate , 27: Color 12
				Print "29. ";
				Color 7: Print memberName$(29);
				Locate , 54: Color 12
				Print "30. ";
				Color 7: Print memberName$(30)
				Locate 17: Color 12
				Print "31. ";
				Color 7: Print memberName$(31);
				Locate , 27: Color 12
				Print "32. ";
				Color 7: Print memberName$(32);
				Locate , 54: Color 12
				Print "33. ";
				Color 7: Print memberName$(33)
				Locate 18: Color 12
				Print "34. ";
				Color 7: Print memberName$(34);
				Locate , 27: Color 12
				Print "35. ";
				Color 7: Print memberName$(35);
				Locate , 54: Color 12
				Print "36. ";
				Color 7: Print memberName$(36)
				Locate 19: Color 12
				Print "37. ";
				Color 7: Print memberName$(37);
				Locate , 27: Color 12
				Print "38. ";
				Color 7: Print memberName$(38);
				Locate , 54: Color 12
				Print "39. ";
				Color 7: Print memberName$(39)
				Locate 20: Color 12
				Print "40. ";
				Color 7: Print memberName$(40)

				Do
					Locate 22
					Print "ENTER ANOTHER TEAM? (Y/N)"
					I$ = GetKeyPress$
				Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

			End If
			
		End If

    Loop Until UCase$(I$) = "N" Or LGE$ = ""

	If LGE$ <> "" Then

		Open diskPaths$(0) + LGE$ + ".POS" For Output As #2

		For J = 1 To TEAMS_PER_LEAGUE 
			Write #2, posTeamIdx(J), memberName$(J), posTeamYr$(J)
		Next

		Close #2

	End If

End Sub
