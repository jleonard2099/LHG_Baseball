'----------------------------------------
'
'----------------------------------------
'
Sub POSCR ()

    _Title "Full Count Baseball - Create Position Search"

    Dim TM(1 To 40)
    Dim T$(1 To 40), TYN$(1 To 40)

    Do

        Do
            
			Color 15, 0
			Cls

			Call MenuBox (6, 8, 61, 7, 15, 1)

			Locate 7, 32

            Locate , 12: Print "ENTER NAME FOR POSITION FILE: ": Print
			Locate , 12: Print "THE NAME SHOULD HAVE NO PERIODS OR SPECIAL CHARACTERS"
			Locate , 12: Print "DO NOT ADD A FILE EXTENSION; THE PROGRAM"
            Locate , 12: Print "WILL AUTOMATICALLY ADD .POS"
            Locate , 12: Print "SAMPLE NAMES ARE AL-1965, DRAFT, ETC"
            Locate 7, 41: Input LGE$

            If LGE$ <> "" Then

                Do
                    Locate 13, 12: Print "ANY CHANGE? (Y/N) "
                    I$ = GetKeyPress$
                Loop Until UCase$(I$) = "Y" Or UCase$(I$) = "N"

            End If

        Loop Until UCase$(I$) = "N"

    Loop Until LGE$ <> ""


    Do

        Color 15, 0
        Cls

		Call ChooseTeam (YN$, NTMS, teamNbr, team$)

		If teamNbr <> 0 Then

			J = J + 1

			TM(J) = teamNbr
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
				Print "ENTER ANOTHER TEAM? (Y/N)"
				I$ = GetKeyPress$
			Loop Until UCase$(I$) = "N" Or UCase$(I$) = "Y"

		End If

    Loop Until UCase$(I$) = "N"

    Open diskPaths$(0) + LGE$ + ".POS" For Output As #2

    For J = 1 To 40: 
		Write #2, TM(J), T$(J), TYN$(J)
	Next

    Close #2

End Sub
