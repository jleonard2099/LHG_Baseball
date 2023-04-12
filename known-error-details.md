
- Executing code from:  12330 
B7%(P9, I)  1 

ComputerLineups
	Call SortHitters(P9)
	Call pbpLog(12330)
	Call DHLineups(P9, reselect, LU%)


- Executing code from:  11608 
B7%(P9, I) -1 

DHLineups

    For I = 1 To 9:
		...
		Call pbpLog(11608)
		...
	Next I


- Executing code from:  11625 
B7%(P9, I)  1 

    If LU% = 1 Then LU% = 0: Call SelectBatters(batterFlag%, P9)

    For I = 10 To 22
        Locate I, 59: Print Space$(21);
    Next

    'Cls
    Call pbpLog(11625)
    Call NEWLINES(P9)


SelectBatters does not modify B7%() or call any routine that does
The exception is assigning -1 if you ESC from the menu
This is only happening outside of autoplay
Moreover, at this point the value has been corrected

________________________________________


- Executing code from:  14470 
B9%(D, 1)   2 

    If O%(0) = 3 Then
        Call pbpLog(14470)
        GoTo 15000

- 1500 '
beginning of source
nothing that changes B9%()

- Executing code from:  13118 
B9%(D, 1)   99 

    If usingGfx = 1 Then
        Call pbpLog(13118)
        Call DisplayBallField_Gfx(BC%, P, U%, D)

________________________________________
