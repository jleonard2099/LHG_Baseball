Randomize Timer

Dim x$(67)

GoTo 10

'----------------------------------------

' This clearly is meant to
' assign strings from test.bas
' to array values for X$
Open "pbpstrings.bas" For Input As #1
For x = 1 To 10
    Input #1, x$(x)
    Print x$(x)
Next
Close 1

' This is used to create fc.pbp
' as a file with strings from
' test.bas, starting at 626
' The first 625 items are older
' PBP strings ???
Open "fc.pbp" As #1 Len = 30
Field #1, 30 As q$
For x = 1 To 10
    LSet q$ = x$(x)
    Put #1, x + 625
Next

Close 1

'----------------------------------------

' This creates pbpstrings.BAS from 
' the revised.pbp file

' This file is used to build revised.pbp
' from the REVISED.BAS script file

' In the end, the revised.pbp file has junk
' and no cleanup is done, so this code 
' seems useless.

10 '
Cls
Kill "pbpstrings.bas"

For x = 1 To 3169

    Open "REVISED.pbp" As #1 Len = 44
    Field #1, 44 As q$
    Get #1, x
    X$ = q$

    Open "test.bas" For Append As #2
    Write #2, X$
    Close 2

    '15 If Right$(X$, 1) = " " Then X$ = Left$(X$, Len(X$) - 1): GoTo 15
    Print _Trim$(X$)

    Close 1
Next

