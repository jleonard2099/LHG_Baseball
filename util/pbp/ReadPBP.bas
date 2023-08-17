'$INCLUDE:'KeyInput.bi'

Dim Shared Y$, YY$
Dim Shared ntimes&
Dim Shared pbpIdx%


On Error GoTo Errhandler


Open "revised.pbp" For Random As #1 Len = 44
Y& = LOF(1)

Field #1, 44 As YY$

numRecords = Y& / 44

Print "Parsing "; numRecords; " entries in revised.pbp"

For X = 1 To numRecords
    Call GETRPBP(X, YY$)
    Print X; " - "; YY$
    nextPage$ = GetKeyPress$
Next X

Close #1



Errhandler:
Screen 0
Cls
Print "Error"; Err; "on program file line"; _ErrorLine
Print "pbp index - "; pbpIdx%
Print "# times - "; ntimes&
Print YY$
Print Y$
ErrorString$ = GetKeyPress$
Resume Next ' moves program to code following the error.



'----------------------------------------
'           GETRPBP Subroutine
'----------------------------------------
' ...explanation...
Sub GETRPBP (pbpIdx%, pbpString$)
    Open "REVISED.PBP" For Random As #1 Len = 44
    Field #1, 44 As YY$
    Get #1, pbpIdx%
    pbpString$ = _Trim$(YY$)
    Close 1

    ntimes& = ntimes& + 1

End Sub

'$INCLUDE:'KeyInput.bm'

