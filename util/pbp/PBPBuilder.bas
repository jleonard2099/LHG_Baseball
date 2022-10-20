' Read pbpstrings by line
' ---------------------

'Determine how many records
Open "pbpstrings" For Input As #1
Do
    Line Input #1, A$
    numRecords% = numRecords% + 1
Loop Until EOF(1)
Close #1

' Assign all records to an array
' This uses more memory but less disk
ReDim YYY$(numRecords%)

Open "pbpstrings" For Input As #1 'Len = 44
ReDim Shared YYY$(numRecords%)

For X = 1 To numRecords%
    Line Input #1, YYY$(X)
Next X

Close #1


' Create revised.PBP file
' ---------------------
Open "revised.pbp" For Output As #2 Len = 44

For X = 1 To UBound(YYY$)
    Print #2, YYY$(X);
Next X

Close #2

