$CONSOLE

Data Arizona Diamondbacks
Data Atlanta Braves
Data Chicago Cubs
Data Cincinnati Reds
Data Colorado Rockies
Data Los Angeles Dodgers
Data Miami Marlins
Data Milwaukee Brewers
Data New York Mets
Data Philadelphia Phillies
Data Pittsburgh Pirates
Data St. Louis Cardinals
Data San Diego Padres
Data San Francisco Giants
Data Washington Nationals
Data Baltimore Orioles
Data Boston Red Sox
Data Chicago White Sox
Data Cleveland Guardians
Data Detroit Tigers
Data Houston Astros
Data Kansas City Royals
Data Los Angeles Angels
Data Minnesota Twins
Data New York Yankees
Data Oakland Athletics
Data Seattle Mariners
Data Tampa Bay Rays
Data Texas Rangers
Data Toronto Blue Jays

Dim Shared teamNameX$(30)
Dim Shared scheduleNG%(1500, 18)
Dim Shared scheduleYN$(1500, 2)

For iBM = 1 To 30:
    Read teamNameX$(iBM)
Next

_Dest _Console

' 2024 MLB schedule CSV was found here:  https://shanemcd.org/2024/03/21/2024-mlb-schedule-in-excel-xlsx-and-csv-formats/

filePath$ = "C:\Users\BBM0928\Downloads\FullCount\mlb-2024-orig.csv"

IF LEN(filePath$) > 0 THEN
    ReadCSVFile filePath$
ELSE
    PRINT "No file path provided. Exiting program."
END IF


SUB ReadCSVFile (filePath$)
    Const SCHEDULE_SIZE_BYTES = 48
    Dim Q$(23)

    IF _FileExists(filePath$) THEN

        If _FileExists("ML2024-1.SCD") Then Kill "ML2024-1.SCD"
        If _FileExists("ML2024-2.SCD") Then Kill "ML2024-2.SCD"
        Open "ML2024-1.SCD" For Random As #2 Len = SCHEDULE_SIZE_BYTES

        For X = 0 To 18
            Field #2, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
        Next
        Field #2, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)

        Open filePath$ FOR INPUT AS #1
        Line Input #1, line$    ' skip header line
        PRINT "Header line: "; line$'

        iBM = 0
        Do While Not EOF(1)
            Line Input #1, line$
            If line$ = "" Then
                Print "Empty line found, skipping..."
                Print
                _Continue
            END IF

            Print iBM; line$
            comma = INSTR(line$, ",")
            theDate$ = LEFT$(line$, comma - 1)

            Print "Date  : "; theDate$

            dash = INSTR(theDate$, "-")
            theYear$ = LEFT$(theDate$, dash - 1)

            theDate$ = MID$(theDate$, dash + 1)
            dash = INSTR(theDate$, "-")
            theMonth$ = LEFT$(theDate$, dash - 1)

            theDate$ = MID$(theDate$, dash + 1)
            theDay$ = theDate$

            Print "Year  : "; theYear$; " Month: "; theMonth$; " Day: "; theDay$

            line$ = MID$(line$, comma + 1)
            comma = INSTR(line$, ",")
            theTime$ = LEFT$(line$, comma - 1)

            Print "Time  : "; theTime$

            colon = INSTR(theTime$, ":")
            theHour$ = LEFT$(theTime$, colon - 1)
            theTime$ = MID$(theTime$, colon + 1)

            space = INSTR(theTime$, " ")
            theMinute$ = LEFT$(theTime$, space - 1)
            theAMPM$ = MID$(theTime$, space + 1)

            Print "Hour  : "; theHour$; " Minute: "; theMinute$; " AMPM: "; theAMPM$

            line$ = MID$(line$, comma + 1)
            comma = INSTR(line$, ",")
            bsTime$ = LEFT$(line$, comma - 1)

            Print "bsTime: "; bsTime$   ' not used, but appears in the CSV

            line$ = MID$(line$, comma + 1)
            comma = INSTR(line$, ",")
            theVisitor$ = LEFT$(line$, comma - 1)

            Print "Visitr: "; theVisitor$; FindTeamIndex(theVisitor$)

            theHome$ = MID$(line$, comma + 1)

            Print "Home  : "; theHome$; FindTeamIndex(theHome$)

            If FindTeamIndex(theVisitor$) = -1 Then
                PRINT "Visitor team not found in the list!"
                Exit Sub
            End If
            If FindTeamIndex(theHome$) = -1 Then
                PRINT "Home team not found in the list!"
                Exit Sub
            End If

            LSet Q$(1) = MKI$( FindTeamIndex(theVisitor$) )
            LSet Q$(2) = MKI$( FindTeamIndex(theHome$) )

            For iFoo = 3 To 15:
                LSet Q$(iFoo) = MKI$(0)
            Next
            LSet Q$(16) = MKI$( VAL(theHour$) * 100 + VAL(theMinute$) + 1200 * ABS(theAMPM$ = "PM") )
            LSet Q$(17) = MKI$( VAL(theDay$) )
            LSet Q$(18) = MKI$( VAL(theMonth$) )
            LSet Q$(19) = MKI$( VAL(theYear$) - 2000 )

            LSet Q$(22) = MKI$(0)   ' visitor score'
            LSet Q$(23) = MKI$(0)   ' home score'
            LSet Q$(20) = RTrim$("H24")
            LSet Q$(21) = RTrim$("H24")

            Put #2, (iBM MOD 1215) + 1
            iBM = iBM + 1

            if iBM = 1215 Then  ' halfway point (81 of 162), close this file and open a second one, because FullCount currently allows only 1500 rows per schedule file
                Close #2
                Open "ML2024-2.SCD" For Random As #2 Len = SCHEDULE_SIZE_BYTES

                For X = 0 To 18
                    Field #2, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
                Next
                Field #2, 38 As X$, 2 As Q$(22), 2 As Q$(23), 3 As Q$(20), 3 As Q$(21)
            End If

        LOOP

        Close #1
        Close #2

        Print "Total lines read: "; iBM
    ELSE
        Print "File not found: "; filePath$
    END IF
END SUB


' Function to find the index of a team name in the array
Function FindTeamIndex(teamName$)
    Shared teamNameX$()

    For iBM = 1 To 30:
        IF teamNameX$(iBM) = teamName$ THEN
            FindTeamIndex = iBM
            EXIT FUNCTION
        END IF
    Next

    FindTeamIndex = -1      ' Not found
End Function


Sub SaveScheduleRow(rowNum%)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 18:
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1:
            LSet Q$(20 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(22) = MKI$(visitorScores(I))
        LSet Q$(23) = MKI$(homeScores(I))

        Put #1, I

    Next

End Sub
