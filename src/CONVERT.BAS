'----------------------------------------
'         ReadSchedOld SubRoutine
'----------------------------------------
'This subroutine opens the target SCHEDULE
'file and reads the data into corresponding
'arrays for use in the scheduler
Sub ReadSchedOld (targetFile$)

    Shared BS%, NS%

    Shared scheduleAP%(), scheduleNG%()
    Shared homeScores(), visitorScores()
    Shared scheduleYN$()

    Open targetFile$ For Random As #1 Len = (SCHEDULE_SIZE_BYTES - 4)

    fileLength& = LOF(1)
    scheduleAP%(0) = fileLength& / (SCHEDULE_SIZE_BYTES - 4)
    BS% = Int((scheduleAP%(0) - 1) / 20)

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 4) - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 5 As Q$(20), 5 As Q$(21)

    For I = 1 To scheduleAP%(0)

        Get #1, I

        For X = 0 To 18
            scheduleNG%(I, X) = CVI(Q$(X + 1))
        Next

        For X = 0 To 1
            scheduleYN$(I, X) = RTrim$(Q$(20 + X))
        Next

        visitorScores(I) = CVI(Q$(22))
        homeScores(I) = CVI(Q$(23))

    Next

    Close 1

    NS% = 0

End Sub


'----------------------------------------
'         SaveSchedOld SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveSchedOld (saveFile$)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(diskPaths$(3) + saveFile$ + ".SCD") Then Kill diskPaths$(3) + saveFile$ + ".SCD"

    Open diskPaths$(3) + saveFile$ + ".SCD" For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 18
        Field #1, X * 2 As X$, 2 As Q$(X + 1), (SCHEDULE_SIZE_BYTES - 4) - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(22), 2 As Q$(23), 5 As Q$(20), 5 As Q$(21)

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

    Close #1

End Sub


'----------------------------------------
'    SaveSchedFileConverted SubRoutine
'----------------------------------------
'This subroutine writes all scheduled data,
'as stored in the relevant arrays, back to
'the target schedule file.
Sub SaveSchedFileConverted (saveFile$, numberGames)

    Shared homeScores(), visitorScores()
    Shared scheduleAP%(), scheduleNG%()
    Shared scheduleYN$()

    If _FileExists(saveFile$) Then Kill saveFile$

    Open saveFile$ For Random As #1 Len = SCHEDULE_SIZE_BYTES

    For X = 0 To 20
        Field #1, X * 2 As X$, 2 As Q$(X + 1), SCHEDULE_SIZE_BYTES - 2 - 2 * X As X$
    Next

    Field #1, 38 As X$, 2 As Q$(24), 2 As Q$(25), 5 As Q$(22), 5 As Q$(23)

    For I = 1 To scheduleAP%(0)

        For X = 0 To 20:
            LSet Q$(X + 1) = MKI$(scheduleNG%(I, X))
        Next

        For X = 0 To 1:
            LSet Q$(22 + X) = RTrim$(scheduleYN$(I, X))
        Next

        LSet Q$(24) = MKI$(visitorScores(I))
        LSet Q$(25) = MKI$(homeScores(I))

        Put #1, I

    Next

    Close #1

End Sub


'----------------------------------------
'       ConvertSched4to5 Subroutine
'----------------------------------------
' Reads in existing schedule data.
' For each game in the schedule, it
' will migrate to the new format and
' populate with fixed defaults.
' From there, the updated data is saved.
Sub ConvertSched6to7 (targetFile$, silent)

    Shared scheduleNG%()

    newFile$ = GetFileBaseName$(targetFile$) + ".NEW"

    If targetFile$ <> "" Then

        Open targetFile$ For Random As #1
        fileLength& = LOF(1)
        Close #1

        numberGames = fileLength& / (SCHEDULE_SIZE_BYTES - 4)

        Call ReadSchedOld(targetFile$)

        For currGame = 1 To numberGames
            'Assign defaults
            scheduleNG%(currGame, 19) = 0
            scheduleNG%(currGame, 20) = 0
        Next currGame

        'Call SaveSchedFileConverted(newFile$, numberGames)

        'Kill targetFile$

        'result$ = CopyFile$(newFile$, targetFile$)

        'Kill newFile$

        Kill targetFile$

        Call SaveSchedFileConverted(targetFile$, numberGames)

        If silent = 0 Then
            result& = _MessageBox("Success!", "The schedule file has been converted for use", "ok", "info", 1)
        End If

    End If

End Sub


'----------------------------------------
'       ConvertAllSched Subroutine
'----------------------------------------
' Quick and dirty script to convert all
' schedule files in the "Schedule" folder
Sub ConvertAllSched ()

    fileSpec$ = diskPaths$(3) + "*.SCD"

    Count% = FileCount%(fileSpec$)
    ReDim foundFiles$(0 To Count%)
    foundFiles$(0) = fileSpec$
    Call ReadFile(foundFiles$())

    For X = 1 To Count%
        target$ = diskPaths$(3) + foundFiles$(X)
        Print "Converting "; target$
        Call ConvertSched6to7(target$, 1)
    Next X

End Sub
