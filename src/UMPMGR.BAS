'----------------------------------------
'           UMPMGR Subroutine
'----------------------------------------
'This subroutine gives the user options for
'working with umpire files.
Sub UMPMGR ()

    _Title "Full Count Baseball - Umpire Manager"

    Do

        Cls
        Color 15, 0

        Call TITLE

        ReDim menuChoices$(1 To 3)
        menuChoices$(1) = "ENTER A NEW UMPIRE FILE  "
        menuChoices$(2) = "EDIT EXISTING UMPIRE FILE"
        menuChoices$(3) = "RETURN TO MAIN MENU      "
        SELECTION% = 1
        Count% = 3
        Start% = 1
        ExitCode% = 3
        FileFGColor = 15: FileBGColor = 1
        HiLiteFGColor% = 0: HiLiteBGColor% = 7
        Spacing% = 6
        Row% = 8
        menuWidth% = Len(menuChoices$(1))
        Column% = (80 - menuWidth% - 2) / 2

        Call box0(Row% - 1, Column% - 1, Row% + Count%, Column% + menuWidth%, 2, FileFGColor, FileBGColor)

        _MouseShow "DEFAULT"
        MMenuVert menuChoices$(), SELECTION%, Start%, ExitCode%, FileFGColor, FileBGColor, HiLiteFGColor%, HiLiteBGColor%, Count%, Row%, Column%
        _MouseHide

        Color , 0

        If ExitCode% <> 27 Then

            Select Case SELECTION%
                Case 1: Call EnterUmpireFile
                Case 2: Call EditUmpireFile
            End Select

        End If

    Loop Until SELECTION% = 3 Or ExitCode% = 27

End Sub


'----------------------------------------
'       EnterUmpireFile Subroutine
'----------------------------------------
'This subroutine assists the user with
'creating a specific umpire file.
Sub EnterUmpireFile ()

    Dim umpireNames$(50), umpRating(50)

    For X = 1 To 50

        Cls
        Print "UMPIRE #"; X
        Input "ENTER UMPIRE NAME (END TO QUIT) "; umpireNames$(X)

        If UCase$(umpireNames$(X)) = "END" Then
            Erase umpireNames$
            Exit For
        Else
            Input "ENTER RATING (0-9) "; umpRating(X)
        End If

    Next

    Do
        Cls
        Print "IS THIS UMPIRING CREW (1)-AL OR (2)-NL ?"
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 1 And I <= 2 Or I$ = Chr$(27)

    If I$ <> Chr$(27) Then

        If I = 1 Then LG$ = "AL" Else LG$ = "NL"

        Open diskPaths$(0) + "UMPIRES." + LG$ For Output As #1

        For X = 1 To 50:
            Print #1, umpireNames$(X)
            Print #1, umpRating(X)
        Next

        Close #1

        Erase umpireNames$

        For X = 1 To 50:
            umpRating(X) = 5
        Next

    End If

End Sub

'----------------------------------------
'       EditUmpireFile Subroutine
'----------------------------------------
'This subroutine assists the user with
'editing specific umpires in the file.
Sub EditUmpireFile ()

    Dim umpireNames$(50), umpRating(50)

    Cls

    Print "IS THIS UMPIRING CREW (1)-AL OR (2)-NL ?"

    Do
        I$ = GetKeyPress$
        I = Val(I$)
    Loop Until I >= 1 And I <= 2

    If I = 1 Then LG$ = "AL" Else LG$ = "NL"

    If _FileExists(diskPaths$(0) + "UMPIRES." + LG$) Then

        Open diskPaths$(0) + "UMPIRES." + LG$ For Input As #1

        For X = 1 To 50:
            Input #1, umpireNames$(X)
            Input #1, umpRating(X)
        Next

        Close #1

    End If

    Cls

    For I = 1 To 10

        Locate I + 5, 1
        Print Using "##"; I;: Print " "; umpireNames$(I);
        Locate , 17
        Print Using "##"; I + 10;: Print " "; umpireNames$(I + 10);
        Locate , 33
        Print Using "##"; I + 20;: Print " "; umpireNames$(I + 20);
        Locate , 50
        Print Using "##"; I + 30;: Print " "; umpireNames$(I + 30);
        Locate , 67
        Print Using "##"; I + 40;: Print " "; umpireNames$(I + 40)
    Next

    Do
        Locate 18, 5
        Print "SELECT UMPIRE TO EDIT (0 TO QUIT) ";
        Input I
    Loop Until I >= 0 And I <= 50

    If I <> 0 Then

        Cls
        Locate 1, 30: Print "UMPIRE-"; I
        Locate , 1
        Locate 1: Print "1. NAME "; umpireNames$(I)
        Locate 2: Print "2. RATING "; umpRating(I)
        C1 = 1

        Do

            Do
                Locate 5: Input "# TO CHANGE (0 TO CONTINUE) "; B9
            Loop Until B9 >= 0 And B9 <= 2

            Select Case B9

                Case 1:
                    Locate 1
                    Input "1. NAME "; umpireNames$(I)

                Case 2:
                    Locate 2
                    Input "2. RATING "; umpRating(I)

            End Select

        Loop Until B9 = 0

    End If

End Sub

