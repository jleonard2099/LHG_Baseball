'********* GetVideo.Bas - determines monitor type, declares Shared variables

'Copyright (c) 1990 Brian Giedt

'DEFINT A-Z

'$INCLUDE: 'Common.GW'
ReDim Shared Font$(0, 0), FontWidth%(0, 0), FontHeight%(0)
ReDim Shared OutLineFont$(0, 0), FontSize%(0, 0), OutLineHeight%(0)
ReDim Shared Tile$(0), AltTile$(0), GPDat%(87), G$(8)
GPMonitor% = MultMonitor%
If GPMonitor% And &H200 Then 'Capabil of 8514A, use VGA 16 color
    GPDat%(31) = 8
ElseIf GPMonitor% And &H20 Then 'Capabil of VGA 16 color
    GPDat%(31) = 8
ElseIf GPMonitor% And &H10 Then 'Capabil of VGA mono
    GPDat%(31) = 7
ElseIf GPMonitor% And &H8 Then 'Capabil of EGA 16 color
    GPDat%(31) = 5
ElseIf GPMonitor% And &H4 Then 'Capabil of EGA mono
    GPDat%(31) = 1
ElseIf GPMonitor% And &H2 Then 'Capabil of CGA (Set to 2 color)
    GPDat%(31) = 6
ElseIf GPMonitor% And &H1 Then 'Capabil of HERC mono
    GPDat%(31) = 2
    If Not Hercthere% Then
        Cls
        Print "You must run QBHERC.COM before running this program."
        Print "Press any key to end.";
        While InKey$ = "": Wend
        End
    End If
End If

'These next two lines allow you to force certain screen modes to be used
'IF GPDat%(31) = 8 Then GPDat%(31) = 9     'Un' for VGA 256 color mode
'IF GPDat%(31) = 6 THEN GPDat%(31) = 4     'Un' for CGA 4 color mode
'setup only those elements of GPDat% we need set
GPDat%(23) = 0 'Boolean to set boldfacing
GPDat%(24) = 90 'Angle for Italicizing of Text
GPDat%(33) = 0 'Number of GraphPak Fonts
GPDat%(47) = 2 'Vertical Spacing for GraphPak Fonts
GPDat%(48) = 3 'Horizontal Spacing for GraphPak Fonts
GPDat%(57) = 0 'Overall Background Color (Used by GPaintBox)
GPDat%(71) = 14 'Default Lines used by a character font (EGA mode)
GPDat%(72) = &HAA00 'Default location to save images to when using the
' GMove2VE routine.
'Note: elements 73 through 80 are assigned below for use with the
'      pulldown and vertical menu systems.
GPDat%(81) = 0 'Graphics Workshop Fonts available
GPDat%(82) = 0 'Graphics Workshop Font Active
GPDat%(83) = 6 'Graphics Workshop Horizontal Font Spacing
GPDat%(84) = 5 'Graphics Workshop Vertical Font Spacing
GPDat%(85) = 0 'Graphics Workshop Overall Font Height
GPDat%(86) = 0 'Color in the font when drawing it

'    CALL InitMouse(GPDat%(73))

'Now set the colors for the pulldown, vertical menus, message boxes
If (GPDat%(31) = 5 Or GPDat%(31) = 8) And InStr(Command$, "/B") = 0 Then
    GPDat%(74) = 15 * 256 + 1 'Horizontal Bar Color
    GPDat%(75) = 15 * 256 + 1 'Pulled Down Box Color
    GPDat%(76) = 15 * 256 + 0 'Active Menu Choices
    GPDat%(77) = 15 * 256 + 7 'InActive (non-selectable) Menu Choices
    GPDat%(78) = 1 * 256 + 15 'Menu Bar Color for Active Choices
    GPDat%(79) = 4 * 256 + 0 'Menu Bar Color for InActive Choices
    GPDat%(80) = 1 * 256 + 14 'Normal Screen Color
Else
    GPDat%(74) = 7 * 256 + 0 'All variables serve the same purpose
    GPDat%(75) = 7 * 256 + 0 ' as described above, but are for the
    GPDat%(76) = 7 * 256 + 0 ' monochrome screen modes.
    GPDat%(77) = 7 * 256 + 0
    GPDat%(78) = 0 * 256 + 15
    GPDat%(79) = 5 * 256 + 0
    GPDat%(80) = 0 * 256 + 7
End If
