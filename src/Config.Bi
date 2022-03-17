
Type Config
    MonTyp As Integer 'Monitor Type
    PulBar As Integer 'Horizontal Bar Color
    MenBox As Integer 'Pulled down Box Color
    ActivCh As Integer 'Active menu choice color
    InActCh As Integer 'Inactive (Non-selectable) Choice Color
    HiLite As Integer 'Menu Bar Color for active choices
    InActHiLt As Integer 'Dito for inactive choices
    NonMen As Integer 'Normal screen color
    CurSize As Integer 'Cursor Scan Lines
    Mouse As Integer 'Mouse in residence Flag
End Type

Dim Cnf As Config

'CALL InitMouse(Cnf.Mouse)

Conf.MonTyp = Monitor

ClrDisp% = Cnf.MonTyp = 3 Or (Cnf.MonTyp >= 5 And Cnf.MonTyp <= 9) Or Cnf.MonTyp = 11

If InStr(Command$, "/B") Then ClrDisp% = 0

If ClrDisp% Then
    Cnf.PulBar = 48
    Cnf.MenBox = 49
    Cnf.ActivCh = 48
    Cnf.InActCh = 52
    Cnf.HiLite = 31
    Cnf.InActHiLt = 64
    Cnf.NonMen = 30
Else
    Cnf.PulBar = 112
    Cnf.MenBox = 112
    Cnf.ActivCh = 112
    Cnf.InActCh = 112
    Cnf.HiLite = 15
    Cnf.InActHiLt = 80
    Cnf.NonMen = 7
End If

If Cnf.MonTyp >= 3 And Cnf.MonTyp <= 5 Then
    Cnf.CurSize = 7
Else
    Cnf.CurSize = 12
End If

