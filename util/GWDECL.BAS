' GWDECL.BAS - General Declares for Functions and Subroutines which pass
'              parameters by value.  Both of which are not automatic in QB.

' Copyright 1989, Brian C. Giedt

'Standard TYPE definitions for Graphics Workshop
TYPE Coord0
    XPos AS INTEGER
    YPos AS INTEGER
END TYPE

TYPE Coord1
    Col0 AS INTEGER
    Line0 AS INTEGER
END TYPE

TYPE Coord2
    XPos AS INTEGER
    YPos AS INTEGER
END TYPE

TYPE Window1
    x1 AS INTEGER
    y1 AS INTEGER
    x2 AS INTEGER
    y2 AS INTEGER
END TYPE

TYPE Window2
    Col1 AS INTEGER
    Line1 AS INTEGER
    Col2 AS INTEGER
    Line2 AS INTEGER
END TYPE


'***************************************************************************

'Standard DECLAREs for Graphics Workshop

'Assembly routines
DECLARE SUB PaintByte (BYVAL Col1%, BYVAL Line1%, BYVAL OldColor%, BYVAL NewColor%)
DECLARE SUB PaintBits (BYVAL Col1%, BYVAL Line1%, BYVAL BitPattern%, BYVAL OldColor%, BYVAL NewColor%)
DECLARE SUB TransferEGA (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%)
DECLARE SUB FadeEGA (BYVAL GridX%, BYVAL GridY%, BYVAL GridNumber%)
DECLARE SUB Fade2EGA (BYVAL GridX%, BYVAL GridY%, BYVAL GridNumber%)
DECLARE SUB GMove1VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB GMove2VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GMove3VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestCol%, BYVAL DestLine%, BYVAL SkipLines%)
DECLARE SUB GMove4VE (BYVAL FromCol%, BYVAL FromLine%, BYVAL Cols%, BYVAL Lines%, BYVAL DestSegment%, BYVAL Direction%)
DECLARE SUB GScrollVE (BYVAL Col1%, BYVAL Line1%, BYVAL Cols%, BYVAL Lines%, BYVAL ColDelta%, BYVAL LineDelta%)

'Graphics Primitives
DECLARE FUNCTION GetPointVE% (BYVAL X%, BYVAL Y%)
DECLARE SUB DrawPointVE (BYVAL X%, BYVAL Y%, BYVAL PointColor%)
DECLARE SUB DrawPointVEOpts (BYVAL X%, BYVAL Y%, BYVAL PointColor%)
DECLARE SUB DrawByteVE (BYVAL ScreenOffset%, BYVAL BitPattern%, BYVAL ByteColor%)
DECLARE SUB DrawByteVEOpts (BYVAL ScreenOffset%, BYVAL BitPattern%, BYVAL ByteColor%)
DECLARE FUNCTION MakeAddressVE% (BYVAL X%, BYVAL Y%, BitPattern%)
DECLARE SUB LineVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineToVE (BYVAL ToX%, BYVAL ToY%, BYVAL LineColor%)
DECLARE SUB LineToStepVE (BYVAL StepX%, BYVAL StepY%, BYVAL LineColor%)
DECLARE SUB LineStepVE (BYVAL StepX1%, BYVAL StepY1%, BYVAL StepX2%, BYVAL StepY2%, BYVAL LineColor%)
DECLARE SUB LineBVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBFVE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB LineBF2VE (BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL LineColor%)
DECLARE SUB CircleVE (BYVAL XCenter%, BYVAL YCenter%, BYVAL Radius%, BYVAL CircleColor%, BYVAL XAspect%, BYVAL YAspect%)
DECLARE SUB ClsVE ()

DECLARE SUB GPrint0VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint1VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint2VE (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3VE (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint0H (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint0C2 (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint0V256 (BYVAL Row%, BYVAL Col%, Text$, BYVAL TextColor%)
DECLARE SUB GPrint3V256 (BYVAL Line1%, BYVAL Col1%, Text$, BYVAL TextColor%)
DECLARE FUNCTION GetCharacter% (BYVAL Row%, BYVAL Col%)
DECLARE SUB ClearScreenArray ()

DECLARE FUNCTION GetPointH% (BYVAL X%, BYVAL Y%)
DECLARE SUB DrawPointH (BYVAL X%, BYVAL Y%, BYVAL PointColor%)
DECLARE FUNCTION MakeAddressH% (BYVAL X%, BYVAL Y%, BitPattern%)
DECLARE SUB ClsH

'Pixel routines with use a simple cache
DECLARE FUNCTION GetCacheVE% (BYVAL X%, BYVAL Y%)
DECLARE SUB ReDrawVE (BYVAL PointColor%)

'Mouse routines
DECLARE SUB InitMouse (MouseThere%)
DECLARE SUB ButtonPress (Button%, Status%, Count%, LastX%, LastY%)
DECLARE SUB GetCursor (X%, Y%, Button%)
DECLARE SUB GrafCursor (X%, Y%, Cursor$)
DECLARE SUB ShowCursor ()
DECLARE SUB HideCursor ()
DECLARE SUB Motion (Value%)
DECLARE SUB Mouse (AX%, BX%, CX%, DX%)
DECLARE SUB SetCursor (X%, Y%)

DECLARE FUNCTION MultMonitor%

DECLARE SUB CompletePalVGA (BYVAL NumColors%, BYVAL PalSeg%)
DECLARE SUB GetCompletePalVGA (BYVAL NumColors%, BYVAL PalSeg%)
DECLARE SUB GetPalTripleVGA (BYVAL PalReg%, Red%, Green%, Blue%)
DECLARE FUNCTION GetPaletteVGA% (BYVAL PalReg%)
DECLARE SUB SetPalTripleVGA (BYVAL PalReg%, BYVAL Red%, BYVAL Green%, BYVAL Blue%)
DECLARE SUB SetPaletteEGA (BYVAL PalReg%, BYVAL Value%)

DECLARE SUB SavePCXVE (Filename$)
'DECLARE FUNCTION SavePCXVE% (Filename$)   'To use these routines as functions
DECLARE SUB SavePCXC (Filename$)           ' follow the instructions in the
'DECLARE FUNCTION SavePCXC% (Filename$)    ' README file.
DECLARE SUB SavePCXH (Filename$)
'DECLARE FUNCTION SavePCXH% (Filename$)

DECLARE SUB SavePCXRegionVE (Filename$, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%)

DECLARE FUNCTION OpenPCXFile% (Filename$, Array$)
DECLARE SUB DispPCXC (BYVAL Display%)
DECLARE SUB DispPCXH (BYVAL Display%)
DECLARE SUB DispPCXVE (BYVAL Display%)
DECLARE SUB DispScrollPCXVE (BYVAL Display%, BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%)
DECLARE SUB PositionPCXVE (BYVAL LineNum%, BYVAL ColNum%)
DECLARE SUB DispPCXV256 (BYVAL Display%)
DECLARE SUB ClearVE ()

DECLARE FUNCTION QBinaryLoad% (Filename$, SEG Array%)
DECLARE SUB PCXArrayC (BYVAL ArraySeg%, BYVAL Display%)
DECLARE SUB PCXArrayH (BYVAL ArraySeg%, BYVAL Display%)
DECLARE SUB PCXArrayVE (BYVAL ArraySeg%, BYVAL Display%)
DECLARE SUB PCXArrayV256 (BYVAL ArraySeg%, BYVAL Display%)

DECLARE SUB Screen0 (KeepData%)
DECLARE SUB Screen1 (KeepData%)
DECLARE SUB Screen2 (KeepData%)
DECLARE SUB Screen3 (KeepData%)
DECLARE SUB Screen7 (KeepData%)
DECLARE SUB Screen9 (KeepData%)
DECLARE SUB Screen11 (KeepData%)
DECLARE SUB Screen12 (KeepData%)
DECLARE SUB Screen13 (KeepData%)
DECLARE FUNCTION GetScreenMode%

DECLARE SUB SetGWPages (BYVAL Source%, BYVAL Dest%)
DECLARE SUB SetSourcePage (BYVAL Source%)
DECLARE SUB SetDestPage (BYVAL Dest%)

DECLARE SUB SetLastCoord (BYVAL X%, BYVAL Y%)
DECLARE FUNCTION GetLastXCoord%
DECLARE FUNCTION GetLastYCoord%

DECLARE FUNCTION MBuffSize% ()
DECLARE SUB MGetState (X$)
DECLARE SUB MSetState (X$)

DECLARE SUB ScrnDump2 (DPI$, LptNumber%, Translate%, XMultiplier%, YMultiplier%, LandOrPort%)

DECLARE FUNCTION PDQTimer& ()
DECLARE FUNCTION GWFileSize& (Filename$)
DECLARE FUNCTION HercThere% ()
DECLARE FUNCTION AltKey% ()
DECLARE FUNCTION InStat% ()
DECLARE FUNCTION FindLast% (BYVAL Address%, Size%)

DECLARE SUB SlideDown (BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB SlideUp (BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB SlideLeft (BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB SlideRight (BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%, BYVAL DestCol%, BYVAL DestLine%)
DECLARE SUB SplitHorizontal (BYVAL Col1%, BYVAL Line1%, BYVAL Col2%, BYVAL Line2%, BYVAL DestCol%, BYVAL DestLine%)

'***************************************************************************

'BASIC routines

DECLARE FUNCTION MakeBinary% (Bin$)
DECLARE FUNCTION GetCountLocation% ()
DECLARE FUNCTION GetMouseCount% (Position%)
DECLARE SUB DisplayPCXFile (Filename$, VideoPage%)
DECLARE SUB DisplayPCXFile2 (Filename$, VideoPage%, StartLine%, StartCol%)
DECLARE SUB SetVideo ()

DECLARE SUB StepText (X%, Y%, Text$, Angle%, Colr%, TextSize#)
DECLARE SUB DrawText (X%, Y%, Text$, Angle%, Colr%, TextSize#)
DECLARE SUB OutlineText (X%, Y%, Text$, Angle%, Colr%, Mult%, Divide%)
DECLARE SUB LoadOutlineFont (FontFile$)
DECLARE SUB LoadFont (FontFile$)
DECLARE FUNCTION GetWidth% (Text$)
DECLARE FUNCTION GetOutlineWidth% (Text$)
DECLARE SUB SetGPFont (FontNumber%)
DECLARE SUB SetGWFont (FontNumber%)
DECLARE SUB SetGPSpacing (SpacingH%, SpacingV%)
DECLARE SUB SetGWSpacing (SpacingH%, SpacingV%)

DECLARE SUB GPaintBox (Col1%, Line1%, Col2%, Line2%, OldColor%, NewColor%)

DECLARE SUB BarPrintMSG (Choice$(), Stat%())
DECLARE SUB PullDnMSG (Choice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE FUNCTION PullMenKeyMSG% (Ky$, Choice$(), Stat%())

DECLARE SUB PullDownG (Choice$(), Stat%(), Menu%, Choice%, Ky$, Action%)
DECLARE SUB BarPrintG (Choice$(), BarClr%)

DECLARE SUB MsgBoxG (Message$, Wdth%)

DECLARE SUB VertMenuG (Item$(), Choice%, MaxLen%, BoxBot%, Ky$, Action%)

DECLARE SUB FillPoly (a%, b%, c%, d%, e%, f%, g%, h%, Colr%)
DECLARE SUB DrawCursor (X%, Y%, CursorWidth%, CursorLength%, CursorIs%, CursorTime%)


