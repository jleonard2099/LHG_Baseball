'********** QEdit.Bas - this version is for QuickBASIC 4.0

'Copyright (c) 1988-1990 Crescent Software
'by Don Malin


DEFINT A-Z

'$INCLUDE: 'QEditype.BI'

TYPE BlkCoords                                  'Type for Block coordinates
    B AS INTEGER                                'Block coordinates
    PB AS INTEGER                               'Previous block coordinates
    P AS INTEGER                                'Paint coordinates
    PP AS INTEGER                               'Previous paint coordinates
END TYPE


'----- assembler calls
DECLARE SUB APrint0 (BYVAL X, NumEls, StartChar, NumChars, Colr)
DECLARE SUB APrintT0 (BYVAL Segment%, BYVAL Address%, StrLen%, NumEls%, FirstChar%, NumChars%, Clr%)
DECLARE SUB ButtonPress (Button, Status, Count, X, Y)
DECLARE SUB DeleteStr (BYVAL Address, NumEls)
DECLARE SUB Get1Str (Work$, SEG Element, StrNumber)
DECLARE SUB GetCursor (X, Y, Button)
DECLARE SUB GetVMode (Mode, Page, PageSize, Rows, Columns)
DECLARE SUB HideCursor ()
DECLARE SUB InsertStr (BYVAL Address, Inserted$, NumEls)
DECLARE SUB LoadHelp (BYVAL Segment%, BYVAL Address%)
DECLARE SUB MidStrSave (BYVAL Address, NumEls, FirstChar, NumChars, SEG Storage)
DECLARE SUB MidStrRest (Buffer$, StrNumber, SEG Storage)
DECLARE SUB MouseTrap (UlRow, UlCol, LrRow, LrCol)
DECLARE SUB MPaintBox (UlRow, UlCol, LrRow, LrCol, Colr)
DECLARE SUB MPRestore (UlRow%, UlCol%, LrRow%, LrCol%, OrigWidth%, SEG ArrayEl%)
DECLARE SUB MQPrint (X$, Colr)
DECLARE SUB MScrnSave (UlRow, UlCol, BRRow, BRCol, SEG Address)
DECLARE SUB MScrnRest (UlRow, UlCol, BRRow, BRCol, SEG Address)
DECLARE SUB Pause (Eighteenth)
DECLARE SUB QPrintRC (X$, Row%, Col%, Clr%)
DECLARE SUB ScrollD (UlRow%, UlCol%, BRRow%, BRCol%, Rows%, Page%)
DECLARE SUB ScrollL (UlRow%, UlCol%, BRRow%, BRCol%, Cols%, Page%)
DECLARE SUB ScrollR (UlRow%, UlCol%, BRRow%, BRCol%, Cols%, Page%)
DECLARE SUB ScrollU (UlRow%, UlCol%, BRRow%, BRCol%, Rows%, Page%)
DECLARE SUB ShowCursor ()
DECLARE SUB SplitColor (AColor, Fg, Bg)
DECLARE SUB StringSave (BYVAL Address, SEG Element, NumEls)


'----- ASM Functions
DECLARE FUNCTION ArraySize% (UlRow, UlCol, BRRow, BRCol)
DECLARE FUNCTION ASCII% (Ky$)
DECLARE FUNCTION Blanks% (Text$)
DECLARE FUNCTION DOSVer% ()
DECLARE FUNCTION FindLast% (BYVAL Address, Size)
DECLARE FUNCTION MaxInt% (Num1, Num2)
DECLARE FUNCTION MinInt% (Num1, Num2)
DECLARE FUNCTION Monitor% ()
DECLARE FUNCTION Null% (Text$)
DECLARE FUNCTION PeekBuf% ()
DECLARE FUNCTION QInstrB% (Start%, Text$, Search$)
DECLARE FUNCTION QPLen% (Text$)
DECLARE FUNCTION ShiftKey% ()
DECLARE FUNCTION StringSize& (BYVAL Address, NumEls)
DECLARE FUNCTION StrLength% (SEG Element, NumBytes AS ANY, StrNumber)

CONST NumPad$ = "12346789"                      'Shifted arrow keys
CONST NumPad2$ = "stOPQKMGHK"                   'Unshifted arrow keys
CONST SkipTbl$ = " ^*()[]-=+,.<>/\"             'Word delimiters


'----- Returns the number of columns to advance at the end of a line
DEF FnSpaces2Pad (Text$)
    STATIC Temp

    SELECT CASE ASCII(RIGHT$(Text$, 1))
       CASE 46, 33, 63          '".", "!", "?"
          Temp = 2
       CASE 32, -1              'Space
          Temp = 0
       CASE ELSE
          Temp = 1
    END SELECT
    FnSpaces2Pad = Temp
END DEF

'****************************************************************************
'  Title: QEdit - Full featured text editing sub-program
'     by: Don Malin  10/21/88
'
'Purpose: Displays a word processing window and allows the user to edit text.
'         Horizontal and vertical scrolling can be used to edit a string
'         array passed to the routine.  Margins and word wrap are supported
'         along with full block operations and mouse support.
'
' Syntax: Call QEdit (Arg1$(), Arg2$, Arg3%, Arg4 as EditInfo)
'
' Inputs: Arg1$(1) - Text array to be edited.  Must be a conventional
'                    (not fixed-length) string array.
'         Arg2$    - Last Key pressed
'         Arg3%    - Action flag  (0 = Take full control, return only on Esc
'                                 (1 = Init window, save underlying screen)
'                                 (2 = Re-display but don't save window)
'                                 (3 = Just check keyboard and return
'                                 (5 = Restore underlying screen)
'         Arg4     - Structure defined in QEdit.Bas MAIN and the Caller's
'                    MAIN (see $INCLUDE file - QEditype.BI)
'
'  Notes: It is up to you to determine the probable amount of string space
'         required for your text array.
'
'         If you don't want to support block operations, the code used to
'         perform them is contained in a single block which can be deleted.
'         Search for "Block Operations".  The same is true for mouse handling
'         code.  Search for "Mouse Handling".
'****************************************************************************
'
SUB QEdit (Array$(), X$, action%, Ed AS EditInfo) STATIC

    Ed.MErr = 0                              'No errors yet

    DIM Bl(3) AS BlkCoords

    '----- Action of 0 or 1 - Save the underlying screen
    IF action < 2 THEN
       WrapWas = Ed.Wrap
       Ed.HTab = MaxInt(Ed.HTab, 1)             'Horizontal Tab spaces
       TMarg = 1
       BMarg = 1
       Insrt = Ed.InsStat
       IF Insrt = 0 THEN Insrt = -1             'Insert mode set to ON
                                                'make 1 to default to OT
       CtrlP = 32

       CurLine$ = SPACE$(4)                     'Used to display Cursor Pos.
       CurCol$ = SPACE$(3)

       MouseKey$ = CHR$(0) + CHR$(3)            'Dummy key for mouse actions
       MouseKey2$ = CHR$(0) + CHR$(4)
      
       '----- Compose the Ruler line string
       REDIM Ruler(0 TO 1) AS STRING * 260      'Put it out in far memory

       FOR N = 0 TO 250 STEP 10                 'Compose Ruler line
           MID$(Ruler(0), N + 1) = MID$(STR$(N MOD 100), 2, 1)
           FOR X = 1 TO 9
               IF X = 5 THEN
                  MID$(Ruler(0), N + X + 1) = "Å"
               ELSE
                  MID$(Ruler(0), N + X + 1) = "Â"
               END IF
           NEXT
       NEXT

    
       MouseThere = 0                           'Assume Mouse code absent
       BlocksThere = 0                          'Assume Block code absent


       '----- Determine bottom cursor scan line.
       SELECT CASE Monitor%                     'Determine the monitor type
          CASE 3, 5, 10                         'CGA, EGA monitors
             BScan = 7                          'Set bottom scan line
          CASE ELSE                             'All other monitors
             BScan = 12
       END SELECT
       LOCATE , , 0, BScan - 1, BScan

       NPixLines = 8
       GetVMode 0, 0, 0, ScrRows, 80            'Get current line mode
                                                'Fix for OS2 DOS
       IF ScrRows = 50 AND DOSVer = 1000 THEN NPixLines = 7

       Ed.TSRow = CSRLIN - (Ed.Frame <> 0)      'Calc the top line of window
       Ed.LSCol = POS(0) - (Ed.Frame <> 0)      'Calc Left margin of window
                                                'Both are to inside of frame
       Ed.Rows = MinInt(MaxInt(Ed.Rows, 3), (ScrRows + ((Ed.Frame <> 0) * 2)) - CSRLIN + 1)
       Ed.Wide = MinInt(MaxInt(Ed.Wide, 11), (80 + ((Ed.Frame <> 0) * 2)) - POS(0) + 1)

       '----- Save current window coordinates for later resizing
       DIM WC(3, 1)
       WC(0, 1) = Ed.TSRow + (Ed.Frame <> 0)    'Top line
       WC(1, 1) = Ed.LSCol + (Ed.Frame <> 0)    'Left margin
       WC(2, 1) = WC(0, 1) + Ed.Rows + (2 * (Ed.Frame <> 0)) - 1'Bottom line of window
       WC(3, 1) = WC(1, 1) + Ed.Wide + (2 * (Ed.Frame <> 0)) - 1'Right margin of window
      
       '----- Dim array to hold the underlying screen and save it.
       REDIM ScrBuf(1 TO (ScrRows * 80) + 2)
       MScrnSave 1, 1, ScrRows, 80, ScrBuf(1)
      
       IF Ed.LC <= 0 THEN Ed.LC = 1             'Set window column to 1
       IF Ed.CurCol <= 0 THEN Ed.CurCol = 1     'establish cursor column at 1
       IF Ed.TL <= 0 THEN Ed.TL = 1             'Top of window row number
       IF Ed.CurLine <= 0 THEN Ed.CurLine = 1   'make top line the cursor col.
       GOSUB WindowLimits

    END IF

    '----- Action of 5 - Restore the underlying screen
    IF action = 5 THEN
       GOSUB CleanUp                            'Restore the screen
       action = 1                               'Reset action to 1
       EXIT SUB                                 'Bail out
    END IF
   
    '----- Action of 0, 1, 2 - Display the edit window
    IF action < 3 OR action = 4 THEN

       '----- Find the last element being used in the text array
       ArrayEnd = UBOUND(Array$)
       Ed.LCount = FindLast%(VARPTR(Array$(ArrayEnd)), ArrayEnd)


       IF Ed.Wrap THEN     'If word wrap is on, use "" paragraph marker
          Par$ = CHR$(20)  '(you could use CHR$(255) for invisible marker)
          Ed.Wrap = MaxInt(MinInt(Ed.Wrap, 255), 10)
       ELSE                'Otherwise,
          Par$ = ""        '  use a null character
       END IF
  
       '----- If word wrap is on, format all the text.
       IF Ed.Wrap THEN
          WrapLine = 1
          IF action < 3 THEN
             WrapLine = 1
             GOSUB WrapAll
          ELSE
             WrapLine = Ed.CurLine
             
             GOSUB WrapUp
             GOSUB WrapDown
          END IF
          WrapWas = Ed.Wrap
       END IF
      
                          
       '----- Insert margin markers in ruler line.
       Ruler(1) = Ruler(0)
       IF Ed.Wrap THEN
          MID$(Ruler(1), 2) = CHR$(16)
          MID$(Ruler(1), Ed.Wrap + 1) = CHR$(17)
       END IF

       HiClr = Ed.AColor                        'Determine high intensity Clr
       SplitColor Ed.AColor, X, Temp            'Split color # into Fg & Bg
       IF X < 8 THEN HiClr = Ed.AColor + 8
       IF Ed.AColor = 112 THEN
          BlkClr = 7
       ELSE
          BlkClr = 112
       END IF

       GOSUB InitWindow                         'Display the window

       GOSUB CsrSize                            'Set the cursor size
      
    END IF

    

    '----- Main editing (key processing) loop
    DO

        '----- Position the cursor
        LOCATE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol + (Ed.CurCol - Ed.LC), 1
       
        Ed.UnKnownKey = 0
        LineWas = Ed.CurLine

        '----- Handle regular keys
        IF QPLen(X$) = 1 THEN
    
           SELECT CASE ASCII(X$)

              '----- All Non Control Characters
              CASE IS >= CtrlP

                 X = QPLen(Array$(Ed.CurLine))  'See if line is long enough
                 IF X < 256 AND Ed.CurCol < 256 THEN
                    IF Ed.CurLine < ArrayEnd OR Ed.Wrap = 0 OR X < Ed.Wrap THEN
                       IF Ed.CurCol > X THEN    'Make space for new character
                          Array$(Ed.CurLine) = Array$(Ed.CurLine) + SPACE$(Ed.CurCol - X)
                       END IF
                                                'Update line count if past
                       Ed.LCount = MaxInt%(Ed.LCount, Ed.CurLine) ' end
                                                'Insert character in line
                       IF Insrt = -1 AND Ed.CurCol <= X THEN
                          Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1) + X$ + MID$(Array$(Ed.CurLine), Ed.CurCol)
                          MQPrint MID$(Array$(Ed.CurLine), Ed.CurCol, Ed.Wide - (Ed.CurCol - Ed.LC)), Ed.AColor
                          IF Ed.Wrap THEN
                             WrapLine = Ed.CurLine
                             GOSUB WrapDown
                          END IF
                       ELSE                     'Concat character to end
                          MID$(Array$(Ed.CurLine), Ed.CurCol, 1) = X$
                          MQPrint X$, Ed.AColor 'Print the character
                          IF Ed.Wrap AND Ed.CurCol > Ed.Wrap THEN
                             WrapLine = Ed.CurLine
                             GOSUB WrapDown
                          END IF
                       END IF

                       Ed.CurCol = Ed.CurCol + 1'Update the current column
                       GOSUB WindowLimits
                       Ed.Changed = -1          'We just changed the text
                       LineEd = -1
                    END IF
                 END IF

                 CtrlP = 32


              '----- Back Space
              CASE 8
                 IF Ed.CurCol > 1 THEN          'If not at column 1,
                    Ed.CurCol = Ed.CurCol - 1   '  decrement the cursor posit.
                    IF Insrt = -1 THEN          'If in Insert mode, drag the
                                                '  line 1 char. left
                       Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1) + MID$(Array$(Ed.CurLine), Ed.CurCol + 1)
                       IF LEN(Array$(Ed.CurLine)) THEN Ed.Changed = -1
                       IF Ed.Wrap THEN
                          WrapLine = Ed.CurLine
                          GOSUB WrapUp          'Wrap words up
                       END IF
                                                'Not in Insert mode, blank char
                    ELSEIF Ed.CurCol = QPLen(Array$(Ed.CurLine)) THEN
                       Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1)
                       Ed.Changed = -1
                    ELSEIF Ed.CurCol < QPLen(Array$(Ed.CurLine)) THEN
                       MID$(Array$(Ed.CurLine), Ed.CurCol, 1) = " "
                       Ed.Changed = -1
                       LineEd = -1
                    END IF                      'Put edited line in a buffer
                    IF Ed.CurCol < Ed.LC THEN  'If we're at left window col,
                       Ed.LC = Ed.LC - 1        '  decrement window column
                       RePrint = -1
                    ELSE
                       LSET LineBuf$ = MID$(Array$(Ed.CurLine), Ed.LC)
                       LOCATE , Ed.LSCol, 0        'Print the edited line
                       MQPrint LineBuf$, Ed.AColor
                    END IF
                                                'If in Insert mode, go up
                 ELSEIF Ed.CurLine > 1 THEN
                                                
                    Temp = FnSpaces2Pad(Array$(Ed.CurLine - 1))
                                                'Copy line to line above
                    Ed.CurCol = QPLen(Array$(Ed.CurLine - 1)) + Temp + 1
                    Array$(Ed.CurLine - 1) = Array$(Ed.CurLine - 1) + SPACE$(Temp) + Array$(Ed.CurLine)
                                                'Update window column
                    Ed.LC = MaxInt%(Ed.CurCol - Ed.Wide + 1, Ed.LC)

                    Temp = Ed.CurLine           'Delete the current line
                    GOSUB DeleteLine
                   
                    Ed.CurLine = Ed.CurLine - 1
                    GOSUB CursorUp              'Back up a line
                    RePrint = -1                'Set flag to re-print window
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapDown           'Wrap words down
                    END IF
                    Ed.Changed = -1
                 END IF
                  
              '----- Enter
              CASE 13
                 IF Ed.CurLine < ArrayEnd THEN

                    X = Blanks(Array$(Ed.CurLine)) + 1
                                                'If in Insert mode, insert a
                    IF Insrt = -1 THEN
                                                '  new line below.
                       IF Ed.LCount < ArrayEnd THEN
                          Ed.LCount = Ed.LCount + 1
                                                'New line is right of cursor
                          Temp$ = SPACE$(MinInt(X, Ed.CurCol) - 1) + RTRIM$(MID$(Array$(Ed.CurLine), Ed.CurCol))
                          Temp = Ed.CurLine + 1
                          GOSUB InsertLine
                                                'Current line is left of
                                                '  cursor & paragraph marker
                          Array$(Ed.CurLine) = RTRIM$(LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1))
                          Ed.Changed = -1
                       END IF
                    END IF
                     
                    IF X > 1 OR QPLen(Array$(Ed.CurLine)) THEN Ed.CurCol = X
                    Ed.CurLine = MinInt%(Ed.CurLine + 1, ArrayEnd)
                    GOSUB WindowLimits

                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- TAB
              CASE 9
                                                'Make current column "Ed.HTab"
                                                '  to the right
                 N = ((Ed.CurCol + Ed.HTab - 1) \ Ed.HTab) * Ed.HTab + 1
                 N = MinInt(N, 255 - Ed.HTab)
                 IF N <= Ed.Wrap OR Ed.Wrap = 0 THEN
                                                'If in Insert mode
                    IF Insrt = -1 AND Ed.CurCol < QPLen(Array$(Ed.CurLine)) THEN
                                                '  insert "Ed.HTab" spaces
                       Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1) + SPACE$(N - Ed.CurCol) + MID$(Array$(Ed.CurLine), Ed.CurCol)
                       IF Ed.Wrap THEN
                          WrapLine = Ed.CurLine
                          GOSUB WrapDown        'Wrap words down
                       END IF
                       Ed.Changed = -1
                    END IF
                                               
                    Ed.CurCol = N
                                                'Is it off the right of window?
                    IF Ed.CurCol > Ed.LC + Ed.Wide - 1 THEN
                       Ed.LC = Ed.LC + Ed.HTab  'Make it "Ed.HTab" to the right
                       RePrint = -1             'Set flag to re-print window
                    ELSE                        'Move to the left margin
                       LOCATE , Ed.LSCol, 0
                                                'Put new line in buffer
                       LSET LineBuf$ = MID$(Array$(Ed.CurLine), Ed.LC)
                                                'Print the buffer
                       MQPrint LineBuf$, Ed.AColor
                    END IF
                 END IF

              '----- Escape
              CASE 27
                 IF action = 0 THEN             'If not using Action flag,
                    GOSUB CleanUp               '  restore the screen and
                                                'Clean up memory
                    EXIT DO                     'see ya later oscillator
                 ELSE
                    Ed.UnKnownKey = -1
                 END IF
                 

              '----- Ctrl Y - Delete a Line
              CASE 25
                 IF Ed.CurLine <= Ed.LCount THEN 'Disallow if we're past the last line
                                                'Calc the screen line number
                    X = Ed.TSRow + Ed.CurLine - Ed.TL
                                                'Paint the line before we delete it
                                                '  just for effect
                    MPaintBox X, Ed.LSCol, X, Ed.LSCol + Ed.Wide - 1, BlkClr

                    Temp = Ed.CurLine           'Delete the current line
                    GOSUB DeleteLine

                    Pause 1                     'Wait an 1/18 of a second
                    RePrint = -1                'Set flag to re-print window
                    IF Ed.LCount THEN Ed.Changed = -1
                 END IF
                
            
              '----- Ctrl N - Insert a Line
              CASE 14
                 Temp = Ed.CurLine
                 Temp$ = ""
                 GOSUB InsertLine
                 RePrint = -1                   'Set flag to re-print window
                 IF Ed.LCount THEN Ed.Changed = -1


              '----- Ctrl P - Imbed characters
              CASE 16
                 CtrlP = 0
                 LineEd = -1

              CASE ELSE
                 Ed.UnKnownKey = -1

           END SELECT

           
           X& = FRE("")
           IF X& < 1280 THEN
              BEEP
              Ed.MErr = 1                       'Set error flag
           END IF


        '----- Handle Extended (2 Char) key codes
        ELSEIF QPLen(X$) = 2 THEN

           SELECT CASE ASCII(RIGHT$(X$, 1))     'Get ASCII value of right 1

              '----- HELP key pressed
              CASE 59
                                                'Make array for underlying Scr
                 REDIM HelpScr(ArraySize%(2, 80 - 57 - 1, 12, 80))
                                                'Save underlying screen
                 MScrnSave 2, 80 - 57 - 1, 12, 80, HelpScr(1)

                 REDIM Help(35) AS STRING * 57
                                                'Load array with Help text
                 LoadHelp VARSEG(Help(1)), VARPTR(Help(1))

                 LOCATE 2, 80 - 57 - 1, 0       'Locate the cursor
                                               
                 HideCursor                     'Print the help screen
                 APrintT0 VARSEG(Help(1)), VARPTR(Help(1)), 57, 10, 1, 57, 112'79
                 ShowCursor
                                                'Paint Shadows
                 MPaintBox 3, 79, 11, 80, 8
                 MPaintBox 12, 80 - 57 + 1, 12, 80, 8
                 LOCATE 10, 78
                 IF NOT MouseThere AND NOT BlocksThere THEN
                    MQPrint "º", 79
                 ELSE
                    MQPrint CHR$(18), 240
                 END IF

                 LOCATE 3, 80 - 57 - 1          'Locate the cursor
                 I = 2
                 DO                             'Wait for a key press
                    Temp = PeekBuf
                    SELECT CASE Temp            'Get key but don't remove it
                       CASE -72, -73            'Up arrow or PgUp
                          SELECT CASE I         'Set pointer based on last one
                             CASE 27
                                IF BlocksThere THEN
                                   I = 19
                                ELSE
                                   I = 2
                                END IF
                             CASE 19
                                I = 11
                             CASE 11
                                I = 2
                             CASE 2
                                IF MouseThere THEN
                                   I = 27
                                ELSEIF BlocksThere THEN
                                   I = 11
                                END IF
                          END SELECT
                       CASE -80, -81            'Down arrow or PgDn
                          SELECT CASE I         'Set pointer based on last one
                             CASE 2
                                IF BlocksThere THEN
                                   I = 11
                                ELSEIF MouseThere THEN
                                   I = 27
                                END IF
                             CASE 11
                                IF MouseThere THEN
                                   I = 19
                                ELSE
                                   I = 2
                                END IF
                             CASE 19
                                I = 27
                             CASE 27
                                I = 2
                          END SELECT

                       CASE 27                  'Escape key
                          Temp$ = INKEY$        'Take key out of buffer
                          EXIT DO               'Bail out
                       CASE 0
                       CASE ELSE                'Bail out if any other key
                          EXIT DO
                    END SELECT

                    IF Temp THEN
                       Temp$ = INKEY$           'Now extract key from buffer
                       HideCursor               'Display new page of help
                       APrintT0 VARSEG(Help(I)), VARPTR(Help(I)), 57, 8, 1, 56, 112'79
                       ShowCursor
                    END IF
                                                'Bail out if mouse button pressed
                    ButtonPress 1, 0, Ed.Presses, X, Y
                 LOOP UNTIL Ed.Presses
                                                'Restore the screen
                 MScrnRest 2, 80 - 57 - 1, 12, 80, HelpScr(1)
                 ERASE HelpScr, Help            'Clean up memory


              '----- Cursor UP
              CASE 72
                 IF PeekBuf = -72 THEN
                    Ed.CurLine = Ed.CurLine - 2
                    X$ = INKEY$
                 ELSE
                    Ed.CurLine = Ed.CurLine - 1
                 END IF
                 GOSUB CursorUp                 'Go do it

              '----- Cursor DOWN
              CASE 80
                 IF PeekBuf = -80 THEN
                    Ed.CurLine = Ed.CurLine + 2
                    X$ = INKEY$
                 ELSE
                    Ed.CurLine = Ed.CurLine + 1
                 END IF
                 GOSUB CursorDown               'Go do it

              '----- Cursor LEFT
              CASE 75
                 IF Ed.CurCol > 1 THEN
                    IF PeekBuf = -75 THEN
                       Ed.CurCol = Ed.CurCol - 2
                       X$ = INKEY$
                    ELSE
                       Ed.CurCol = Ed.CurCol - 1
                    END IF
                    GOSUB CursorLeft
                                                'Are we in word wrap mode?
                 ELSEIF Ed.Wrap AND Ed.CurLine > 1 THEN
                    Ed.CurCol = Ed.Wrap         'Set column to right margin
                    Ed.LC = MaxInt%(Ed.Wrap - Ed.Wide + 1, 1)
                    Ed.CurLine = Ed.CurLine - 1
                    GOSUB CursorUp              'Let "CursorUp" handle it
                    RePrint = -1
                    ColBlock = 0
                 END IF

              '----- Cursor RIGHT
              CASE 77
                 IF Ed.CurCol < 256 THEN
                    IF PeekBuf = -77 THEN
                       Ed.CurCol = Ed.CurCol + 2
                       X$ = INKEY$
                    ELSE
                       Ed.CurCol = Ed.CurCol + 1
                    END IF
                    GOSUB CursorRight
                                                'If at right margin
                    IF Ed.Wrap AND Ed.CurCol > Ed.Wrap THEN
                       Ed.CurCol = 1            'Set current column to 1
                       Ed.LC = 1                'Set window column to 1
                       Ed.CurLine = Ed.CurLine + 1
                       GOSUB CursorDown         'Let "CursorDown" move down
                                                '  a line
                       RePrint = -1
                       ColBlock = 0
                    END IF
                 END IF

              '----- Ctrl LEFT - move one word left
              CASE 115
                 DO
                    IF Ed.CurCol = 1 AND Ed.CurLine > 1 THEN
                       Ed.CurLine = Ed.CurLine - 1
                       Ed.CurCol = MaxInt(QPLen(Array$(Ed.CurLine)), 1)
                       ColBlock = 0
                    END IF
                      
                    X = -1
                    DO WHILE Ed.CurCol > 1
                       Ed.CurCol = Ed.CurCol - 1
                       IF X THEN
                          IF INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1)) = 0 THEN X = 0
                       ELSE
                          IF INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1)) THEN
                             Ed.CurCol = Ed.CurCol + 1
                             EXIT DO
                          END IF
                       END IF
                    LOOP
                 LOOP WHILE Ed.CurLine > 1 AND INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1))
                
                 GOSUB WindowLimits             'If off left of window, adjust

              '----- Ctrl RIGHT - move one word right
              CASE 116
                 DO
                    X = -1
                    DO WHILE Ed.CurCol <= QPLen(Array$(Ed.CurLine))
                       Ed.CurCol = Ed.CurCol + 1 'Increment column position
                       IF X THEN
                          IF INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1)) THEN X = 0
                       ELSE
                          IF INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1)) = 0 THEN EXIT DO
                       END IF
                    LOOP
                                                'If past end of line,
                    IF Ed.CurCol > QPLen(Array$(Ed.CurLine)) AND Ed.CurLine < Ed.LCount THEN
                       Ed.CurCol = 1            '  Set current column to 1
                       Ed.CurLine = Ed.CurLine + 1 'Increment line number
                       ColBlock = 0
                    ELSE
                       EXIT DO
                    END IF
                 LOOP WHILE INSTR(SkipTbl$, MID$(Array$(Ed.CurLine), Ed.CurCol, 1))
                                                'If off right of window, adjust
                 GOSUB WindowLimits

              '----- Cursor HOME
              CASE 71                           'Starting at column 1
                 X = Blanks%(Array$(Ed.CurLine)) + 1

                 IF X = Ed.CurCol THEN X = 1
                 Ed.CurCol = X

                 GOSUB WindowLimits
                                 

              '----- Cursor END
              CASE 79                           'Set current column to end +
                 Ed.CurCol = QPLen(Array$(Ed.CurLine)) + FnSpaces2Pad(Array$(Ed.CurLine)) + 1

                 GOSUB WindowLimits

              '----- Ctrl HOME
              CASE 119                          'Make current line top of screen
                 Ed.CurLine = Ed.CurLine - (CSRLIN - Ed.TSRow)

              '----- Ctrl END
              CASE 117                          'Make bottom current line
                 Ed.CurLine = Ed.CurLine + ((Ed.TSRow + Ed.Rows - 1) - CSRLIN)

              '----- PgUp
              CASE 73
                 IF Ed.TL > 1 THEN              'ignore if already at the top
                    X = Ed.TL                   'save Ed.TL for a moment
                    Ed.TL = MaxInt%(1, Ed.TL - Ed.Rows)
                    X = X - Ed.TL               'calc dif. between new and old
                    Ed.CurLine = Ed.CurLine - X 'don't move cursor unless we have to
                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- PgDn
              CASE 81                           'almost at end, work backwards
                 IF Ed.TL > ArrayEnd - Ed.Rows * 2 THEN
                    X = Ed.CurLine - Ed.TL      '  from end of array
                    Ed.TL = ArrayEnd - Ed.Rows + 1
                    Ed.CurLine = Ed.TL + X      'Calc new current line
                    RePrint = -1                'Set flag to re-print window
                 ELSE
                    Ed.TL = Ed.TL + Ed.Rows     'calc top line of next page
                    Ed.CurLine = Ed.CurLine + Ed.Rows 'update current line
                    RePrint = -1                'Set flag to re-print window
                 END IF

              '----- Ctrl PgUp
              CASE 132
                 Ed.CurCol = 1                  'Make column 1 current
                 Ed.CurLine = 1                 'ditto for the current line
                 Ed.TL = 1                      'Set current top of screen
                 Ed.LC = 1                      'Set current left of screen
                 RePrint = -1                   'Set flag to re-print window

              '----- Ctrl PgDn
              CASE 118                          'we're already there - ignore
                 IF Ed.TL <= ArrayEnd - Ed.Rows THEN
                    IF Ed.LCount > 0 THEN       'empty file - go to the top
                                                'Cursor past the end, but see
                       IF Ed.CurLine > Ed.LCount THEN '  exactly where we are
                          X = Ed.CurLine - Ed.LCount
                                                'Last line showing on screen
                          IF X < Ed.Rows AND Ed.TL <= Ed.LCount THEN
                                                '  make that the current line
                             Ed.CurLine = Ed.LCount
                             LOCATE CSRLIN - X  '  and put the cursor there
                          END IF                '  too
                       END IF

                       Ed.CurLine = Ed.LCount   'Make the last line current
                       X = Ed.CurLine - Ed.TL   'See where cursor is relative
                                                '  to end
                                                'Last line is on the screen,
                       IF X <= 0 OR X >= Ed.Rows THEN
                                                '  don't re-display, just move
                                                'establish top line to display
                          Ed.TL = Ed.CurLine - (Ed.Rows - 1)
                                                'Can't have negative line #
                          Ed.TL = MaxInt%(Ed.TL, 1)
                          RePrint = -1          'Set flag to re-print window
                       END IF
                                                'Locate at end of line
                       Ed.CurCol = QPLen(Array$(Ed.CurLine)) + 1
                       IF Ed.CurCol < Ed.LC THEN
                          Ed.LC = MaxInt(1, Ed.CurCol - Ed.Wide + 1)
                          RePrint = -1
                       ELSEIF Ed.CurCol > Ed.LC + Ed.Wide - 1 THEN
                          Ed.LC = Ed.CurCol - Ed.Wide + 1
                          RePrint = -1
                       END IF

                    END IF
                 END IF

              '----- Togle INSERT mode
              CASE 82
                 Insrt = Insrt * -1             'toggle insert on and off
                 GOSUB CsrSize                  'set cursor size accordingly

              '----- Delete
              CASE 83
                 IF Ed.CurCol > QPLen(Array$(Ed.CurLine)) THEN
                    Array$(Ed.CurLine) = Array$(Ed.CurLine) + SPACE$(Ed.CurCol - QPLen(Array$(Ed.CurLine)) - 1)

                    'if not on last line concatenate the next line to this one
                    IF Ed.CurLine < ArrayEnd THEN Array$(Ed.CurLine) = Array$(Ed.CurLine) + LTRIM$(Array$(Ed.CurLine + 1))

                    Temp = Ed.CurLine + 1
                    GOSUB DeleteLine

                    RePrint = -1                'Set flag to re-print window
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapDown           'Wrap words down
                    END IF
                 ELSE                           'Within line, delete current
                                                '  character
                    Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), Ed.CurCol - 1) + MID$(Array$(Ed.CurLine), Ed.CurCol + 1)
                                                'Put new text in line buffer
                    LSET LineBuf$ = MID$(Array$(Ed.CurLine), Ed.LC)
                    LOCATE , Ed.LSCol, 0        'Print the line buffer
                    MQPrint LineBuf$, -1
                 END IF
                 IF Ed.Wrap THEN
                    WrapLine = Ed.CurLine
                    GOSUB WrapUp                'Wrap words up
                 END IF
                 Ed.Changed = -1
                 LineEd = -1

                  
              '----- Back TAB
              CASE 15                           'Calc spaces to next stop
                 X = (((Ed.CurCol - Ed.HTab - 2) \ Ed.HTab) + 1) * Ed.HTab + 1
                 X = MaxInt(X, 1)
                                                'If in Insert mode and not at
                                                '  end of line,
                 IF Insrt = -1 AND Ed.CurCol <= QPLen(Array$(Ed.CurLine)) THEN
                                                'drag line to the left
                    Array$(Ed.CurLine) = LEFT$(Array$(Ed.CurLine), X - 1) + MID$(Array$(Ed.CurLine), Ed.CurCol)
                    IF Ed.Wrap THEN
                       WrapLine = Ed.CurLine
                       GOSUB WrapUp             'Wrap words up
                    END IF
                    Ed.Changed = -1
                 END IF

                 Ed.CurCol = X                  'Adjust current column posit.

                 IF Ed.CurCol < Ed.LC THEN      'If off left of window,
                    Ed.LC = Ed.CurCol           '  adjust current window col
                    RePrint = -1                'Set flag to re-print window
                 ELSE                           'Otherwise,
                    LOCATE , Ed.LSCol, 0 'locate at left margin
                                                'Put new text in line buffer
                    LSET LineBuf$ = MID$(Array$(Ed.CurLine), Ed.LC)
                    MQPrint LineBuf$, Ed.AColor 'Print the line buffer
                 END IF
               
              CASE ELSE                         'Ignor all other keys
                 Ed.UnKnownKey = -1

           END SELECT

        END IF

       
        IF LineEd AND Ed.CurLine <> LineWas THEN
           Array$(LineWas) = RTRIM$(Array$(LineWas))
           LineEd = 0
        END IF


        '----- Re-display the window if flag set or block marking complete
        IF RePrint OR (Ed.WasMarked AND MarkBlock = 0) THEN
           LOCATE Ed.TSRow, Ed.LSCol, 0         'APrint the screen
           HideCursor                           'Turn Mouse cursor off
           APrint0 VARPTR(Array$(Ed.TL)), Ed.Rows, Ed.LC, Ed.Wide, Ed.AColor
           ShowCursor                           'Turn mouse cursor back on
           RePrint = 0                          'Turn Flag off
           IF MarkBlock = 0 THEN Ed.WasMarked = 0  'Turn hiliting off
        END IF
     

        '----- If we have a window frame
        IF Ed.Frame THEN
                                                'If line number has changed,
           IF Ed.CurLine <> LastCurLine OR RePrint THEN
              GOSUB DisplayLineNumber           '  display line # on status
              LastCurLine = Ed.CurLine          'Save line number
           END IF
                                                'If column number has changed,
           IF Ed.CurCol <> LastCurCol OR RePrint THEN
              GOSUB DisplayColNumber            '  display col. # on status
              LastCurCol = Ed.CurCol            'Save column number
           END IF

           IF Ed.LC <> LastC THEN               'If window column has changed,
              LOCATE Ed.TSRow - 1, Ed.LSCol, 0
              MQPrint MID$(Ruler(1), Ed.LC + 1, Ed.Wide), 112
              LastC = Ed.LC                     'Save window column
           END IF
        END IF

       
        '----- Position the cursor
        LOCATE Ed.TSRow + Ed.CurLine - Ed.TL, Ed.LSCol + (Ed.CurCol - Ed.LC), 1

        IF Action = 1 OR Action = 2 OR Action = 4 THEN
           X$ = ""
           Action = 3
           EXIT DO
        END IF

        '----- Wait for a key press
        DO
            X$ = INKEY$
        LOOP UNTIL QPLen(X$)
        
    LOOP WHILE action = 0                       'Go back for another key if
                                                '  not in polling mode.

    Ed.InsStat = Insrt                           'Show the caller the Insert
                                                '  state

    EXIT SUB                                    'Bail out


'****************************************************************************
'*********************   Start of Subroutine code   *************************
'****************************************************************************



'----- Adjust the size depending on monitor type, and insert mode
CsrSize:
    IF Insrt = -1 THEN                          'If in insert mode,
       LOCATE , , , BScan \ 2, BScan            '  make large size cursor
    ELSE                                        'Overtype mode,
       LOCATE , , , BScan - 1, BScan            '  use 2 scan line cursor.
    END IF
RETURN


'----- Display the current line number on the status line
DisplayLineNumber:
    LOCATE , , 0                                'Turn the cursor off
    RSET CurLine$ = MID$(STR$(Ed.CurLine), 2)   'Put number in temp string
    QPrintRC CurLine$, Ed.TSRow + Ed.Rows, Ed.LSCol, 63

    '----- Update the vertical scroll bar pointer
    IF Ed.LCount >= Ed.Rows THEN                'Calc the Vertical position %
       VrtPtr = MinInt(Ed.CurLine * (Ed.Rows - 3&) \ Ed.LCount, Ed.Rows - 3)
    ELSE
       VrtPtr = 0
    END IF
                                                'Erase old pointer
    QPrintRC CHR$(176), Ed.TSRow + OldVrtPtr + 1, Ed.LSCol + Ed.Wide, 112
                                                'Print the new one
    QPrintRC CHR$(8), Ed.TSRow + VrtPtr + 1, Ed.LSCol + Ed.Wide, 112
    OldVrtPtr = VrtPtr                          'Save old pointer for later

RETURN



'----- Display the current column number on the status line
DisplayColNumber:
    LOCATE , , 0                                'Turn the cursor off
    RSET CurCol$ = MID$(STR$(Ed.CurCol), 2)     'Put number in temp string
    QPrintRC CurCol$, Ed.TSRow + Ed.Rows, Ed.LSCol + 5, 63

    '----- Update the horizontal scroll bar pointer
    IF Ed.Wrap THEN                             'Calc pointer position
       HorPtr = Ed.Wrap + 1
    ELSE
       HorPtr = 256
    END IF
    HorPtr = MinInt(Ed.CurCol * (Ed.Wide - 10) \ HorPtr, Ed.Wide - 11)

    QPrintRC CHR$(176), Ed.TSRow + Ed.Rows, Ed.LSCol + 9 + OldHorPtr, 112
    QPrintRC CHR$(8), Ed.TSRow + Ed.Rows, Ed.LSCol + 9 + HorPtr, 112
    OldHorPtr = HorPtr                          'Save pointer for next pass
    
RETURN




WindowLimits:
    IF Ed.CurCol >= Ed.LC + Ed.Wide THEN
       Ed.LC = Ed.CurCol - Ed.Wide + 1
       RePrint = -1
    ELSEIF Ed.CurCol < Ed.LC THEN
       Ed.LC = MaxInt(1, Ed.CurCol - Ed.Wide)
       RePrint = -1
    END IF

    IF Ed.CurLine >= Ed.TL + Ed.Rows THEN
       Ed.TL = Ed.CurLine - Ed.Rows + 1
       RePrint = -1
    ELSEIF Ed.CurLine < Ed.TL THEN
       Ed.TL = Ed.CurLine
       RePrint = -1
    END IF
RETURN



'----- Move the cursor down a line
CursorDown:
    IF Ed.CurLine >= ArrayEnd THEN Ed.CurLine = ArrayEnd
                                     'if we're at the bottom of the screen
    Temp = (Ed.CurLine - Ed.TL + 1) - Ed.Rows
    IF Temp > 0 THEN
       Ed.TL = Ed.TL + Temp          '  show the top line being one higher,
       X = Ed.TSRow + Ed.Rows - 1
       HideCursor                    'Scroll the window up
       ScrollU Ed.TSRow, Ed.LSCol, X, Ed.LSCol + Ed.Wide - 1, Temp, -1
       FOR N = 1 TO Temp
           QPrintRC MID$(Array$(Ed.CurLine - N + 1), Ed.LC, Ed.Wide), X - N + 1, Ed.LSCol, -1
       NEXT
       ShowCursor
    END IF
RETURN



'----- Move the cursor up a line
CursorUp:
    IF Ed.CurLine < 1 THEN Ed.CurLine = 1 'ignore if already on first line

    Temp = Ed.TL - Ed.CurLine
    IF Temp > 0 THEN                    'if we're at the top of the screen
       Ed.TL = Ed.TL - Temp             '  show the top line being one less,
       HideCursor                       'Scroll the window Down
       ScrollD Ed.TSRow, Ed.LSCol, Ed.TSRow + Ed.Rows - 1, Ed.LSCol + Ed.Wide - 1, Temp, -1
       FOR N = Temp TO 1 STEP -1
           QPrintRC MID$(Array$(Ed.CurLine + N - 1), Ed.LC, Ed.Wide), Ed.TSRow + N - 1, Ed.LSCol, Ed.AColor
       NEXT
       ShowCursor
    END IF
RETURN




'----- Move cursor left
CursorLeft:
    IF Ed.CurCol < 1 THEN Ed.CurCol = 1
      
    Temp = Ed.LC - Ed.CurCol
    IF Temp > 0 THEN                            'If off the left of screen
       Ed.LC = Ed.LC - Temp                     'Decrement window column
       HideCursor
       ScrollR Ed.TSRow, Ed.LSCol, Ed.TSRow + Ed.Rows - 1, Ed.LSCol + Ed.Wide - 1, Temp, -1
       LOCATE Ed.TSRow, Ed.LSCol
       APrint0 VARPTR(Array$(Ed.TL)), Ed.Rows, Ed.LC, Temp, -1
       ShowCursor
    END IF
RETURN




'----- Move cursor right
CursorRight:
    IF Ed.CurCol > 255 THEN Ed.CurCol = 255
                                                'If off right of window,
    Temp = (Ed.CurCol - Ed.LC + 1) - Ed.Wide
    IF Temp > 0 THEN
       Ed.LC = Ed.LC + Temp                     '  increment window column
       HideCursor
       ScrollL Ed.TSRow, Ed.LSCol, Ed.TSRow + Ed.Rows - 1, Ed.LSCol + Ed.Wide - 1, Temp, -1
       LOCATE Ed.TSRow, Ed.LSCol + Ed.Wide - Temp
       APrint0 VARPTR(Array$(Ed.TL)), Ed.Rows, Ed.LC + Ed.Wide - Temp, Temp, -1
       ShowCursor
    END IF
RETURN




'----- Wrap all text to current right margin
WrapAll:
    DO UNTIL WrapLine > Ed.LCount               'Stop at last used element
        IF QPLen(Array$(WrapLine)) THEN         'If it's not a blank line,
           
           GOSUB WrapUp                         '  then up.
           GOSUB WrapDown                       '  wrap paragraph down and
           
           WrapLine = Lin + 1                   'Look at line past paragraph
        ELSE                                    '  "Lin" is set in "WrapUp"
           WrapLine = WrapLine + 1              'Line was blank, look at next
        END IF
    LOOP
RETURN




'----- Wrap words down to form a paragraph
WrapDown:
    Lin = WrapLine                              'Make temp copy of line #
    LinLen = QPLen(Array$(Lin))                 'Get the length of cur. line
    P = INSTR(Array$(Lin), Par$)                'Look for a paragraph marker
                                                'Do lines that are too long
    DO WHILE LinLen > Ed.Wrap

       IF P > 0 AND P <= Ed.Wrap + 1 THEN       'If marker is in range,
          B = P                                 '  set cutoff to marker
          Array$(Lin) = LEFT$(Array$(Lin), P - 1) + MID$(Array$(Lin), P + 1)
       ELSE                                     'Starting at the right margin,
                                                'look backwards for a blank
          I = QInstrB%(Ed.Wrap + 1, Array$(Lin), " ")
          IF I = 0 THEN I = Ed.Wrap + 1         'If no blanks, chop it off

          B = Blanks%(MID$(Array$(Lin), I)) + I 'Now look fwd. for non-blank
       END IF
       

       IF B <= LinLen THEN                      'If we didn't go past the end,
          Lin = Lin + 1                         '  increment the line number
          IF Lin > ArrayEnd THEN                'Past end?  Bail out
             Lin = Lin - 1
             Ed.MErr = 2
             EXIT DO
          END IF

          LinLen = QPLen(Array$(Lin))           'Get the length of new line
                                                'If this is a blank line or
                                                '  starts with a space
          IF LinLen = 0 OR Blanks%(Array$(Lin)) OR ASCII(Array$(Lin)) = 20 THEN
             Temp = Lin                         'Insert remainder of last line
             Temp$ = MID$(Array$(Lin - 1), B)   '  into a new line
             GOSUB InsertLine
          ELSE                                  'Add remainder of prev. line
                                                '  to this line
             Array$(Lin) = MID$(Array$(Lin - 1), B) + SPACE$(FnSpaces2Pad(Array$(Lin - 1))) + Array$(Lin)
          END IF
                                                'If we just wrapped current
          IF Lin - 1 = Ed.CurLine THEN          '  line and the cursor was
             IF Ed.CurCol >= B THEN             '  within the wrapped words,
                Ed.CurCol = Ed.CurCol - B + 1   '  move the cursor to the new
                Ed.LC = ((Ed.CurCol \ Ed.Wide) * Ed.Wide) + 1'  line at the end of the
                Ed.CurLine = MinInt(Lin, ArrayEnd)           '  wrapped words
                Ed.TL = MaxInt%(Ed.TL, Ed.CurLine - Ed.Rows + 1)
             END IF
          END IF

          LinLen = QPLen(Array$(Lin))           'Get length of the new line
                                                'Trim the prev. line
          Array$(Lin - 1) = RTRIM$(LEFT$(Array$(Lin - 1), B - 1))
          RePrint = -1                          'Set flag to re-print window

       ELSE                                     'Words weren't found past wrap
          Lin = Lin + 1                         'Increment the line number
          IF Lin > ArrayEnd THEN
             Lin = Lin - 1
             Ed.MErr = 2
             EXIT DO                            'Past end?  Bail out
          END IF
          LinLen = QPLen(Array$(Lin))           'Get its length
       END IF

       P = INSTR(Array$(Lin), Par$)             'Look for a paragraph marker

    LOOP                                        'Process the new line

    IF P THEN
       Array$(Lin) = LEFT$(Array$(Lin), P - 1) + MID$(Array$(Lin), P + 1)
    END IF

RETURN



'----- Wrap words Up to re-form a paragraph
WrapUp:
    Lin = WrapLine                              'Make temp copy of line #
    '----- If the line isn't nul and current line isn't past the end and the
    '----- current line doen't have a paragraph marker on the end.
    DO UNTIL Null(Array$(Lin)) OR Lin >= Ed.LCount

        P = INSTR(Array$(Lin + 1), Par$)
        IF P THEN
           Array$(Lin + 1) = LEFT$(Array$(Lin + 1), P - 1) + MID$(Array$(Lin + 1), P + 1)
        END IF

        LinLen = QPLen(Array$(Lin + 1))         'Get the length of next line
                                                'If it has Par marker or is
                                                '  blank, bail out
        IF Blanks%(Array$(Lin + 1)) THEN EXIT DO

        Temp = FnSpaces2Pad(Array$(Lin))

        IF Lin = WrapLine AND Ed.Wrap > WrapWas THEN
           Extra = WrapWas - QPLen(Array$(Lin)) - Temp + 1
           GOSUB GetWordPos
           IF I THEN                            'Words were found to fill so,
              IF INSTR(LEFT$(Array$(Lin + 1), I - 1), " ") THEN
                 EXIT DO
              END IF
           END IF
        END IF

        Extra = Ed.Wrap - QPLen(Array$(Lin)) - Temp + 1

        GOSUB GetWordPos

        IF I THEN

           IF Lin = WrapLine AND Ed.Wrap <= WrapWas THEN
              IF INSTR(LEFT$(Array$(Lin + 1), I - 1), " ") THEN ' OR I = LinLen THEN
                 EXIT DO
              END IF
           END IF
                                                'Add words from next line
           Array$(Lin) = Array$(Lin) + SPACE$(Temp) + LEFT$(Array$(Lin + 1), I)
                                                'Now delete them from next
           Array$(Lin + 1) = LTRIM$(MID$(Array$(Lin + 1), I + 1))
           RePrint = -1                         'Set "RePrint" flag

           IF LEN(Array$(Lin + 1)) = 0 THEN     'If nothing is left of next
                                                '  line, delete it.
              Temp = Lin + 1
              GOSUB DeleteLine

              Lin = Lin - 1
              IF Lin + 1 > Ed.LCount THEN EXIT DO 'Off the end, bail out
           END IF
        END IF

        Lin = Lin + 1
    LOOP

RETURN

GetWordPos:
    IF Extra > 0 THEN                           'If there is extra space,
       IF Extra >= LinLen THEN
          I = LinLen
       ELSE
          B = Extra
          IF MID$(Array$(Lin + 1), B, 1) <> " " THEN
             B = QInstrB(B, Array$(Lin + 1), " ")
          END IF

          FOR I = B TO 1 STEP -1
              IF MID$(Array$(Lin + 1), I, 1) <> " " THEN EXIT FOR
          NEXT
       END IF
    ELSE
       I = 0
    END IF
RETURN




'----- Insert an element into the text array at "Temp"
InsertLine:
    IF Temp <= Ed.LCount THEN
       Ed.LCount = MinInt%(Ed.LCount + 1, ArrayEnd)
       InsertStr VARPTR(Array$(Temp)), Temp$, Ed.LCount - Temp
    ELSE
       Ed.LCount = Temp
       Array$(Temp) = Temp$
    END IF
RETURN




'----- Delete an element from the text array
DeleteLine:
    IF Temp <= Ed.LCount THEN
       IF Temp < Ed.LCount THEN
          DeleteStr VARPTR(Array$(Temp)), Ed.LCount - Temp
       END IF
       Array$(Ed.LCount) = ""
       Ed.LCount = Ed.LCount - 1                'Update last line number
    END IF
RETURN




'----- Resize and Draw the window frame
InitWindow:
    '----- Calculate the bounds of the window
    WC(0, 0) = Ed.TSRow + (Ed.Frame <> 0)       'Top line of window
    WC(1, 0) = Ed.LSCol + (Ed.Frame <> 0)       'Left margin of window
    WC(2, 0) = WC(0, 0) + Ed.Rows - (2 * (Ed.Frame <> 0)) - 1  'Bottom line of window
    WC(3, 0) = WC(1, 0) + Ed.Wide - (2 * (Ed.Frame <> 0)) - 1  'Right margin of window
   
    'Did the Lower Right corner move Up?
    IF WC(2, 0) < WC(2, 1) THEN                 'Restore slice
       MPRestore WC(2, 0) + 1, WC(1, 1), WC(2, 1), WC(3, 1), 80, ScrBuf((WC(2, 0) * 80) + WC(1, 1))
    END IF

    'Did the Lower Right corner move Left?
    IF WC(3, 0) < WC(3, 1) THEN                 'Restore slice
       MPRestore WC(0, 1), WC(3, 0) + 1, WC(2, 1), WC(3, 1), 80, ScrBuf((WC(0, 1) - 1) * 80 + WC(3, 0) + 1)
    END IF

    'Did the Upper Left corner move Down?
    IF WC(0, 0) > WC(0, 1) THEN                 'Restore slice
       MPRestore WC(0, 1), WC(1, 1), WC(0, 0) - 1, WC(3, 1), 80, ScrBuf((WC(0, 1) - 1) * 80 + WC(1, 1))
    END IF
    
    'Did the Upper Left corner move Right?
    IF WC(1, 0) > WC(1, 1) THEN                 'Restore slice
       MPRestore WC(0, 1), WC(1, 1), WC(2, 1), WC(1, 0) - 1, 80, ScrBuf((WC(0, 1) - 1) * 80 + WC(1, 1))
    END IF
                       
    WC(0, 1) = WC(0, 0)
    WC(1, 1) = WC(1, 0)                         'Save new bounds for next time
    WC(2, 1) = WC(2, 0)
    WC(3, 1) = WC(3, 0)
                                                
    LineBuf$ = SPACE$(Ed.Wide)                  'Temp buffer used for printing
    

    '----- If Frame flag set, draw the frame and scroll bars
    IF Ed.Frame THEN
       HideCursor                               'Shut the mouse cursor off
       '----- Print the top line
       QPrintRC CHR$(4), Ed.TSRow - 1, Ed.LSCol - 1, HiClr
       QPrintRC MID$(Ruler(1), Ed.LC + 1, Ed.Wide), Ed.TSRow - 1, Ed.LSCol, 112
       QPrintRC CHR$(18), Ed.TSRow - 1, WC(3, 0), HiClr
       '----- Print the middle lines
       X = Ed.TL
       FOR N = Ed.TSRow TO Ed.TSRow + Ed.Rows - 1

           QPrintRC "³", N, Ed.LSCol - 1, HiClr
           IF X <= ArrayEnd THEN
              LSET LineBuf$ = MID$(Array$(X), Ed.LC)
           ELSE
              LSET LineBuf$ = ""
           END IF
           X = X + 1
           QPrintRC LineBuf$, N, Ed.LSCol, Ed.AColor

           QPrintRC "°", N, WC(3, 0), 112
       NEXT
       '----- Print the up and down arrows
       QPrintRC CHR$(24), Ed.TSRow, WC(3, 0), 112
       QPrintRC CHR$(25), N - 1, WC(3, 0), 112
       '----- Print the status spaces on bottom of window
       QPrintRC "À", Ed.TSRow + Ed.Rows, Ed.LSCol - 1, HiClr
       QPrintRC ":", Ed.TSRow + Ed.Rows, Ed.LSCol + 4, 48
       '----- Print the horizontal scroll bar
       QPrintRC CHR$(27) + STRING$(Ed.Wide - 10, "°") + CHR$(26), Ed.TSRow + Ed.Rows, Ed.LSCol + 8, 112
       QPrintRC CHR$(4), Ed.TSRow + Ed.Rows, WC(3, 0), HiClr

       ShowCursor                               'Turn the mouse cursor back on
       LastCurLine = 0                          'Save cursor location so we
       LastCurCol = 0                           '  know when to reprint status
    ELSE
       RePrint = -1                             'Set flag to print the window
    END IF
                                                'Make sure the cursor stays in
                                                '  the window
    Ed.CurLine = MinInt(Ed.CurLine, Ed.TL + Ed.Rows - 1)
    Ed.CurCol = MinInt(Ed.CurCol, Ed.LC + Ed.Wide - 1)
    OldVrtPtr = 0                               'Init scroll pointers
    OldHorPtr = 0

RETURN
     



'----- Restore the screen and free up memory
CleanUp:
    MScrnRest 1, 1, ScrRows, 80, ScrBuf(1)      'Restore the screen
    ERASE ScrBuf, Ruler                         'Clean up memory
    LineBuf$ = ""
    LOCATE , , , BScan - 1, BScan
RETURN



END SUB          'Thats all folks.  And you thought editing was easy?

