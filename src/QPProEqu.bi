DefInt A-Z

DECLARE FUNCTION FCount% (spec$)
DECLARE FUNCTION FileSize& (FileName$)
DECLARE FUNCTION OneColor% (FGColor, BGColor)
DECLARE FUNCTION READDIR$(spec$)

DECLARE SUB box0 (ULRow%, ULCol%, LRRow%, LRCol%, Char%, FGColor%, BGColor%)
DECLARE SUB CapNum ()
DECLARE SUB ClearEOL (FGColor%, BGColor%)
DECLARE SUB Comline (NumArgs%, Args$(), MaxArgs)
DECLARE SUB CSMsgBox (Message$, Wdth%, Cnf AS ANY)
DECLARE SUB MAMenu (menuItems$(), Selected%, Start%, count%, ScanCode%, ItemFGColor%, ItemBGColor%, HighlightFGColor%, HighlightBGColor%, NumRows%, NumCols%, Spacing%, Row%, Column%)
DECLARE SUB MMenuVert (menuItems$(), Selected%, Start%, ScanCode%, ItemFGColor%, ItemBGColor%, HighlightFGColor%, HighlightBGColor%, NumRows%, Row%, Column%)
DECLARE SUB MPaintBox (ULRow%, ULCol%, LRRow%, LRCol%, Colr%)
DECLARE SUB MQPrint (X$, Colr%)
DECLARE SUB QuickSort (start AS INTEGER, finish AS INTEGER, array() AS SINGLE)
DECLARE SUB QPrint (X$, FGColor%, BGColor%, Page)
DECLARE SUB ReadFile$ (fileList$())
DECLARE SUB TextIn (T$, Max%, NumOnly%, CapsOn, ExitCode%, FGColor%, BGColor%)
DECLARE SUB YesNoB (YN$, ExitCode%, FGColor%, BGColor%)

