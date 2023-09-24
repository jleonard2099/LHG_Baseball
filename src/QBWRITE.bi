DefInt A-Z

DECLARE SUB SaveIt (FFName$)
DECLARE Sub QBWRITE(FFName$)
DECLARE Sub OpenAFile(FFName$)

Common Shared Npath$, NewFile, KeyPress, PageNo, NoOfPages
Common Shared LastKey
Common Shared FileLine, FLNo, keystroke$, InsertMode

Dim Shared TmpDoc$(LineNo)
Dim Shared Doc$(LineNo)
Dim Shared Text$(row)

Clear , , 12000

