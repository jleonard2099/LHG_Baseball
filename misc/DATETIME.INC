' DATETIME.INC
'
' DOS date and time stamp formatting routines
' Copyright (C) Robert J. Showalter, 1987.  CompuServe ID 72220,466.
'
'     Name             Description
'     ----             -----------
' def fnformatdate$    formats a DOS date stamp into a string
' def fnformattime$    formats a DOS time stamp into a string
'

'-----------------------------------------------------------------------
'
' fnformatdate$     formats a DOS date stamp into a string
'
' input parameters:
'
'    date%           DOS date stamp to be formatted
'
' function returns: date formatted as MM/DD/YY (8 character string)
'
def fnformatdate$(date%)
      local y%,m%,d%,i%
      local x$

      y% = (date%\512 + 1980) mod 100
      m% = date%\32 mod 16
      d% = date% mod 32
      x$ = right$(str$(m%),2)+"/"+right$(str$(d%),2)+"/"+right$(str$(y%),2)
      for i% = 2 to len(x$)              ' note: leave first leading blank
         if mid$(x$,i%,1)=" " then mid$(x$,i%,1) = "0"
      next i%
      fnformatdate$ = x$
end def


'-----------------------------------------------------------------------
'
' fnformattime$     formats a DOS time stamp into a string
'
' input parameters:
'
'    time%           DOS time stamp to be formatted
'
' function returns: time formatted as HH:MM:SS AM (11 character string)
'
' note that time stamp resolution is only 2 seconds
'
def fnformattime$(time%)
      local h%,m%,s%,i%
      local m$,x$

      if time%<0 then
         time% = time% and &h7FFF
         h% = 16
      else
         h% = 0
      end if
      h% = h%+time%\2048
      if h%>=12 then
         m$ = " PM"
         h% = h%-12
      else
         m$ = " AM"
      end if
      if h%=0 then h% = 12
      m% = (time%\32) mod 64
      s% = (time% mod 32)*2
      x$ = right$(str$(h%),2)+":"+right$(str$(m%),2)+":"+right$(str$(s%),2)+m$
      for i% = 2 to 8                    ' note: leave first leading blank
         if mid$(x$,i%,1)=" " then mid$(x$,i%,1) = "0"
      next i%
      fnformattime$ = x$
end def
