' DIRECTRY.INC
'
' Turbo Basic $INCLUDE file for directory/file utility routines.
' Copyright (C) Robert J. Showalter, 1987.  CompuServe ID 72220,466.
'
'     Name              Description
'     ----              -----------
' def fnisdir%          tests string for valid subdirectory name
' sub splitfn           splits a path/file name into its separate components
' sub findfile          searches directory for matching files
' def fngetdir$         returns current directory for specified drive
' def fndrivecode%      converts a drive letter into a DOS drive code
' sub mergefn           merges partial file spec into complete path/file name
' def fnsearchpath$     searches for file using PATH specification
'
' This file requires the include file DEFS.INC
'

' internal function to build a word from two bytes.  avoids overflow
' aborts when unsinged value exceeds 32767
'
def fnmakeint%(lsb%,msb%)

      msb% = msb% and 255
      lsb% = lsb% and 255
      if msb%>127 then msb% = msb%-256
      fnmakeint% = lsb%+256*msb%
end def


'-----------------------------------------------------------------------
'
' fnisdir%          tests whether a string specifies a file or a
'                   subdirectory.
'
' input parameters:
'
'    path$          string to be tested
'
' function return:  0  = path$ specifies a file
'                   1  = path$ specifies a subdirectory
'                   <0 = a dos CHMOD error occurred.  the value returned
'                        is -(AL).  most common error is file not found
'                        or invalid drive/path specification
'
def fnisdir%(path$)
      local p%,o%,s%
      static buf$                       ' note: this must be a static variable

      buf$=path$+chr$(0)                ' create a buffer to hold path
      def seg
      p% = varptr(buf$)                 ' get offset to string descriptor
      o% = fnmakeint(peek(p%+2),peek(p%+3))    ' offset to string (in string segment)
      s% = fnmakeint(peek(0),peek(1))          ' string segment
      reg %AX,&h4300                    ' dos CHMOD get attributes function
      reg %DS,s%                        ' DS:DX = path name
      reg %DX,o%
      call interrupt %MSDOS
      if (reg(%FLAGS) and 1)<>0 then    ' carry flag signals error
         fnisdir% = -reg(%AX)
      else
         if (reg(%CX) and &h0010)=0 then fnisdir% = 0 else fnisdir% = 1
      end if
end def


'-----------------------------------------------------------------------
'
' splitfn           splits a complete file name into drive, directory,
'                   filename and extension components.  any missing
'                   components are returned as null strings.  file name
'                   is truncated to 8 characters and extension is truncated
'                   to 3 characters (plus dot).  a path of ".." or "."
'                   will be returned as a directory.  NOTE:  this is a
'                   text-only routine.  no checking is performed on the
'                   path for valid drive names, directories, etc.
'
' input parameters:
'
'    path$          path/file string to be split
'
' output parameters: (set to "" if not specified in path$)
'
'    drive$         drive code, includes ":"
'    dir$           directory path, includes trailing "\"
'    file$          file name
'    ext$           extension string, includes "."
'
sub splitfn(path$,drive$,dir$,file$,ext$)
      local temp$
      local i%                          ' start of directory
      local j%                          ' start of file name
      local k%                          ' start of extension
      local l%                          ' end of path

      temp$ = ucase$(path$)             ' get local copy of path
      while left$(temp$,1)=" ": temp$ = right$(temp$,len(temp$)-1): wend
      while right$(temp$,1)=" ": temp$ = left$(temp$,len(temp$)-1): wend
      drive$ = ""
      dir$ = ""
      file$ = ""
      ext$ = ""

      if temp$="." or temp$=".." then   ' special case
         dir$ = temp$
         exit sub
      end if

      l% = len(temp$)+1                 ' set end of path
      i% = 2                            ' find colon
      while i%<=l and mid$(temp$,i%-1,1)<>":": i% = i%+1: wend
      if mid$(temp$,i%-1,1)<>":" then i% = 1
      j% = l%                           ' find rightmost backslash
      while j%>i% and mid$(temp$,j%-1,1)<>"\": j% = j%-1: wend
      k% = l%                           ' find extension dot
      while k%>j% and mid$(temp$,k%,1)<>".": k% = k%-1: wend
      if mid$(temp$,k%,1)<>"." then k% = l%  ' no extension dot found

      drive$ = mid$(temp$,1,i%-1)       ' extract drive
      dir$ = mid$(temp$,i%,j%-i%)       ' extract directory
      file$ = left$(mid$(temp$,j%,k%-j%),8)  ' extract file name
      ext$ = left$(mid$(temp$,k%,l%-k%),4)   ' extract extension
end sub


'-----------------------------------------------------------------------
'
' findfile          interface to DOS find first/find next directory
'                   functions
'
' input parameters:
'
'    search$        string containing search string.  drive and path are
'                   allowed, as well as wild cards in the file name and
'                   extension.  search$ should be passed as a null string
'                   after the first call to return the next matching file.
'
'    sattrib        search attribute byte.  use 0 to search for normal files.
'
' output parameters:
'
'    f$             file name and type (does not include path)
'    fattrib%        file attribute byte
'    size&          long file size in bytes
'    date%           file date stamp
'    time%           file time stamp
'    e%              error code (0=no error, 2=invalid path, 18=no more files)
'                      (if e returns non-zero, above output parameters are NOT
'                      set)
'
sub findfile(search$,sattrib%,f$,fattrib%,size&,date%,time%,e%)
      static dta%()                     ' static disk transfer area
      dim dta%(0:31)                    ' 64 bytes - preserved between calls
      local dtao%,dtas%,p%,o%,s%
      static buf$                       ' note: this must be a static variable

      dtas% = varseg(dta%(0))
      dtao% = varptr(dta%(0))
      reg %AX,&h1A00                    ' DOS set DTA address function
      reg %DS,dtas%                     ' DS:DX = address of buffer
      reg %DX,dtao%
      call interrupt %MSDOS

      if search$<>"" then               ' prepare for find first call
         buf$ = search$+chr$(0)         ' create a buffer to hold search string
         def seg                        ' set default data segment
         p% = varptr(buf$)              ' get offset to string descriptor
         o% = fnmakeint(peek(p%+2),peek(p%+3)) ' offset to string (in string segment)
         s% = fnmakeint(peek(0),peek(1))       ' string segment
         reg %AX,&h4E00                 ' DOS find first function
         reg %CX,sattrib%               ' search attribute byte in CL
         reg %DS,s%                     ' DS:DX = pointer to buffer
         reg %DX,o%
      else                              ' prepare for find next call
         reg %AX,&h4F00                 ' DOS find next function
      end if
      call interrupt %MSDOS
      e% = reg(%AX)                     ' get error from AX
      if e%=0 then                      ' ok, so extract file info
         def seg = dtas%
         i% = dtao%+30                  ' offset to filename in DTA
         f$ = ""
         while peek(i%)<>0              ' build file name
            f$ = f$+chr$(peek(i%))
            i% = i%+1
         wend
         fattrib% = peek(dtao%+21)      ' file attribute byte
         time% = fnmakeint(peek(dtao%+22),peek(dtao%+23))
         date% = fnmakeint(peek(dtao%+24),peek(dtao%+25))
         size& = clng(peek(dtao%+26))+256*clng(peek(dtao%+27))+ _
                 65536*clng(peek(dtao%+28))+16777216*clng(peek(dtao%+29))
         def seg
      end if
end sub


'-----------------------------------------------------------------------
'
' fngetdir$         function to return drive and directory path string.
'
' input parameters:
'
'    drive          drive code 0=current, 1=A:, 2=B:, etc.
'
' function return:  a string containing the current drive and directory
'                   path for the specified drive, or a null string
'                   for an invalid drive specification.
'
def fngetdir$(drive%)
      local p%,o%,s%
      local d$
      static buf$                       ' note: this must be a static variable

      if drive%=0 then                  ' use default drive
         reg %AX,&h1900                 ' DOS get current drive function
         call interrupt %MSDOS
         drive% = reg(%AX) and &h00FF   ' get drive from AL
         drive% = drive%+1              ' convert to 1=A: convention
      end if

      buf$ = space$(64)                 ' create a buffer to hold directory
      def seg                           ' set default data segment
      p% = varptr(buf$)                 ' get offset to string descriptor
      o% = fnmakeint(peek(p%+2),peek(p%+3))    ' offset to string (in string segment)
      s% = fnmakeint(peek(0),peek(1))          ' string segment

      reg %AX,&h4700                    ' DOS get directory function
      reg %DX,drive%                    ' DL = drive code
      reg %DS,s%                        ' DS:SI = pointer to buffer
      reg %SI,o%
      call interrupt %MSDOS
      if (reg(%AX) and &h00FF)=0 then   ' AL = 0 for successful
         buf$ = ucase$(left$(buf$,instr(buf$,chr$(0))-1))
         if buf$<>"" then buf$=buf$+"\"
         fngetdir$ = chr$(drive%+64)+":"+"\"+buf$
      else
         fngetdir$ = ""
      end if
end def


'-----------------------------------------------------------------------
'
' fndrivecode%       converts a drive letter string into a DOS drive
'                   code
'
' input parameters:
'
'    drive$         string representing a drive letter and colon (req'd),
'                   or a null string to indicate the default drive.
'
' function returns: DOS drive code (1=A:, 2=B:, etc) or -1 for an
'                   invalid drive specification
'
def fndrivecode%(drive$)
      local d%,dd%,nd%

      reg %AX,&h1900                    ' get default drive
      call interrupt %MSDOS
      dd% = reg(%AX) and &h00FF         ' drive code from AL (0=A:)
      reg %AX,&h0E00                    ' re-select default drive
      reg %DX,dd%
      call interrupt %MSDOS             ' using only to get # of drives
      nd% = reg(%AX) and &h00FF         ' # of drives from AL

      drive$ = ucase$(drive$)
      while left$(drive$,1)=" ": drive$ = right$(drive$,len(drive$)-1): wend
      while right$(drive$,1)=" ": drive$ = left$(drive$,len(drive$)-1): wend
      if drive$="" then
         fndrivecode% = dd%+1           ' return default drive
      elseif len(drive$)<>2 or right$(drive$,1)<>":" then
         fndrivecode% = -1              ' invalid format
      else
         d% = asc(left$(drive$,1))-64   ' drive code from string
         if d%<1 or d%>nd% then         ' bad drive code
            fndrivecode% = -1
         else
            fndrivecode% = d%
         end if
      end if
end def


'-----------------------------------------------------------------------
'
' mergefn           merges a file specification with the current
'                   drive and directory to make a fully qualified
'                   file specification.  Note: this procedure does
'                   error checking only on the drive code and the
'                   proper use of \..\ within the directory.
'
' input parameters:
'
'    path$          drive/path/file specification to be merged
'
'    dfile$         default file specification to use if none found
'
'    dext$          default extension to use if none found
'
' output parameters:
'
'    newpath$       resultant merged drive/path/file specification
'
'    e%              error code 0 = no error
'                              1 = invalid drive specification
'                              2 = invalid ".." specification in directory
'
sub mergefn(path$,dfile$,dext$,newpath$,e%)
      local cur.path$,cur.drive$,cur.dir$,z$
      local inp.path$,drive$,dir$,file$,ext$
      local d%,i%,j%
'
' compress input path, removing embedded spaces, then
' split path, handling case where it ends with a subdirectory
'
      inp.path$ = path$
      i% = instr(inp.path$," ")
      while i%<>0
         inp.path$ = left$(inp.path$,i%-1)+right$(inp.path$,len(inp.path$)-i%)
         i% = instr(inp.path$," ")
      wend
      if right$(inp.path$,1)<>"\" and fnisdir%(inp.path$)=1 then
         inp.path$ = inp.path$+"\"
      end if
      call splitfn(inp.path$,drive$,dir$,file$,ext$)
'
' get and split current drive and directory (for specified drive)
'
      d% = fndrivecode%(drive$)
      if d%<0 then
         e% = 1
         exit sub
      end if
      cur.path$ = fngetdir$(d%)
      call splitfn(cur.path$,cur.drive$,cur.dir$,z$,z$)
'
' merge components, using current/defaults where not given
'
      if drive$="" then drive$ = cur.drive$
      if left$(dir$,1)<>"\" then dir$ = cur.dir$+dir$
      if file$="" then file$ = dfile$
      if ext$="" then ext$ = dext$
'
' compress directory, removing . and .. references
'
      i% = instr(dir$,"\.\")
      while i%<>0
         dir$ = left$(dir$,i%)+right$(dir$,len(dir$)-(i%+2))
         i% = instr(dir$,"\.\")
      wend
      i% = instr(dir$,"\..\")
      while i%<>0
         j = i%-1
         while j%>0 and mid$(dir$,j%,1)<>"\": j% = j%-1: wend
         if j%=0 then
            e% = 2
            exit sub
         end if
         dir$ = left$(dir$,j%-1)+right$(dir$,len(dir$)-(i%+2))
         i% = instr(dir$,"\..\")
      wend
'
' reassemble merged path
'
      newpath$ = ucase$(drive$+dir$+file$+ext$)
      e% = 0
end sub


'-----------------------------------------------------------------------
'
' fnsearchpath$     given an input file name, searches for the file
'                   in the current directory, and if not found, searches
'                   the directories specified in the dos PATH environment
'                   variable.  returns a fully qualified file name if
'                   the file was found, or a null string if the file
'                   was not found in any of the directories.  Wild cards
'                   are allowed, and the first matching file found will
'                   be returned.
'
' input parameters:
'
'    f$             file name with optional extension
'    attrib%         attribute byte for search (0 = normal files)
'
def fnsearchpath$(f$,attrib%)
     local path$,dir$,search$,found$
     local k%,fattrib%,size&,date%,time%,e%

     path$ = environ$("PATH")           ' fetch dos search path
     k% = 1                             ' pointer into path$
     dir$ = fngetdir$(0)                ' start with current directory
     search$ = dir$+f$                  ' build search string
     call findfile(search$,attrib%,found$,fattrib%,size&,date%,time%,e%)
     while e%<>0 and k%<=len(path$)     ' continuing search through path
        dir$ = ""                       ' build directory
        while k%<=len(path$) and mid$(path$,k%,1)<>";"
           dir$ = dir$+mid$(path$,k%,1)
           incr k%
        wend
        incr k%                         ' skip over semicolon
        while left$(dir$,1)=" ": dir$ = right$(dir$,len(dir$)-1): wend
        while right$(dir$,1)=" ": dir$ = left$(dir$,len(dir$)-1): wend
        if right$(dir$,1)<>"\" then dir$=dir$+"\"
        search$ = dir$+f$
        call findfile(search$,attrib%,found$,fattrib%,size&,date%,time%,e%)
     wend
     if e%=0 then                       ' found in dir$
        fnsearchpath$ = dir$+found$
     else
        fnsearchpath$ = ""
     end if
end def
