/*REXX*/
/* JAM - Just Another Macro language for z/OS

Copyright (c) 2008-2020, Andrew J. Armstrong
All rights reserved.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.

Author:
Andrew J. Armstrong <androidarmstrong@gmail.com>
*/
/*REXX*****************************************************************
**                                                                   **
** NAME     - JAM                                                    **
**                                                                   **
** TITLE    - JUST ANOTHER MACRO LANGUAGE FOR Z/OS                   **
**                                                                   **
** FUNCTION - See the README.md file at:                             **
**            https://github.com/abend0c1/jam                        **
**                                                                   **
** AUTHOR   - AA  - Andrew J. Armstrong <androidarmstrong@gmail.com> **
**                                                                   **
** HISTORY  - Date     By  Reason (most recent at the top please)    **
**            -------- --- ----------------------------------------- **
**            20201218 AA  Complete rewrite to make it work on Linux **
**                         and Windows too.                          **
**            20080401 AA  Original version called REXX Server Pages **
**                         (RSP).                                    **
**                                                                   **
**********************************************************************/
trace off
  parse arg sCmdLine
  signal on syntax name onSyntax
  numeric digits 22
  g. = ''
  parse source t1 t2 t3 t4 t4 t6 t7 t8 t9
  g.0ZOS = wordpos(t1,'TSO MVS') > 0
  g.0NIX = t1 = 'UNIX' | t8 = 'OMVS'
  g.0WIN = pos('WIN',t1) > 0
  g.0EBCDIC_ENVIRONMENT = g.0ZOS | t8 = 'OMVS' /* Platform is EBCDIC */

  select
    when g.0ZOS then do
      parse arg sCmdLine ' ('sOptions
      sCmdLine = strip(sCmdLine)
      parse var sCmdLine sFileIn sFileOut .
      sFileIn = strip(sFileIn,'BOTH',"'")
      parse var sFileIn sDsnIn'('sMemIn')'
      sFileOut = strip(sFileOut,'BOTH',"'")
      parse var sFileOut sDsnOut'('sMemOut')'
    end
    when g.0NIX then do
      call parseCmdLine sCmdLine,'/'
      g.0IN = openFile(sFileIn)
    end
    when g.0WIN then do
      call parseCmdLine sCmdLine,'\'
      g.0IN = openFile(sFileIn)
    end
    otherwise do
      say 'JAM0016E Unsupported environment:' t1
      exit 4
    end
  end

  call getOptions sOptions
  call prolog

  /*
   *-------------------------------------------------------------------
   *
   *-------------------------------------------------------------------
  */

  sLine = getLogicalLine()
  do while g.0RC = 0 & \g.0QUIT
    select
      /*
       *---------------------------------------------------------------
       * Define a MACRO
       *---------------------------------------------------------------
      */
      when g.0MACDEF then do /* We are defining a MACRO */
        parse upper var sLine '..' sVerb sAction .
        if left(sLine,2) = '..' & sVerb = 'MACRO' & sAction = 'END'
        then do
          g.0MACDEF = 0   /* Not defining a macro now */
          g.0MACRO  = ''  /* No macro name required   */
        end
        else do
          sMacroName = g.0MACRO
          nMacroLine = g.0MAC.sMacroName.0 + 1 /* No. of lines so far */
          if sVerb = 'INCLUDE'
          then do
            /* We need to include the lines into the macro definition
               because it doesn't work very well to include the lines
               when the macro is executed */
            call includeInMacro sAction
          end
          else do
            g.0MAC.sMacroName.nMacroLine = strip(sLine,'TRAILING')
            g.0MAC.sMacroName.0 = nMacroLine
          end
        end
      end
      /*
       *---------------------------------------------------------------
       * Process a JAM statement
       *---------------------------------------------------------------
      */
      when left(sLine,2) = '..' then do /* Possible JAM statement */
        call processStatement
        do while g.0RC = 0 & sPendingStatement <> ''
          sLine = sPendingStatement
          call processStatement
        end
        call putQueued
      end
      /*
       *---------------------------------------------------------------
       * Perform REXX expression substitution
       *---------------------------------------------------------------
      */
      when g.0EMIT then do   /* We are emitting */
        select
          when sLine = '' then do
            if blanks = 1 then queue getSub71(sLine)
          end
          when left(sLine,3) = '//*' then do
            if comments = 1 then queue getSub71(sLine)
          end
          otherwise queue getSub71(sLine)
        end
        call putQueued
      end
      /*
       *---------------------------------------------------------------
       * Ignore this input line
       *---------------------------------------------------------------
      */
      otherwise nop /* Not emitting, so do nothing */
    end
    sLine = getLogicalLine()
  end
  call Epilog
return

parseCmdLine:
  parse arg sFileIn sFileOut '--' sOptions,sPathSep
  parse value split(sFileIn, sPathSep, '.'),
        with sPathIn 'ff'x sFileNameIn 'ff'x sExtIn
  if sFileOut = '-'
  then do
    sPathout = ''
    sFileNameOut = '-'
    sExtOut = ''
  end
  else do
    parse value split(sFileOut, sPathSep, '.'),
          with sPathOut 'ff'x sFileNameOut 'ff'x sExtOut .
    if sPathOut = '' then sPathOut = sPathIn
    if sFileNameOut = '' then sFileNameOut = sFileNameIn
    if sExtOut = '' then sExtOut = '.txt'
    sFileOut = sPathOut || sFileNameOut || sExtOut
  end
return

onSyntax:
  sSourceLine = strip(sourceline(sigl))
  say 'JAM0099I' errortext(rc) 'at line' sigl':' sSourceLine
return ''

getOptions: procedure expose g.
  parse upper arg sOptions
  do i = 1 to words(sOptions)
    sOption = word(sOptions,i)
    g.0OPTION.sOption = 1
  end
return

emitFile: procedure expose g.
  parse arg sFile,sContent
  select
    when sFile = '-' then do /* terminal */
      do queued()
        parse pull sLine
        say sLine
      end
    end
    when g.0ZOS & isDDName(sFile) then do /* DD:ddname */
      parse var sFile 'DD:'sDD .
      address TSO 'EXECIO * DISKW' sDD '(FINIS'
    end
    otherwise do
      hFile = openFile(sFile,'OUTPUT')
      if g.0RC = 0
      then do
        say 'JAM0017I Writing' sContent 'to file:' sFile
        do queued()
          parse pull sLine
          call putLine hFile,sLine
        end
        call closeFile hFile
      end
      else say 'JAM0015E Could not open' sContent 'file for output:' sFile
    end
  end
return

quietly: procedure expose g. o.
  parse arg sCommand
  rc = outtrap('o.')
  address TSO sCommand
  g.0RC = rc
  rc = outtrap('off')
return



/*REXX 2.0.0
Copyright (c) 2009-2020, Andrew J. Armstrong
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in
      the documentation and/or other materials provided with the
      distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

*/

/*REXX*****************************************************************
**                                                                   **
** NAME     - IO                                                     **
**                                                                   **
** FUNCTION - Simple I/O routines.                                   **
**                                                                   **
** API      - The routines in this module are:                       **
**                                                                   **
**            openFile(filename,options,attrs)                       **
**                Opens the specified file with the specified options**
**                and returns a file handle to be used in other I/O  **
**                operations. By default the file will be opened for **
**                input. Specify 'OUTPUT' to open it for output.     **
**                For TSO, you can specify any operand of the TSO    **
**                ALLOCATE command in the third operand. For example:**
**                rc = openFile('MY.FILE','OUTPUT','RECFM(F,B)'      **
**                              'LRECL(80) BLKSIZE(27920)')          **
**                                                                   **
**            closeFile(handle)                                      **
**                Closes the file specified by 'handle' (which was   **
**                returned by the openFile() routine.                **
**                                                                   **
**            getLine(handle)                                        **
**                Reads the next line from the file specified by     **
**                'handle'.                                          **
**                                                                   **
**            putLine(handle,data)                                   **
**                Appends the specified data to the file specified   **
**                by 'handle'.                                       **
**                                                                   **
**                                                                   **
** AUTHOR   - Andrew J. Armstrong <androidarmstrong+sf@gmail.com>    **
**                                                                   **
** HISTORY  - Date     By  Reason (most recent at the top please)    **
**            -------- --------------------------------------------- **
**            20090822 AJA Changed from GPL to BSD license.          **
**            20061017 AJA Added support for UNIX environment.       **
**                         Tested on Ubuntu Linux 6.06 LTS.          **
**            20050930 AJA Initial version.                          **
**                                                                   **
**********************************************************************/

  parse source . . sSourceFile .
  parse value sourceline(1) with . sVersion
  say 'Simple REXX I/O routines' sVersion
  say 'You cannot invoke this rexx by itself!'
  say
  say 'This rexx is a collection of subroutines to be called'
  say 'from your own rexx procedures. You should either:'
  say '  - Append this procedure to your own rexx procedure,'
  say '    or,'
  say '  - Append the following line to your rexx:'
  say '    /* INCLUDE' sSourceFile '*/'
  say '    ...and run the rexx preprocessor:'
  say '    rexxpp myrexx myrexxpp'
  say '    This will create myrexxpp by appending this file to myrexx'
exit

/*-------------------------------------------------------------------*
 * Open a file
 *-------------------------------------------------------------------*/

openFile: procedure expose g.
  parse arg sFile,sOptions,sAttrs
  hFile = ''
  select
    when g.0ENV = 'TSO' then do
      bOutput = wordpos('OUTPUT',sOptions) > 0
      bQuoted = left(sFile,1) = "'"
      if bQuoted then sFile = strip(sFile,,"'")
      parse var sFile sDataset'('sMember')'
      if sMember <> '' then sFile = sDataset
      if bQuoted then sFile = "'"sFile"'"
      if bOutput
      then 'LMINIT  DATAID(hFile) DATASET(&sFile) ENQ(EXCLU)'
      else 'LMINIT  DATAID(hFile) DATASET(&sFile)'
      if sMember <> ''
      then do /* Open a member of a PDS */
        'LMOPEN  DATAID(&hFile) OPTION(INPUT)' /* Input initially */
        /* ...can't update ISPF stats when opened for output */
        g.0MEMBER.hFile = sMember
        'LMMFIND DATAID(&hFile) MEMBER('sMember') STATS(YES)'
        if bOutput
        then do
          if rc = 0
          then g.0STATS.hFile = zlvers','zlmod','zlc4date
          else g.0STATS.hFile = '1,0,0000/00/00'
          'LMCLOSE DATAID(&hFile)'
          'LMOPEN  DATAID(&hFile) OPTION(&sOptions)'
        end
      end
      else do /* Open a sequential dataset */
        'LMOPEN  DATAID(&hFile) OPTION(&sOptions)'
        if rc <> 0 /* If dataset does not already exist... */
        then do /* Create sequential dataset then open it */
          'LMCLOSE DATAID(&hFile)'
          'LMFREE  DATAID(&hFile)'
          address TSO 'ALLOCATE DATASET('sFile') NEW CATALOG',
                      'SPACE(5,15) TRACKS RECFM(V,B)',
                      'LRECL('g.0OPTION.WRAP.1 + 4')',
                      'BLKSIZE(27990)' sAttrs
          if bOutput
          then do
            'LMINIT  DATAID(hFile) DATASET(&sFile) ENQ(EXCLU)'
            'LMOPEN  DATAID(&hFile) OPTION(&sOptions)'
          end
          else do
            'LMINIT  DATAID(hFile) DATASET(&sFile)'
            'LMOPEN  DATAID(&hFile) OPTION(INPUT)'
          end
        end
      end
      g.0OPTIONS.hFile = sOptions
      g.0rc = rc /* Return code from LMOPEN */
    end
    otherwise do
      if wordpos('OUTPUT',sOptions) > 0
      then junk = stream(sFile,'COMMAND','OPEN WRITE REPLACE')
      else junk = stream(sFile,'COMMAND','OPEN READ')
      hFile = sFile
      if stream(sFile,'STATUS') = 'READY'
      then g.0rc = 0
      else g.0rc = 4
    end
  end
return hFile

/*-------------------------------------------------------------------*
 * Read a line from the specified file
 *-------------------------------------------------------------------*/

getLine: procedure expose g.
  parse arg hFile
  sLine = ''
  select
    when g.0ENV = 'TSO' then do
      'LMGET DATAID(&hFile) MODE(INVAR)',
            'DATALOC(sLine) DATALEN(nLine) MAXLEN(32768)'
      g.0rc = rc
      sLine = strip(sLine,'TRAILING')
      if sLine = '' then sLine = ' '
    end
    otherwise do
      g.0rc = 0
      if chars(hFile) > 0
      then sLine = linein(hFile)
      else g.0rc = 4
    end
  end
return sLine

/*-------------------------------------------------------------------*
 * Append a line to the specified file
 *-------------------------------------------------------------------*/

putLine: procedure expose g.
  parse arg hFile,sLine
  select
    when g.0ENV = 'TSO' then do
      g.0LINES = g.0LINES + 1
      'LMPUT DATAID(&hFile) MODE(INVAR)',
            'DATALOC(sLine) DATALEN('length(sLine)')'
    end
    otherwise do
      junk = lineout(hFile,sLine)
      rc = 0
    end
  end
return rc

/*-------------------------------------------------------------------*
 * Close the specified file
 *-------------------------------------------------------------------*/

closeFile: procedure expose g.
  parse arg hFile
  rc = 0
  select
    when g.0ENV = 'TSO' then do
      if g.0MEMBER.hFile <> '', /* if its a PDS */
      & wordpos('OUTPUT',g.0OPTIONS.hFile) > 0 /* opened for output */
      then do
        parse value date('STANDARD') with yyyy +4 mm +2 dd +2
        parse var g.0STATS.hFile zlvers','zlmod','zlc4date
        zlcnorc  = min(g.0LINES,65535)   /* Number of lines   */
        nVer = right(zlvers,2,'0')right(zlmod,2,'0')  /* vvmm */
        nVer = right(nVer+1,4,'0')       /* vvmm + 1          */
        parse var nVer zlvers +2 zlmod +2
        if zlc4date = '0000/00/00'
        then zlc4date = yyyy'/'mm'/'dd   /* Creation date     */
        zlm4date = yyyy'/'mm'/'dd        /* Modification date */
        zlmtime  = time()                /* Modification time */
        zluser   = userid()              /* Modification user */
        'LMMREP DATAID(&hFile) MEMBER('g.0MEMBER.hFile') STATS(YES)'
      end
      'LMCLOSE DATAID(&hFile)'
      'LMFREE  DATAID(&hFile)'
    end
    otherwise do
      if stream(hFile,'COMMAND','CLOSE') = 'UNKNOWN'
      then rc = 0
      else rc = 4
    end
  end
return rc



includeInMacro: procedure expose g. dataset
  i.0 = 0
  if g.0ZOS
  then do
    parse upper arg sDataset
    sDSN = getFileName(sDataset)
    sFileStatus = sysdsn("'"sDSN"'")
    if sFileStatus = 'OK'
    then do
      call quietly "ALLOCATE FILE(INC) DSNAME('"sDSN"') INPUT SHR REUSE"
      'EXECIO * DISKR INC (FINIS STEM i.'
      call quietly 'FREE FILE(INC)'
    end
    else do
      say 'JAM002W Could not read dataset:' sDSN '-' sFileStatus
    end
  end
  else do
    parse arg sFileName
    call readIntoStem sFileName
  end
  if i.0 > 0 /* If there are any lines to include */
  then do
    sMacroName = g.0MACRO
    do i = 1 to i.0
      i.i = normaliseSquareBrackets(i.i)
      parse upper var i.i 1 sPrefix +2 1 . sVerb sDataset .
      if sPrefix = '..' & sVerb = 'INCLUDE'
      then do
        call includeInMacro sDataset
      end
      else do
        nMacroLine = g.0MAC.sMacroName.0 + 1
        g.0MAC.sMacroName.nMacroLine = strip(i.i,'TRAILING')
        g.0MAC.sMacroName.0 = nMacroLine
      end
    end
  end
return

readIntoStem: procedure expose g. i.
  parse arg sFileName
  i.0 = 0
  hFile = openFile(sFileName)
  if g.0RC = 0
  then do
    sLine = getLine(hFile)
    do i = 1 while g.0RC = 0
      i.0 = i
      i.i = sLine
      sLine = getLine(hFile)
    end
    call closeFile(hFile)
  end
  else do
    say 'JAM002W Could not read file:' sFileName
  end
return

putQueued:
  if g.0ZOS
  then 'EXECIO' queued() 'DISKW OUT'
return

getLogicalLine: procedure expose g. trunc
  /* Get the next logical input line (collapse continuations) */
  sLine = getNextLine()
  if \g.0MACDEF /* If we are not currently defining a macro */
  then do  /* Process continuations */
    g.0CHAINED = 0 /* Set if 2+ commands are to be run as one step */
    g.0CONT = 0    /* Set if a command extends to the next line */
    if left(sLine,2) = '..'
    then do
      if trunc = 1 /* If user wants input truncated to 71 characters */
      then sLine = strip(left(sLine,71),'TRAILING')
      else sLine = strip(     sLine    ,'TRAILING')
      parse var sLine '..' c .
      bNotComment = left(c,1) <> '.'
      sLastChar = right(sLine,1)
      g.0CHAINED = sLastChar = ',' & bNotComment /* Don't chain comments    */
      g.0CONT   = sLastChar = '-' & bNotComment  /* Don't continue comments */
      if g.0CHAINED | g.0CONT
      then sLine = left(sLine,length(sLine)-1) /* Remove continuation */
      do while g.0CONT & g.0RC = 0 /* Accumulate line continuations */
        sNextLine = getLogicalLine() /* TODO: must this be recursive? */
        if g.0RC = 0
        then do
          parse var sNextLine 3 sNextLine
          sLine = sLine || strip(sNextLine)
        end
      end
    end
  end
return sLine

getNextLine: procedure expose g.
  /* Get the next input line (high to low priority) as follows:
     - the next override line for the current macro line (if any)
     - the next line from the current macro (if any)
     - the next line from the last INCLUDE operation (if any)
     - the next line from the file currently being EDITed or VIEWed (if z/OS)
     - the next line from the DD named IN (if z/OS)
     - the next line from the file named in g.0IN (if not z/OS)
  */
  g.0RC = 0
  select
    when g.0MACRUN then do   /* if running a macro */
                             /* retrieve next line of the macro */
      sName = g.0MACRO       /* current macro name */
      nLine = g.0MACLINE     /* current line within that macro */
      nOvers = g.0MAC.sName.nLine.0 /* number of overrides for this line */
      /* don't advance the macro line until all overrides are processed */
      if nOvers <> '' & g.0MAC.sName.nLine.0# < nOvers
      then do /* return next override line for this macro line */
        nOver = g.0MAC.sName.nLine.0# + 1
        g.0MAC.sName.nLine.0# = nOver
        sLine = g.0MAC.sName.nLine.nOver
      end
      else do /* return the next line of the current macro */
        g.0MACLINE = g.0MACLINE + 1
        do while g.0MACRUN & g.0MACLINE > g.0MAC.sName.0 /* at macro end */
          g.0MACRUN.sName = 0 /* Not running this macro anymore */
          call removeOverrides sName
          parse value popStack() with sClause g.0EMIT g.0MACLINE g.0MACRO
          sName = g.0MACRO
          if sName = ''      /* if there is no invoking macro */
          then g.0MACRUN = 0 /* not running a macro now */
          else do
            nLine = g.0MACLINE
            nOvers = g.0MAC.sName.nLine.0
            if nOvers <> '' & g.0MAC.sName.nLine.0# < nOvers
            then do
            end
            else do
              g.0MACLINE = g.0MACLINE + 1 /* next line of parent macro */
            end
          end
        end
        if g.0MACRUN /* we are still running a macro */
        then do
          nLine = g.0MACLINE /* progress within this macro */
          nOvers = g.0MAC.sName.nLine.0
          if nOvers <> '' & g.0MAC.sName.nLine.0# < nOvers
          then do /* return next override line for this macro line */
            nOver = g.0MAC.sName.nLine.0# + 1
            g.0MAC.sName.nLine.0# = nOver
            sLine = g.0MAC.sName.nLine.nOver
          end
          else do
            sLine = g.0MAC.sName.nLine
          end
        end
        else sLine = getNextInputLine()
      end
    end
    otherwise sLine = getNextInputLine()
  end
  g.0ERRLINE = sLine
return sLine


getNextInputLine: procedure expose g.
  select
    when g.0INCLO <= g.0INCHI then do
      n = g.0INCLO
      sLine = g.0INC.n  /* retrieve next INCLUDEd line */
      drop g.0INC.n
      g.0INCLO = n + 1
    end
    when g.0EDITENV then do /* only on z/OS */
                 /* retrieve next line from file being edited */
      g.0LINE = g.0LINE + 1
      address ISREDIT '(sLine) = LINE' g.0LINE
      g.0RC = rc
    end
    when g.0ZOS then do /* retrieve next line from IN DD */
      'EXECIO 1 DISKR IN (STEM d.'
      g.0RC = rc
      sLine = d.1
    end
    otherwise do /* retrieve next line from disk file */
      sLine = getLine(g.0IN)
    end
  end
  if g.0ZOS
  then sLine = normaliseSquareBrackets(sLine)
return sLine

normaliseSquareBrackets: procedure expose g.
  /* Convert IBM037 or IBM1047 square brackets to whatever
    encoding is used by THIS rexx implementation */
  parse arg sLine
return translate(sLine,'[][]','babbadbd'x)

getSub71:
  if trunc = 0 then return getSub(sLine)
  parse arg sLine +71 sRest
return left(getSub(sLine),71) || sRest

getSub:
  parse arg sSubLine
  sSubLine = hardBrackets(sSubLine)
  sOut = ''
  nBeg = pos('[',sSubLine)
  nEnd = pos(']',sSubLine)
  if nBeg > 0 & nEnd = 0
  then say 'JAM006W Missing terminating bracket:' g.0ERRLINE
  do while nBeg > 0 & nBeg < nEnd
    parse var sSubLine sBefore'['sExpression']'sSubLine
    if right(strip(sExpression),1) = '?'
    then do
      sExpression = strip(strip(sExpression),'TRAILING','?')
      parse var sExpression sVarName sPrompt
      if sPrompt <> ''
      then say sPrompt':'
      else say 'Enter' sVarName':'
      parse pull sValue
      interpret sVarName "= '"toStr(sValue)"'"
    end
    else interpret 'sValue =' sExpression
    cLeft  = left(sExpression,1)
    cRight = right(sExpression,1)
    nWidth = length(sExpression)+2 /* +2 for the surrounding [ and ] */
    select
      when cLeft = ' ' & cRight = ' ' then do /* centre output */
        sValue = centre(sValue,nWidth)
      end
      when cLeft = ' '                then do /* right justify  */
        sValue = right(sValue,nWidth)
      end
      when               cRight = ' ' then do /* left justify   */
        sValue = left(sValue,nWidth)
      end
      otherwise nop
    end
    sOut = sOut || sBefore || sValue
    nBeg = pos('[',sSubLine)
    nEnd = pos(']',sSubLine)
    if nBeg > 0 & nEnd = 0
    then say 'JAM006W Missing terminating bracket:' g.0ERRLINE
  end
  if length(sSubLine) > 0 then sOut = sOut || sSubLine
return softBrackets(sOut)

hardBrackets: procedure
  /* Convert double brackets to "hard" brackets: 'FB'x and 'FE'x     */
  parse arg sLine
  if pos('[[',sLine) > 0
  then sLine = replace('[[','FB'x,sLine) /* Insert hard Begin brackets */
  if pos(']]',sLine) > 0
  then sLine = replace(']]','FE'x,sLine) /* Insert hard End brackets */
return sLine

softBrackets: procedure expose ibm1047 g.
  /* Convert hard brackets to either IBM1047 or IBM037 square brackets */
  parse arg sLine
  if g.0ZOS
  then do
    if ibm1047 = 1
    then sLine = translate(sLine,'ADBD'x,'FBFE'x) /* Code page IBM1047 */
    else sLine = translate(sLine,'BABB'x,'FBFE'x) /* Code page IBM037  */
  end
  else sLine = translate(sLine,'[]','FBFE'x) /* Native encoding */
return sLine

processStatement:
  parse var sLine '..'sStatement
  sVerb = word(sStatement,1)
  /* Slurp up any continuations that are the same as this verb */
  sPendingStatement = ''
  bResidue = 0
  g.0 = 1
  g.1 = getSub(sStatement)
  if debug = 1 then call sayDebug '..'g.1
  do i = 2 while g.0CHAINED
    sLine = getLogicalLine()
    if left(sLine,2) = '..'
    then do
      parse var sLine '..'sStatement
      sNextVerb = word(sStatement,1)
      if sNextVerb = sVerb /* e.g. must continue a COPY to another COPY */
      then do
        g.0 = i
        g.i = getSub(sStatement)
        if debug = 1 then call sayDebug '..'sStatement
      end
      else do
        g.0CHAINED = 0
        sPendingStatement = '..'sStatement
      end
    end
    else do
      bResidue = 1     /* A JAM statement was chained to a */
      sResidue = sLine /* non-JAM statement line */
      g.0CHAINED = 0
    end
  end
  /* Now action the verb */
  upper sVerb
  if g.0EMIT | inSet(sVerb,'IF ELSE END SELECT WHEN OTHERWISE')
  then do
    select
      when sVerb = '.'         then call doJAMComment
      when sVerb = '*'         then call doComment
      when sVerb = 'ARGS'      then call doArgs
      when sVerb = 'ASK'       then call doAsk
      when sVerb = 'ASKQ'      then call doAsk
      when sVerb = 'ASKQU'     then call doAsk
      when sVerb = 'ASKU'      then call doAsk
      when sVerb = 'AUTO'      then call doAuto
      when sVerb = 'BACKUP'    then call doBackup
      when sVerb = 'BR14'      then call doBR14
      when sVerb = 'CATALOG'   then call doCatalog
      when sVerb = 'COMPRESS'  then call doCompress
      when sVerb = 'COPY'      then call doCopy
      when sVerb = 'DATEVARS'  then call doDateVars
      when sVerb = 'DELETE'    then call doDelete
      when sVerb = 'ELSE'      then call doElse
      when sVerb = 'EMPTY'     then call doEmpty
      when sVerb = 'END'       then call doEnd
      when sVerb = 'FOR'       then call doFor
      when sVerb = 'GET'       then call doGet
      when sVerb = 'GETOUT'    then call doGetOut
      when sVerb = '?'         then call doHelp
      when sVerb = 'HELP'      then call doHelp
      when sVerb = 'IF'        then call doIf
      when sVerb = 'INCLUDE'   then call doInclude
      when sVerb = 'JCL'       then call doJCL
      when sVerb = 'JOB'       then call doJob
      when sVerb = 'LISTCAT'   then call doListcat
      when sVerb = 'LISTVTOC'  then call doListVTOC
      when sVerb = 'MACRO'     then call doMacro
      when sVerb = 'MAP'       then call doMap
      when sVerb = 'MOUNT'     then call doMount
      when sVerb = 'OPTION'    then call doOption
      when sVerb = 'OTHERWISE' then call doOtherwise
      when sVerb = 'PUT'       then call doPut
      when sVerb = 'QUEUE'     then call doQueue
      when sVerb = 'QUEUED'    then call doQueued
      when sVerb = 'QUIT'      then call doQuit
      when sVerb = 'RECOVER'   then call doRecover
      when sVerb = 'RENAME'    then call doRename
      when sVerb = 'REPRO'     then call doRepro
      when sVerb = 'RESTORE'   then call doRestore
      when sVerb = 'REXX'      then call doREXX
      when sVerb = 'RUNON'     then call doRunOn
      when sVerb = 'SAY'       then call doSay
      when sVerb = 'SCRATCH'   then call doScratch
      when sVerb = 'SELECT'    then call doSelect
      when sVerb = 'SET'       then call doSet
      when sVerb = 'SHIP'      then call doShip
      when sVerb = 'STEP'      then call doStep
      when sVerb = 'STYLE'     then call doStyle
      when sVerb = 'SUBMIT'    then call doSubmit
      when sVerb = 'SUDO'      then call doSudo
      when sVerb = 'TSO'       then call doTSO
      when sVerb = 'TABLE'     then call doTable
      when sVerb = 'UNCATALOG' then call doUncatalog
      when sVerb = 'UNMOUNT'   then call doUnmount
      when sVerb = 'USS'       then call doUSS
      when sVerb = 'WHEN'      then call doWhen
      when sVerb = 'XEQ'       then call doXEQ
      when sVerb = 'XMIT'      then call doXmit
      otherwise queue sLine     /* Ignore unknown verb */
    end
    if bResidue
    then queue getSub(sResidue)
  end
return

sayDebug: procedure expose g.
  parse arg sDebugMessage
  sDebugMessage = translate(sDebugMessage,'<>','[]')
  if g.0EMIT
  then say '|'sDebugMessage
  else say '-'sDebugMessage
return

queueHelpForVerb: procedure expose g.
  parse upper arg sVerb .
  do i = g.0HELPBEG.sVerb to g.0HELPEND.sVerb
    queue replace('&ast;','*',sourceline(i))
  end
return

queueHelpFromLabel: procedure
  parse arg sLabel
  do i = 1 until sourceline(i) = sLabel
  end
  do i = i+2 while sourceline(i) <> '*/'
    queue sourceline(i)
  end
return

doJAMComment:
/*
### ...         [JAM comment]

This is used to add comments to a JAM input file. These comments
are ignored by the JAM processor and do not produce any output.

For example, the following simply builds a job card and ignores the
preceding comments:

    ...
    ... Build a job card
    ...
    .. job

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'JAMComment'
return

doComment:
/*
### ..*         [comment]

  This will generate a comment using the specified comment text.
  The default comment style is `jcl`. Other built-in
  styles available include `asm`, `box`, `c`, `js`, `rexx`, and `xml`.
  Styles can be added or updated by using the `..style` JAM verb.

  For example:

      ..style xml
      ..* This is an XML comment

      ..style jcl
      ..* This is a JCL comment

  generates:

      <!--
       This is an XML comment
      -->

      //&ast;
      //&ast;-------------------------------------------------------------------*
      //&ast; This is a JCL comment                                             *
      //&ast;-------------------------------------------------------------------*
      //&ast;


*/
  parse var g.1 . sComment
  if sComment = '?'
  then call queueHelpForVerb 'Comment'
  else do
    sStyle = g.0STYLE
    sFirst = g.0STYLE_FIRST.sStyle
    sBorderLeft  = g.0STYLE_BORDER_LEFT.sStyle
    sBorderFill  = g.0STYLE_BORDER_FILL.sStyle
    sBorderRight = g.0STYLE_BORDER_RIGHT.sStyle
    sLeft  = g.0STYLE_LEFT.sStyle
    sRight = g.0STYLE_RIGHT.sStyle
    sLast  = g.0STYLE_LAST.sStyle
    nWidth = g.0STYLE_WIDTH.sStyle
    nInternalWidth = nWidth - length(sRight)
    if nInternalWidth <= 0 then nInternalWidth = nWidth
    nMaxWidth = nInternalWidth - length(sLeft) - 2
    if length(sBorderFill) > 0 /* If top and bottom borders are required */
    then sBorder = left(sBorderLeft,nInternalWidth,left(sBorderFill,1))sBorderRight
    else sBorder = '' /* else don't output a border */
    if length(sFirst) > 0 then queue sFirst
    if length(sBorder) > 0 then queue sBorder
    do i = 1 to g.0
      parse var g.i . sComment
      do until length(sComment) = 0
        if substr(sComment,nMaxWidth,1) = ' '
        then do /* word boundary */
          parse var sComment sChunk +(nMaxWidth) sComment
        end
        else do /* backoff to previous whole word */
          nLastBlank = lastpos(' ',sComment,nMaxWidth)
          if nLastBlank = 0 /* no word break, so we have to split */
          then parse var sComment sChunk +(nMaxWidth) sComment
          else parse var sComment sChunk +(nLastBlank) sComment
        end
        queue left(sLeft sChunk,nInternalwidth)sRight
        sComment = strip(sComment)
      end
    end
    if length(sBorder) > 0 then queue sBorder
    if length(sLast) > 0 then queue sLast
  end
return

addJCLComment: procedure expose g.
  parse arg sComment
  sStyle = 'jcl'
  sFirst = g.0STYLE_FIRST.sStyle
  sBorderLeft  = g.0STYLE_BORDER_LEFT.sStyle
  sBorderFill  = g.0STYLE_BORDER_FILL.sStyle
  sBorderRight = g.0STYLE_BORDER_RIGHT.sStyle
  sLeft  = g.0STYLE_LEFT.sStyle
  sRight = g.0STYLE_RIGHT.sStyle
  sLast  = g.0STYLE_LAST.sStyle
  nWidth = g.0STYLE_WIDTH.sStyle
  nInternalWidth = nWidth - length(sRight)
  if nInternalWidth <= 0 then nInternalWidth = nWidth
  nMaxWidth = nInternalWidth - length(sLeft) - 1
  if length(sBorderFill) > 0 /* If top and bottom borders are required */
  then sBorder = left(sBorderLeft,nInternalWidth,left(sBorderFill,1))sBorderRight
  else sBorder = '' /* else don't output a border */
  if length(sFirst) > 0 then queue sFirst
  if length(sBorder) > 0 then queue sBorder
  do until length(sComment) = 0
    if substr(sComment,nMaxWidth,1) = ' '
    then do /* word boundary */
      parse var sComment sChunk +(nMaxWidth) sComment
    end
    else do /* backoff to previous whole word */
      nLastBlank = lastpos(' ',sComment,nMaxWidth)
      if nLastBlank = 0 /* no word break, so we have to split */
      then parse var sComment sChunk +(nMaxWidth) sComment
      else parse var sComment sChunk +(nLastBlank) sComment
    end
    queue left(sLeft sChunk,nInternalwidth)sRight
    sComment = strip(sComment)
  end
  if length(sBorder) > 0 then queue sBorder
  if length(sLast) > 0 then queue sLast
return


doArgs:
/*
### ..ARGS      var [var...]

This will parse any supplied command-line arguments into the
specified list of variables. For example, suppose you had a JAM
input file called MY.JAM.INPUT containing:

      ..args system lo hi
      The parameters passed are [system], [lo] and [hi]

* You can run JAM in batch as follows:

      //STEP1 EXEC PGM=IKJEFT01,PARM='JAM TEST 0 10'
      //IN      DD DISP=SHR,DSN=MY.JAM.INPUT
      //OUT     DD DISP=SHR,DSN=MY.JAM.OUTPUT

  The MY.JAM.OUTPUT dataset would contain:

      The parameters passed are TEST, 0 and 10

* You can invoke JAM interactively (on the ISPF EDIT command line) as follows:

      JAM TEST 0 10

  A temporary output dataset would be created and edited containing:

      The parameters passed are TEST, 0 and 10

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Args'
  else do
    do i = 1 to g.0
      parse var g.i . sArgList
      interpret 'parse var sArgs' sArgList
    end
  end
return

doAsk:
/*
### ..ASK       var [default [prompt]]
### ..ASKU      var [default [prompt]]
### ..ASKQ      var [default [prompt]]
### ..ASKQU     var [default [prompt]]

   These JAM statements will ask the user for terminal input.

   The text specified by "prompt" is displayed to the user and
   the user's reply is assigned to REXX variable "var". The default reply
   is specified by "default" and if it contains spaces then it must be
   enclosed in apostrophes. The user can accept the default value by
   pressing Enter without supplying a reply. The default value, if
   present, will be shown enclosed in parentheses after the prompt text.
   For example,

   `..ask reply 'red door' Which door do you choose?`

   Will prompt the user as follows:

   `Which door do you choose? (red door):`

   The user can now either key in a non-blank reply or press Enter to
   accept the default shown in parentheses.

   If the prompt text is omitted then the prompt text will be the name
   of the REXX variable "var" that is to receive the reply text.
   For example,

   `..ask reply`

   Will prompt the user as follows:

   `Enter reply:`

   Variations of `..ask` will either convert the user's reply to
   uppercase and/or allow the user to reply Q to quit the JAM session.
   The variations are summarised below:

   | Verb  | Convert reply to uppercase? | Reply Q to Quit? |
   | ----- | --------------------------- | ---------------- |
   | ASK   | No                          | No               |
   | ASKU  | Yes                         | No               |
   | ASKQ  | No                          | Yes              |
   | ASKQU | Yes                         | Yes              |

   Another way to solicit user input is to put a question
   mark at the end of a REXX variable name within square brackets.
   For example:

   `..job [system?]`

   This will result in the user being prompted with:

   `Enter system:`

   You can also provide your own prompt using this syntax. For example:

   `..job [system Enter the system name?]`

   This will result in the user being prompted with:

   `Enter the system name:`

   In both cases, the reply will be saved in the REXX variable
   that was specified ("system" in the above examples) and will
   replace the content bounded by the square brackets.

*/
  parse var g.1 sVerb sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Ask'
  else do
    upper sVerb
    bTranslateToUpperCase = pos('U',sVerb) > 0
    bCheckForQuit = pos('Q',sVerb) > 0
    do i = 1 to g.0
      parse var g.i sCmd sVarName sDefault sPrompt
      sCmd = translate(sCmd)
      c = left(sDefault,1)
      select
        when c = "'" then parse var g.i . sVarName "'"sDefault"'" sPrompt
        when c = '"' then parse var g.i . sVarName '"'sDefault'"' sPrompt
        otherwise nop
      end
      if sPrompt = ''
      then sPrompt = 'Enter' sVarName
      else sPrompt = strip(sPrompt)
      if sDefault = ''
      then say sPrompt':'
      else say sPrompt '('sDefault'):'
      parse pull sReply
      if bCheckForQuit & (sReply = 'Q' | sReply = 'q')
      then do
        g.0QUIT = 1    /* Exit the JAM processor */
        g.0CANCEL = 1  /* Discard the temporary ISPF/EDIT file */
        call initStack /* Discard any stacked IF statements etc */
      end
      else do
        if sReply = '' then sReply = sDefault
        if bTranslateToUpperCase then upper sReply
        interpret sVarName "= '"toStr(sReply)"'"
      end
    end
  end
return

doAuto:
/*
### ..AUTO      [text]

   This is used to automatically run the JAM processor when a file is edited.

   When this statement is detected by the JAMINIT ISPF/EDIT initial macro
   in the **first** line of the file that the user chooses to EDIT or VIEW
   then the JAM processor is automatically run. This saves the user from
   having to manually invoke the JAM processor on the edit command line.
   The "text" (if present) is displayed to the user and
   can be used to let the user know what is happening.

   To enable this facility you need to issue (one time only) the
   following ISPF/EDIT command:

     IMACRO JAMINIT

   To disable this facility, issue:

     IMACRO NONE

*/
  parse var g.1 . sParms
  select
    when sParms = ''  then nop
    when sParms = '?' then call queueHelpForVerb 'Auto'
    otherwise say space(sParms)
  end
return

doBackup:
/*
### ..BACKUP    dsn backup        [options...]

  This will generate a job step that will backup dataset(s) "dsn" to
  a backup dataset called "backup" using ADRDSSU DUMP.

*/
  bError = 0
  sBackupPrev = userid()'.BACKUP'
  sOptions = ''
  parse upper var g.1 . sDataset sBackup .
  do i = 1 to g.0
    parse upper var g.i . sDataset . sOption
    sOptions = sOptions sOption /* Accumulate options */
    if sDataset = '?' | sDataset = ''
    then bError = 1
  end
  if bError
  then do
    call queueHelpForVerb 'Backup'
    return
  end
  call addJCLCommentBlock 'Backup dataset'
  queue getStep('PGM=IEFBR14')
  queue '//BACKUP    DD DSN='sBackup','
  queue '//             DISP=(MOD,DELETE,DELETE),'
  queue '//             UNIT=SYSALLDA,SPACE=(TRK,(1,0))'
  if run = 0
  then queue getStep("PGM=ADRDSSU,PARM='TYPRUN=NORUN'")
  else queue getStep("PGM=ADRDSSU PARM='TYPRUN=NORUN'")
  if sBackup = '' | sBackup = '='
  then sBackup = sBackupPrev
  sBackupPrev = sBackup
  if space = 'SPACE'
  then do
    space = '(CYL,(1000,400),RLSE) <-- CHECK'
    say "JAM0400W Assuming: .set space = '(CYL,(1000,400),RLSE)'"
  end
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//BACKUP    DD DSN='sBackup','
  queue '//             DISP=(NEW,CATLG,DELETE),'
  queue '//             UNIT=SYSALLDA,SPACE='space
  queue '//SYSIN     DD *'
  queue '  DUMP DATASET -'
  queue '       ( -'
  queue '         INCLUDE -'
  queue '         ( -'
  do i = 1 to g.0
    parse upper var g.i . sDataset .
    queue '          'sDataset '-'
  end
  queue '         ) -'
  queue '       ) -'
  queue '       OUTDDNAME (BACKUP) -'
  queue '       ALLDATA   (*) -'
  queue '       ALLEXCP -'
  queue '       PROCESS   (SYS1) -'
  queue '       TOLERATE  (ENQFAILURE) -'
  do i = 1 to words(sOptions)
    queue '       'word(sOptions,i) '-'
  end
  queue ' '
  queue '/*'
return

doBR14:
/*
### ..BR14      [label]

  This will generate a dummy (IEFBR14) JCL step.

  If the step label is omitted then the next
  sequential step number (in the REXX variable "step") is used to
  create a label for you.

  For example,

    ..br14

  generates

    //STEP1   EXEC PGM=IEFBR14

*/
  parse var g.1 . sLabel .
  if sLabel = '?'
  then call queueHelpForVerb 'BR14'
  else do
    do i = 1 to g.0
      parse var g.i . sLabel .
      if sLabel = ''
      then queue getStep('PGM=IEFBR14')
      else queue '//'left(sLabel,8) 'EXEC PGM=IEFBR14'
    end
  end
return

doCatalog:
/*
### ..CATALOG   dsn volser [catalog]

  This will generate a job step that will catalog dataset "dsn" on
  volume "volser" in the specified catalog, or else in
  the catalog appropriate for the "alias" system. The
  following pre-defined catalog variables can be used:

  | Variable | Description |
  | -------- | ----------- |
  | cat      | The master catalog for the alias system. |

*/
  call addJCLCommentBlock 'Catalog dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Catalog'
  else do
    queue getStep('PGM=IDCAMS')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser sCatalog .
      if sCatalog = '' then sCatalog = cat
      queue '  DEFINE NONVSAM ( -'
      queue '                  NAME('sDataset') -'
      queue '                  VOLUMES     ('sVolser') -'
      if left(sVolser,1) = '&' | sVolser = '******'
      then queue '                  DEVICETYPES (0000) -'
      else queue '                  DEVICETYPES (3390) -'
      queue '                 ) -'
      queue '         CATALOG ('sCatalog')'
    end
    queue '/*'
  end
return

doCompress:
/*
### ..COMPRESS  dsn [volser]

  This will generate a job step that will compress partitioned dataset "dsn" on
  volume "volser", or else will compress the cataloged dataset
  if the volume is omitted.

*/
  call addJCLCommentBlock 'Compress PDS'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Compress'
  else do
    queue getStep('PGM=IEBCOPY')
    queue '//SYSPRINT  DD SYSOUT=*'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      if sDataset = ''
      then call queueHelpForVerb 'Compress'
      else do
        if sVolser <> ''
        then do
          queue '//'left('DD'i,9) 'DD DISP=SHR,DSN='sDataset','
          queue '//             UNIT=SYSALLDA,VOL=SER='sVolser
        end
        else do
          queue '//'left('DD'i,9) 'DD DISP=SHR,DSN='sDataset
        end
      end
    end
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      if sDataset <> ''
      then queue ' COPY INDD=DD'i',OUTDD=DD'i
    end
    queue '/*'
  end
return

doCopy:
/*
### ..COPY      fromdsn todsn
### ..COPY      frompds(member,member,...) todsn [tovol] [fromvol]
### ..COPY      fromgdg(n) todsn [tovol]
### ..COPY      frompath todsn [options...]
### ..COPY      fromdsn topath [options...]
### ..COPY      frompath topath
### ..COPY      * todsn
### ..COPY      [fromsite]:fromdsn [tosite]:todsn [options...]
### ..COPY      [fromsite]:frompath [tosite]:topath [options...]

  This will generate a job step that, depending on the operands, will copy either:
  - Dataset "fromdsn" to dataset "todsn"
  - Member(s) in "frompds" to PDS "todsn"
  - Generation data group "n" to dataset "todsn"
  - USS path "frompath" to dataset "todsn"
  - Dataset "fromdsn" to USS path "topath"
  - USS path "frompath" to USS path "topath"

  If "=" is specified for the "tovol", then the volser
  from the catalog is used.

  If "*" is specified for the "fromdsn" then the data is read
  from inline JCL until a $$ terminator is seen.

  If a colon (:) prefixes the dataset name then proxy
  FTP is used to copy (transfer) the dataset or file from
  the source "site" to the target "site". The "site" is
  a user-defined LPAR alias, and it defaults to the LPAR on which the
  job is to be run.

*/

  parse var g.1 . sSource sTarget sVolser .
  sPrevUtil = getCopyUtility(sSource,sTarget)
  nFrom = 1
  nTo = 0
  sPrevSource = ''
  /* Group similar types of copy operation into the same utility */
  do nThis = 1 to g.0
    parse var g.nThis . sSource sTarget sVolser .
    if sSource = '=' then sSource = sPrevSource
    sPrevSource = sSource
    if sTarget = '=' then sTarget = sSource
    sThisUtil = getCopyUtility(sSource,sTarget)
    if sThisUtil <> sPrevUtil
    then do
      nTo = nThis - 1
      call performCopyOperation nFrom nTo
      nFrom = nThis
      sPrevUtil = sThisUtil
    end
  end
  if nTo < g.0 then call performCopyOperation nTo+1 g.0
return

getCopyUtility: procedure
  parse arg sSource,sTarget
  sFromTo = getFileType(sSource) getFileType(sTarget)
  select
    when wordpos('FTP',sFromTo) > 0 then sUtility = 'FTP'
    when sFromTo = 'PTH PTH' then sUtility = 'BPXBATCH'
    when sFromTo = 'MEM MEM' then sUtility = 'IEBCOPY'
    when sFromTo = 'MEM DSN' then sUtility = 'IEBCOPY'
    when sFromTo = 'PTH DSN' then sUtility = 'IKJEFT01'
    when sFromTo = 'PTH MEM' then sUtility = 'IKJEFT01'
    when sFromTo = 'DSN PTH' then sUtility = 'IKJEFT01'
    when sFromTo = 'MEM PTH' then sUtility = 'IKJEFT01'
    when sFromTo = 'DSN DSN' then sUtility = 'ADRDSSU'
    when sFromTo = 'INL DSN' then sUtility = 'IEBGENER'
    when sFromTo = 'INL MEM' then sUtility = 'IEBGENER'
    otherwise sUtility = 'ERROR'
  end
return sUtility

performCopyOperation:
  parse arg nFrom nTo
  select
    when sPrevUtil = 'BPXBATCH' then call CopyUss nFrom nTo
    when sPrevUtil = 'IEBCOPY'  then call CopyMem nFrom nTo
    when sPrevUtil = 'IEBGENER' then call CopyGen nFrom nTo
    when sPrevUtil = 'IKJEFT01' then call CopyPth nFrom nTo
    when sPrevUtil = 'ADRDSSU'  then call CopyDsn nFrom nTo
    when sPrevUtil = 'FTP'      then call CopyFtp nFrom nTo
    otherwise call queueHelpForVerb 'Copy'
  end
return

getFileType: procedure
  parse arg sFile
  select
    when sFile = '*'       then sFileType = 'INL'
    when pos(':',sFile) >0 then sFileType = 'FTP'
    when isPath(sFile)     then sFileType = 'PTH'
    when hasMember(sFile)  then sFileType = 'MEM'
    when sFile = ''        then sFileType = 'ERR'
    when sFile = '?'       then sFileType = 'ERR'
    otherwise                   sFileType = 'DSN'
  end
return sFileType

CopyFtp:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Copy using proxy FTP',nFrom,nTo
  call queueFTPStep
  parse var g.nFrom . sSource sTarget sOptions
  if pos(':',sSource) > 0
  then parse var sSource sPri':'sSource
  else sPri = ''
  if pos(':',sTarget) > 0
  then parse var sTarget sSec':'sTarget
  else sSec = ''
  if sPri = '' then sPri = alias
  if sSec = '' then sSec = alias
  sLastPri = toUpper(sPri)
  sLastSec = toUpper(sSec)
  sLastOpt = toUpper(sOptions)
  if binary = 1
  then sTranslate = 'binary'
  else sTranslate = 'ebcdic'
  queue sPri
  queue sTranslate
  queue 'site ispfstats'
  queue 'proxy open' sSec
  queue 'proxy' sTranslate
  queue 'proxy site ispfstats'
  do i = 1 to words(sOptions) /* Establish any user options  */
    queue 'proxy site' word(sOptions,i)
  end
  do i = nFrom to nTo
    parse var g.i . sSource sTarget sOptions
    if pos(':',sSource) > 0
    then parse var sSource sPri':'sSource
    else sPri = ''
    if pos(':',sTarget) > 0
    then parse var sTarget sSec':'sTarget
    else sSec = ''
    if sPri = '' then sPri = alias
    if sSec = '' then sSec = alias
    sPriUpper = toUpper(sPri)
    sSecUpper = toUpper(sSec)
    sOptUpper = toUpper(sOptions)
    select
      when sPriUpper = sLastPri & sSecUpper = sLastSec then do
        if sOptUpper <> sLastOpt
        then do
          do j = 1 to words(sOptions) /* Establish any user options  */
            queue 'proxy site' word(sOptions,j)
          end
        end
      end
      when sPriUpper <> sLastPri then do /* primary host has changed */
        queue 'proxy close'         /* close old secondary */
        queue 'close'               /* close old primary   */
        queue 'open' sPri           /* open  new primary   */
        queue sTranslate
        queue 'proxy open' sSec     /* open  new secondary */
        queue 'proxy' sTranslate
        queue 'proxy site ispfstats'
        do j = 1 to words(sOptions) /* reestablish any user options  */
          queue 'proxy site' word(sOptions,j)
        end
      end
      otherwise do                  /* same primary, new secondary */
        queue 'proxy close'         /* close old secondary */
        queue 'proxy open' sSec     /* open  new secondary */
        queue 'proxy' sTranslate
        queue 'proxy site ispfstats'
        do j = 1 to words(sOptions) /* reestablish any user options  */
          queue 'proxy site' word(sOptions,j)
        end
      end
    end
    /* The "get" is executed as though client is connect to the target */
    if sTarget = ''
    then call queueFTP "proxy get '"sSource"'"
    else call queueFTP "proxy get '"sSource"' '"sTarget"'"
    sLastPri = toUpper(sPri)
    sLastSec = toUpper(sSec)
    sLastOpt = toUpper(sOptions)
  end
  queue 'quit'
  queue '/*'
return

CopyUss:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Copy Unix files to/from Unix files',nFrom,nTo
  do i = nFrom to nTo
    parse var g.i sVerb sOperands
    g.i = sVerb 'cp -pv' sOperands
  end
  call queueScript 'su',nFrom,nTo
return

CopyPth:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Copy Unix files to/from datasets',nFrom,nTo
  call queueTSOStep
  do i = nFrom to nTo
    parse var g.i . sSource sTarget sOptions
    select
      when isPath(sSource) then do
        sTarget = translate(sTarget)
        call queueTSO "OGET '"sSource"' '"sTarget"'" sOptions
      end
      when isPath(sTarget) then do
        sSource = translate(sSource)
        call queueTSO "OPUT '"sSource"' '"sTarget"'" sOptions
      end
      otherwise nop
    end
  end
  queue '/*'
return

CopyMem:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Copy members',nFrom,nTo
  drop z.
  z. = ''
  z.0DD.0 = 0
  queue getStep('PGM=IEBCOPY')
  queue '//SYSPRINT  DD SYSOUT=*'
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sTargetVol sSourceVol .
    if sTargetVol = '=' then sTargetVol = '' /* use cataloged target */
    parse var sSource sSource'('sMemberList')'
    parse var sTarget sTarget'('sTargetList')'
    if sMemberList <> ''
    then do
      if sTarget = ''  then sTarget = sSource
      if sTarget = '=' then sTarget = sSource
      if isNotCached(sSource,sSourceVol)
      then do
        sDD = getDD(sSource,sSourceVol)
        if sSourceVol = ''
        then do
          queue '//'left(sDD,9) 'DD DISP=SHR,DSN='sSource
        end
        else do
          queue '//'left(sDD,9) 'DD DISP=SHR,DSN='sSource','
          queue '//'left('' ,9) '   UNIT=SYSALLDA,VOL=SER='sSourceVol
        end
      end
      if isNotCached(sTarget,sTargetVol)
      then do
        sDD = getDD(sTarget,sTargetVol)
        if sTargetVol = ''
        then do
          queue '//'left(sDD,9) 'DD DISP=SHR,DSN='sTarget
        end
        else do
          queue '//'left(sDD,9) 'DD DISP=SHR,DSN='sTarget','
          queue '//'left('' ,9) '   UNIT=SYSALLDA,VOL=SER='sTargetVol
        end
      end
    end
  end
  queue '//SYSIN     DD *'
  sPrevCopy = ''
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sTargetVol sSourceVol .
    if sTargetVol = '=' then sTargetVol = '' /* use cataloged target */
    parse var sSource sSource'('sMemberList')'
    parse var sTarget sTarget'('sTargetList')'
    if sMemberList <> ''
    then do
      if sTarget = ''  then sTarget = sSource
      if sTarget = '=' then sTarget = sSource
      sMemberList = translate(sMemberList,' ',',*')
      sTargetList = translate(sTargetList,' ',',*')
      sInDD  = getDD(sSource,sSourceVol)
      sOutDD = getDD(sTarget,sTargetVol)
      sCopy = '  COPY INDD=(('sInDD',R)),OUTDD='sOutDD
      if sCopy <> sPrevCopy
      then do
        queue sCopy
        sPrevCopy = sCopy
      end
      do j = 1 to words(sMemberList)
        sSourceMember = word(sMemberList,j)
        sTargetMember = word(sTargetList,j)
        if sTargetMember = ''
        then queue '  SELECT MEMBER=('sSourceMember')'
        else queue '  SELECT MEMBER=(('sSourceMember','sTargetMember'))'
      end
    end
  end
  queue '/*'
return

isNotCached: procedure expose z.
  arg sDataset,sVolser
  sKey = sDataset || sVolser
return z.0DD.sKey = ''

getDD: procedure expose z.
  arg sDataset,sVolser
  sKey = sDataset || sVolser
  if z.0DD.sKey = ''
  then do
    n = z.0DD.0 + 1
    z.0DD.0 = n
    z.0DD.sKey = 'DD'z.0DD.0
    z.0DD.n    = sDataset
  end
return z.0DD.sKey


CopyDsn:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Copy dataset',nFrom,nTo
  queue getStep('PGM=ADRDSSU')
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//SYSIN     DD *'
  queue '  COPY DATASET -'
  queue '       ( -'
  queue '         INCLUDE -'
  queue '         ( -'
  do i = nFrom to nTo
    parse upper var g.i . sSource .
    queue '          'sSource '-'
  end
  queue '         ) -'
  queue '       ) -'
  queue '       RENAMEUNCONDITIONAL -'
  queue '       ( -'
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget .
    queue '         ( -'
    queue '          'sSource', -'
    queue '          'sTarget '-'
    queue '         ) -'
  end
  queue '       ) -'
  queue '       TOLERATE(ENQFAILURE)'
  queue '/*'
return

CopyGen:
  parse arg nFrom nTo
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sToVol .
    call addJCLCommentBlock 'Copy sysin',nFrom,nTo
    queue getStep('PGM=IEBGENER')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD DUMMY'
    if sToVol == ''
    then do
      queue '//SYSUT2    DD DISP=SHR,DSN='sTarget
    end
    else do
      queue '//SYSUT2    DD DISP=SHR,DSN='sTarget','
      queue '//             UNIT=SYSALLDA,VOL=SER='sToVol
    end
    queue '//SYSUT1    DD DATA,DLM=$$ <-- Data must be terminated by $$'
  end
return

doDateVars:
/*
### ..DATEVARS  [date [+|-days]]
### ..DATEVARS  NEXT dayname [AFTER date]
### ..DATEVARS  PREV dayname [BEFORE date]
### ..DATEVARS  FIRST dayname IN month
### ..DATEVARS  LAST dayname IN month
### ..DATEVARS  LAST dayname
### ..DATEVARS  EASTER [year]

  Takes the specified date and generates several REXX
  variables containing different aspects of that date.

  If "+" or "-" followed by a whole number days is
  specified then that number of days is added to the
  date before generating any REXX variables.

  The "date" can be in a variety of formats - including some that
  are computed (e.g. FRIDAY, NEXT SATURDAY, EASTER 2021, etc).
  Unrecognised dates are silently assumed to be the current date.
  Examples of "date" values that you can specify include:

  | date           |  Interpreted as                          |
  | ----           |  --------------------------------------- |
  | 25/2/66        |  1966/02/25                              |
  | 25/2/1966      |  1966/02/25                              |
  | 25/2           |  yyyy/02/25 (in the current year)        |
  | 2/25           |  yyyy/02/25 (in the current year)        |
  | 2/25/66        |  1966/02/25                              |
  | 2/25/1966      |  1966/02/25                              |
  | 25 Feb 1966    |  1966/02/25                              |
  | 25 Feb 66      |  1966/02/25                              |
  | 25 Feb         |  yyyy/02/25 (in the current year)        |
  | 25FEB          |  yyyy/02/25 (in the current year)        |
  | February 25    |  yyyy/02/25 (in the current year)        |
  | Feb            |  yyyy/02/01 (in the current year)        |
  | 1966 Feb       |  1966/02/01                              |
  | 1966 Feb 25    |  1966/02/25                              |
  | 1966           |  1966/01/01 (first day of that year)     |
  | 66056          |  1966/02/25 (Julian yyddd)               |
  | 66.056         |  1966/02/25 (Julian yy.ddd)              |
  | 250266         |  1966/02/25 (ddmmyy)                     |
  | 717756         |  1966/02/25 (days since 1/1/1900)        |
  | +7             |             (the current date + 7 days)  |
  | -7             |             (the current date - 7 days)  |
  | <unrecognised> |  yyyy/mm/dd (the current date)           |

  The resulting REXX variables created for the specified date are:

  | Variable  | Example         | Description                         |
  | --------  | --------------  | ----------------------------------- |
  | datevar   | 25/2/1966       | The input date passed to ..datevars |
  | basedate  | 717756          | Days since 1900/01/01               |
  | date      | Fri 25 Feb 1966 | REXX default date format            |
  | dayname   | Friday          | Long day name                       |
  | day       |  Fri            | Short day name                      |
  | dd        |  25             | 2-digit day number                  |
  | mm        |  02             | 2-digit month number                |
  | yy        |  66             | 2-digit year                        |
  | yyyy      |  1966           | 4-digit year                        |
  | yyddd     |  66056          | Julian date                         |
  | ddd       |  056            | Days since yyyy/01/01               |
  | yymmdd    |  66/02/25       | Short sortable date format          |
  | ddmmyy    |  25/02/66       | European date format                |
  | month     |  February       | Long month name                     |
  | mon       |  Feb            | Short month name                    |
  | yyyymmdd  |  1966/02/25     | Long sortable date format           |
  | ddmmyyyy  |  25/02/1966     | Long European date format           |
  | days      |  -20000         | Days since today                    |

  For example:

      Input                               Resulting date               Comment
      ---------------------------------   --------------------------   -------
    ..datevars 25/2/1966
      [datevar                         ]  [dayname date             ]
    ..datevars Easter 2021
      [datevar                         ]  [dayname date             ]
    ..datevars prev friday before easter 2021
      [datevar                         ]  [dayname date             ]  (Good Friday)
    ..datevars first monday in january 2021
      [datevar                         ]  [dayname date             ]  (1st Monday)
    ..datevars [date] +7
      [datevar                         ]  [dayname date             ]  (2nd Monday)
    ..datevars [date] +7
      [datevar                         ]  [dayname date             ]  (3rd Monday)
    ..datevars [date] +7
      [datevar                         ]  [dayname date             ]  (4th Monday)
    ..datevars last saturday in march 2021
      [datevar                         ]  [dayname date             ]

  generates:

      Input                               Resulting date               Comment
      ---------------------------------   --------------------------   -------
      25/2/1966                           Friday 25 Feb 1966
      Easter 2021                         Sunday 4 Apr 2021
      prev friday before easter 2021      Friday 2 Apr 2021            (Good Friday)
      first monday in january 2021        Monday 4 Jan 2021            (1st Monday)
      4 Jan 2021 +7                       Monday 11 Jan 2021           (2nd Monday)
      11 Jan 2021 +7                      Monday 18 Jan 2021           (3rd Monday)
      18 Jan 2021 +7                      Monday 25 Jan 2021           (4th Monday)
      last saturday in march 2021         Saturday 27 Mar 2021

*/
  parse var g.1 . date
  datevar = date /* just in case the original input is wanted */
  if date = '?'
  then call queueHelpForVerb 'DateVars'
  else do
    /* datevars some date +n        */
    /* datevars some date +n stem.  */
    /* datevars some date stem.     */
    /* datevars some date           */
    /* datevars stem.               */
    /* datevars                     */
    stem = ''
    tempdate = ''
    do i = 1 to words(date)
      sWord = word(date,i)
      if right(sWord,1) = '.' /* If a REXX stem variable was specified */
      then do
        stem = sWord          /* Remember the stem name */
      end
      else do
        tempdate = tempdate sWord
      end
    end
    date = strip(tempdate)
    /* datevars some date +n        */
    /* datevars some date           */
    /* datevars                     */
    delta = 0
    nWords = words(date)
    if nWords > 1
    then do
      delta = word(date,nWords)
      if pos(left(delta,1),'+-') = 0
      then delta = 0
      else date = subword(date,1,nWords-1)
    end
    if \datatype(delta,'WHOLE')
    then delta = 0
    if left(date,1) <> '-'
    then date = translate(date,'/','-') /* Normalise Euro date separators */
    basedate = getBaseDate(date)
    /* Add the specified offset in days... */
    basedate = basedate + delta
    /* Convert the resulting basedate to other formats... */
    parse value date('STANDARD',basedate,'BASE') with yyyy +4 mm +2 dd +2
    date         = date(,basedate,'BASE')
    yy           = right(yyyy,2)
    ddd          = right(date('DAYS',basedate,'BASE'),3,'0')
    yyddd        = yy || ddd
    yymmdd       = yy'/'mm'/'dd
    ddmmyy       = dd'/'mm'/'yy
    yyyymmdd     = yyyy'/'mm'/'dd
    ddmmyyyy     = dd'/'mm'/'yyyy
    dayname      = date('WEEKDAY',basedate,'BASE')
    day          = left(dayname,3)
    month        = date('MONTH',basedate,'BASE')
    mon          = left(month,3)
    days         = basedate - date('BASE')
    if stem <> '' /* if these are to be assigned to a stem variable */
    then do       /* create stem.xxx variables and drop xxx */
      vars = 'basedate yyyy mm dd date datevar yy ddd yyddd yymmdd',
             'ddmmyy dayname day month mon days'
      do i = 1 to words(vars)
        var = word(vars,i)
        interpret 'value='var'; drop' var';' stem||var '= value'
      end
    end
  end
return

getBaseDate: procedure
  arg date
  nDayNumber = getDayNumber(date) /* Just in case date is a day name    */
  select
    when nDayNumber <> -1 then do /* MO or TUE or WEDNESDAY for example */
      basedate = date('BASE') - date('BASE')//7 + nDayNumber
                  /* today - days since Monday + day requested           */
                  /*                             (0=MON, 1=TUE, etc)     */
    end
    when datatype(date,'WHOLE') then do /* Some kind of numeric date */
      nDate = length(date)
      select
        when date < 0 | left(date,1) = '+' then do
          /* Relative day (-n or +n) */
          basedate = date('BASE') + date
        end
        when nDate = 4 then do
          /* Possibly a year (from 0001 to 9999) */
          basedate = date('BASE',right(date,4,'0')'0101','STANDARD')
        end
        when nDate = 5 & right(date,3) <= 365 then do
          /* Possibly yyddd (Julian) */
          basedate = date('BASE',date,'JULIAN')
        end
        when nDate = 6 & date >= 010101 & date <= 311299 then do
          /* Possibly ddmmyy */
          parse var date dd +2 mm +2 yy +2
          basedate = getBaseYMD(yy,mm,dd)
        end
        when nDate = 8 & date >= 01010001 & date <= 31129999 then do
          /* Possibly ddmmyyyy */
          parse var date dd +2 mm +2 yyyy +4
          basedate = getBaseYMD(yyyy,mm,dd)
        end
        when date <= 3652058 then do /* base date of 31/12/9999 */
          /* Possibly just a base date */
          basedate = date
        end
        otherwise do
          /* Unrecognised number, so just use today's date */
          basedate = date('BASE')
        end
      end
    end
    when pos('/',date) > 0 then do /* Possibly d/m/y or d-m-y etc */
      parse var date d'/'m'/'y . /* Euro date separators already converted to '/' */
      select
        when isDay(d) & isMonth(m) then do
          /* Possibly dd/mm/yy or dd/mm */
          basedate = getBaseYMD(y,m,d)
        end
        when isDay(m) & isMonth(d) then do
          /* Possibly mm/dd/yy or mm/dd */
          basedate = getBaseYMD(y,d,m)
        end
        otherwise do
          /* Possibly yy/mm/dd */
          basedate = getBaseYMD(d,m,y)
        end
      end
    end
    when pos('.',date) > 0 then do
      /* Possibly Julian in fractional notation (yy.ddd or yyyy.ddd) */
      parse var date yy'.'ddd
      if ddd > 0 & ddd <= 365
      then do
        if yy > 99
        then do
          yyyy = right(yy,4,'0')
          basedate = date('BASE',yyyy'0101','STANDARD')+ddd-1
        end
        else do
          basedate = date('BASE',yy||ddd,'JULIAN')
        end
      end
      else do
        basedate = date('BASE')
      end
    end
    otherwise do
      /* Possibly February 25 1966, or 25Feb66, or February 1966, etc... */
      date = tokeniseDate(date) /* 25FEB1966 --> 25 FEB 1966 */
      months = 'JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
      parse upper var date t1 t2 t3 t4 t5 t6 .
      select
        when t1 = 'EASTER' then do
            /* EASTER [yyyy]            */
            if isNum(t2)
            then basedate = getEaster(t2)
            else basedate = getEaster(left(date('STANDARD'),4))
        end
        when inSet(left(t2,3),months) then do  /* x month, or x month x */
            if isDay(t1)
            then basedate = getBaseYMD(t3,t2,t1) /* d month y */
            else basedate = getBaseYMD(t1,t2,t3) /* y month d */
        end
        when inSet(left(t1,3),months) then do  /* month, or month x, or month x x */
            if isDay(t2)
            then basedate = getBaseYMD(t3,t1,t2) /* month d, or month d y */
            else basedate = getBaseYMD(t2,t1,t3) /* month, or month y */
        end
        otherwise do
          d2 = getDayNumber(t2) /* Convert dayname to day number of week */
          if d2 <> -1 /* If d2 is a valid day number (0 to 6) */
          then select                 /* t1   t2       t3    t4    */
            when t1 = 'NEXT'  then do /* NEXT dayname [AFTER date] */
              if t3 = 'AFTER'
              then basedate = dayAfter(d2,getBaseDate(t4 t5 t6)) /* NEXT dayname AFTER date */
              else basedate = dayAfter(d2,date('BASE'))          /* NEXT dayname (...after today) */
            end
            when t1 = 'PREV'  then do /* PREV dayname [BEFORE date] */
              if t3 = 'BEFORE'
              then basedate = dayBefore(d2,getBaseDate(t4 t5 t6)) /* PREV dayname BEFORE date */
              else basedate = dayBefore(d2,date('BASE'))          /* PREV dayname (...before today) */
            end
            when t1 = 'FIRST' then do /* FIRST dayname [IN month | AFTER date] */
              select
                when t3 = 'IN'     then basedate = dayAfter(d2,thisMonth(getBaseDate(t4 t5 t6))-1) /* FIRST dayname IN month */
                when t3 = 'AFTER'  then basedate = dayAfter(d2,getBaseDate(t4 t5 t6))              /* FIRST dayname AFTER date */
                otherwise               basedate = dayAfter(d2,date('BASE'))                       /* FIRST dayname (...in this month) */
              end
            end
            when t1 = 'LAST'  then do /* LAST dayname [IN month | BEFORE date] */
              select
                when t3 = 'IN'     then basedate = dayBefore(d2,nextMonth(getBaseDate(t4 t5 t6)))  /* LAST dayname IN month */
                when t3 = 'BEFORE' then basedate = dayBefore(d2,getBaseDate(t4 t5 t6))             /* LAST dayname BEFORE date */
                otherwise               basedate = dayBefore(d2,date('BASE'))                      /* LAST dayname (...before today) */
              end
            end
            otherwise basedate = date('BASE') /* Unrecognisable date */
          end
          else basedate = date('BASE')/* Unrecognisable date */
        end
      end
    end
  end
return basedate

thisMonth: procedure
  /* Returns base date of the 1st day of the specified month */
  arg nBaseDate
  parse value date('STANDARD',nBaseDate,'BASE') with yyyy +4 mm +2 dd +2
return date('BASEDATE',yyyy||mm'01','STANDARD')

nextMonth: procedure
  /* Returns base date of the 1st of the month after the specified date */
  arg nBaseDate
  parse value date('STANDARD',nBaseDate,'BASE') with yyyy +4 mm +2 dd +2
  mm = mm + 1
  if mm > 12
  then do
    yyyy = yyyy + 1
    mm = 1
  end
return date('BASEDATE',yyyy||right(mm,2,0)'01','STANDARD')

dayAfter: procedure
  /* Returns base date of the next day (eg Monday) after the specified date */
  arg nDay,nBaseDate
  nDiff = nDay - nBaseDate // 7
  if nDiff > 0
  then nBaseDate = nBaseDate + nDiff
  else nBaseDate = nBaseDate + nDiff + 7
return nBaseDate

dayBefore: procedure
  /* Returns base date of the previous day (eg Monday) after the specified date */
  arg nDay,nBaseDate
  nDiff = nBaseDate // 7 - nDay
  if nDiff > 0
  then nBaseDate = nBaseDate - nDiff
  else nBaseDate = nBaseDate - nDiff - 7
return nBaseDate

getEaster: procedure
  /* Returns base date of Easter Sunday for the specified year. */
  /* Adapted from Ian Taylor's "Computus" blog post at          */
  /* http://chilliant.blogspot.com/2010/12/computus-1.html      */
  arg year
  a = year // 19
  b = trunc(year / 4)
  c = b % 25 + 1
  d = trunc(c * 3 / 4)
  e = ((a * 19) - trunc((c * 8 + 5) / 25) + d + 15) // 30
  e = e + trunc((29578 - a - e * 32) / 1024)
  e = e - ((year // 7) + b - d + e + 2) // 7
  d = trunc(e / 32)
  day = e - d * 31
  month = d + 3
return getBaseYMD(year,month,day)

getDayNumber: procedure expose g.
  arg sDay
  sDays = 'MONDAY TUESDAY WEDNESDAY THURSDAY FRIDAY SATURDAY SUNDAY'
  bIsDayName = 0
  do i = 1 to 7 until bIsDayName
    bIsDayName = bIsDayName | abbrev(word(sDays,i),sDay,2)
  end
  if bIsDayName
  then nDayNumber = i-1 /* 0=Mon, 2=Tue, ... 6=Sun */
  else nDayNumber = -1  /* -1=Invalid day name     */
return nDayNumber

tokeniseDate: procedure expose g.
  /* Insert a space between blocks of digits and alphabetics */
  arg s /* 25FEB1966 */
  o = ''
  lastt = ''
  s = translate(s,'','-') /* Remove punctuation */
  do i = 1 to length(s)   /* Convert '25FEB1966' to '25 FEB 1966' */
    c = substr(s,i,1)
    t = translate(c,'9999999999AAAAAAAAAAAAAAAAAAAAAAAAAA',,
                    '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ')
    if t = lastt
    then o = o||c
    else o = o c
    lastt = t
  end
return space(o) /* 25 FEB 1966 */

isMonth: procedure expose g.
  arg m
return datatype(m,'WHOLE') & m > 0 & m <= 12

getMonth: procedure expose g.
  arg m
  if isMonth(m)
  then mm = right(m,2,'0')
  else do
    m3 = left(m,3)
    months = 'JAN FEB MAR APR MAY JUN JUL AUG SEP OCT NOV DEC'
    n = wordpos(m3,months)
    if n = 0 then n = 1
    mm = right(n,2,'0')
  end
return mm /* 01 when month is invalid */

isDay: procedure expose g.
  arg d
return datatype(d,'WHOLE') & d > 0 & d <= 31

getDay: procedure expose g.
  arg d
  if isDay(d)
  then dd = right(d,2,'0')
  else dd = '01'
return dd /* 01 when day is invalid */

getBaseYMD: procedure expose g.
  arg y,m,d
  dd = getDay(d)
  mm = getMonth(m)
  if datatype(y,'WHOLE')
  then do
    if y > 99
    then do
      yyyy = right(y,4,'0')
      basedate = date('BASE',yyyy||mm||dd,'STANDARD')
    end
    else do
      yy = right(y,2,'0')
      basedate = date('BASE',yy'/'mm'/'dd,'ORDERED')
    end
  end
  else do
    thisyear = left(date('STANDARD'),4)
    basedate = date('BASE',thisyear||mm||dd,'STANDARD')
  end
return basedate

doDelete:
/*
### ..DELETE    dsn [catalog] [options...]

  This will generate a job step that will delete dataset "dsn" from the
  specified catalog, or else from the catalog appropriate
  for the "alias" system. You can also specify any options
  valid for IDCAMS DELETE in "options".

*/
  call addJCLCommentBlock 'Delete dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Delete'
  else do
    queue getStep('PGM=IDCAMS')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sCatalog sOptions
      if sCatalog = '' then sCatalog = cat
      queue '  DELETE ('sDataset') -'
      if sOptions <> ''
      then do j = 1 to words(sOptions)
        queue '         'word(sOptions,j) '-'
      end
      queue '         CATALOG   ('sCatalog')'
    end
    queue '/*'
  end
return

doElse:
/*
### ..ELSE      [action]

  This is part of an if-else-end construct:

    ..if [condition]
      output if condition evaluates to 1
    ..else
      output if condition evaluates to 0
    ..end

  If the previous `..if` JAM statement evaluated as
  false, then process the ELSE "action" (if present) and all
  subsequent statements until the closing `..end`
  JAM statement is found.

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'Else'
  else do
    parse var g.1 . sAction
    parse value peekStack() with sClause bEmit bCondition .
    g.0EMIT = bEmit & \bCondition
/*  say left('',2*g.0T),
    'ELSE: emit='g.0EMIT 'cond='bCondition */
    if g.0EMIT
    then do
      if sAction <> '' & \bCondition
      then interpret sAction
    end
  end
return

doEnd:
/*
### ..END

  This closes the previous matching `..if` or `..select` JAM statement.

*/
  parse var g.1 . sParms .
  if sParms <> ''
  then call queueHelpForVerb 'End'
  else do
    parse value popStack() with sClause sState
    select
      when sClause = 'IF'     then do
        parse var sState g.0EMIT .
      end
      when sClause = 'SELECT' then do
        parse var sState g.0EMIT g.0WHEN g.0SELVALUE '/'
      end
      otherwise do
        say 'JAM005E END found without preceding IF or SELECT'
      end
    end
/*  say left('',2*g.0T),
    'END : emit='g.0EMIT '('sClause') sel='g.0SELVALUE 'when='g.0WHEN */
  end
return

doGet:
/*
### ..GET       fromdsn fromsys [todsn [locsiteoptions...]]

  This generates a job step that uses FTP to get a dataset called
  "fromdsn" from system "fromsys" and optionally store
  it locally in dataset "todsn". Use "locsiteoptions" to
  specify options for the FTP client's LOCSITE subcommand (for example
  RECFM=VB etc).

*/
  call addJCLCommentBlock 'Get dataset using FTP'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Get'
  else do
    parse var g.1 . sSource sSystem sTarget sLocsiteOptions
    call queueFTPStep
    queue getHost(sSystem)
    call queueFTP 'locsite ispfstats' sLocsiteOptions
    if binary = 1
    then queue 'binary'
    else queue 'ebcdic'
    sLastSystem = sSystem
    do i = 1 to g.0
      parse var g.i . sSource sSystem sTarget .
      if sSystem <> sLastSystem
      then do
        queue 'close'
        queue 'open' getHost(sSystem)
        call queueFTP 'locsite ispfstats' sLocsiteOptions
        if binary = 1
        then queue 'binary'
        else queue 'ebcdic'
      end
      if sTarget = ''
      then call queueFTP "get '"sSource"' (replace"
      else call queueFTP "get '"sSource"' '"sTarget"' (replace"
      sLastSystem = sSystem
    end
    queue 'quit'
    queue '/*'
  end
return

doGetOut:
/*
### ..GETOUT    fromdsn fromsys todsn

  This generates a job step that uses FTP to submit dataset called
  "fromdsn" on system "fromsys" and store the resulting
  output locally in dataset "todsn".

*/
  call addJCLCommentBlock 'Get output from job using FTP'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'GetOut'
  else do
    parse var g.1 . sSource sSystem sTarget .
    call queueFTPStep
    queue getHost(sSystem)
    call queueSite
    queue 'ebcdic'
    sLastSystem = sSystem
    do i = 1 to g.0
      parse var g.i . sSource sSystem sTarget .
      if sSystem <> sLastSystem
      then do
        queue 'close'
        queue 'open' getHost(sSystem)
        call queueSite
        queue 'ebcdic'
      end
      if sTarget = ''
      then call queueFTP "get '"sSource"' //DD:"sSystem
      else call queueFTP "get '"sSource"' '"sTarget"' (replace"
      sLastSystem = sSystem
    end
    queue 'quit'
    queue '/*'
    drop z.
    z. = ''
    do i = 1 to g.0
      parse var g.i . sSource sSystem sTarget .
      if sTarget = '' & z.sSystem = ''
      then do
        z.sSystem = 1
        queue '//'left(sSystem,9) 'DD SYSOUT=*'
      end
    end
  end
return

queueSite:
  queue 'site filetype=jes'
  queue 'locsite recfm=vba lrecl=255 blksize=27990'
return

doHelp:
/*
### ..HELP

The `..help` JAM statement displays help information about all of the
JAM verbs. If you want to display help information about a particular
JAM verb, specify `?` as the only parameter. For example, to display
help for the `..job` verb, specify:

    ..job ?


#### What JAM does

The JAM processor copies an input text file to an output text file and for
each line of input does the following:

*  JAM performs REXX expression substitutions.

   Expressions are surrounded by square brackets, for example `[expression]`,
   and can be anything that you could specify on the right hand side of
   a REXX assignment statement. For example `[2+2]`, or `[step+1]`, or `[date() time()]`.
   There are several pre-defined system variable names that
   you can use in expressions but you can easily define and use your
   own variables. For example:

       The current time is [time()]
       ..set mytime = time()
       The current time is now [mytime]

*  JAM performs conditional input record selection using if/else/end and
   select/when/otherwise/end JAM statements (with unlimited nesting of
   such statements). For example:

       ..if [rc = 0]
       The return code is zero
       ..else
       The return code is not zero
       ..end

*  JAM can read tabular data from files so that they can be accessed
   from REXX variables. See the [`..table`](#..TABLE-dsn) and
   [`..map`](#..MAP-dsn) JAM verb documention below for more information.

*  JAM generates complex JCL steps from simple JAM statements. For example,

       ..copy my.pds(mem1,mem2,mem3) your.pds

   generates an IEBCOPY step:

       //&ast;
       //&ast;-------------------------------------------------------------------*
       //&ast; Copy members                                                      *
       //&ast;-------------------------------------------------------------------*
       //&ast;
       //STEP1   EXEC PGM=IEBCOPY
       //SYSPRINT  DD SYSOUT=*
       //DD1       DD DISP=SHR,DSN=MY.PDS
       //DD2       DD DISP=SHR,DSN=YOUR.PDS
       //SYSIN     DD *
         COPY INDD=((DD1,R)),OUTDD=DD2
         SELECT MEMBER=(MEM1)
         SELECT MEMBER=(MEM2)
         SELECT MEMBER=(MEM3)
       /&ast;

*  JAM supports a reasonably powerful macro syntax that allows you generate
   dynamic output. For example:

       ..macro define mydd # hlq
       //DD[#] DD DISP=SHR,DSN=[hlq].MY.FILE[#]
       ..macro end
       ..for 1 to 5 macro mydd SYS1

    generates a series of DD statements:

       //DD1 DD DISP=SHR,DSN=SYS1.MY.FILE1
       //DD2 DD DISP=SHR,DSN=SYS1.MY.FILE2
       //DD3 DD DISP=SHR,DSN=SYS1.MY.FILE3
       //DD4 DD DISP=SHR,DSN=SYS1.MY.FILE4
       //DD5 DD DISP=SHR,DSN=SYS1.MY.FILE5


#### The JAM Syntax
   * A JAM statement is identified by two dots (`..`) in columns 1 and 2 and has
   the following syntax:

      ..verb parameters

   There can be spaces before and after the verb and the case of the verb
   is not important, so both of the following have the same result:

      ..delete my.dataset
      ..   Delete MY.DATASET

   Whether or not the parameters are converted to uppercase depends
   on the JAM verb. For example, the `..uss` JAM verb does not convert its
   parameters to uppercase because Unix System Services commands are case-sensitive.

   * You can "chain" JAM statements together so that a single job step can
   perform multiple operations. Statements are chained if they terminate
   with a comma (`,`).

   For example:

       ..copy my.pds(mem1,mem2,mem3) your.pds

   is equivalent to:

       ..copy my.pds(mem1) your.pds,
       ..copy my.pds(mem2) your.pds,
       ..copy my.pds(mem3) your.pds

   * You can continue JAM statements over multiple lines. Statements are
   continued if they terminate with a hypen (`-`) and the next line
   begins with `..`, for example:

       ..copy my.pds(mem1) -
       ..     your.pds

# List of JAM verbs


  JAM is self-documenting. You can obtain help for individual JAM verbs by specifying `?` as the verb's
  operand and then running the JAM processor. For example:

      ..job ?

*/

  call queueHelpForVerb 'Help'
  /* Generate Table of Contents in GitHub markdown syntax */
  do i = 1 to words(g.0VERBS)
    sVerb = word(g.0VERBS,i)
    parse value sourceline(g.0HELPBEG.sVerb) with '..'sSyntax 0 . sVerb .
    /*                e.g. sourceline = '### ..ARGS      var [var...]' */
    /*                        sSyntax =       'ARGS      var [var...]' */
    /*                          sVerb =     '..ARGS'                   */
    sLink = translate(sSyntax,'ff'x,' ')   /* 'ARGS\\\\\\var\[var...]' */
    sLink = translate(sLink,'','[].,=+|')  /* 'ARGS\\\\\\var\ var    ' */
    sLink = space(sLink,0)                 /* 'ARGS\\\\\\var\var'      */
    sLink = translate(sLink,'-','ff'x)     /* 'ARGS------var-var'      */
    sLink = toLower(sLink)                 /* 'args------var-var'      */
    queue '- ['sVerb'](#'sLink')' /* - [..ARGS](#args------var-var)  */
  end
  /* Generate help text for each verb */
  do i = 1 to words(g.0VERBS)
    sVerb = word(g.0VERBS,i)
    if sVerb <> 'HELP'
    then call queueHelpForVerb sVerb
  end

MoreHelp:
/*

# USAGE

# Defining JAM variables

You can define your own variables by using the `..set` JAM statement.
For example, you can set the system on which your
job is to be run by specifying:

    ..set alias = 'SY1'

You can create as many REXX variables as you want and you can assign any
valid REXX expression to them. For example, you can create a variable
that contains the current date and time by coding:

    ..set timestamp = date() time()
    ..set begin = timestamp

# Using JAM variables

JAM will substitute the value of any REXX expression (which includes just REXX variable names)
that you have enclosed in square brackets.
For example:

    ..set timestamp = date() time()
    ..job
    ..* This job was created on [timestamp] by [userid()]
    ..br14

*/

  call queueHelpFromLabel 'MoreHelp:'

HelpVars:
/*
# Pre-defined variables

  The following variables are re-evaluated whenever you use a
  `..job` or `..runon` JAM verb, or whenever you assign a
  system alias to the "alias" variable using `..set alias = youralias`:

  | Variable | Description                         |
  | -------- | ----------------------------------- |
  | alias    | System alias                        |
  | cat      | Catalog name                        |
  | host     | Host name                           |
  | jc       | Job class                           |
  | jesnode  | JES2 node number                    |
  | jobname  | Job name                            |
  | mc       | Message class                       |
  | sysclone | Sysclone system variable            |
  | sysname  | Sysname (the SMF id of this system) |
  | sysplex  | Sysplex name                        |

  Other pre-defined variables that are independent of the "alias"
  variable are:

  | Variable | Description                           | Example     |
  | -------- | ------------------------------------  | ----------- |
  | prog     | Programmer Name from job card         | Donald Duck |
  | userid   | Userid                                | U12345      |
  | user     | Userid (with a shorter variable name) | U12345      |
  | u        | Userid (even shorter variable name)   | U12345      |

# Built-in functions

  | Function                   | Description                                     | Example | Result
  | -------------------------- | ----------------------------------------------  | ------- | ---
  | getHost(addr_or_name)      | Resolve host name from IP address or host name  | `..say [getHost('localhost')]` | 127.0.0.1
  | inRange(n,lo,hi)           | Return 1 if lo <= n <= hi                       | `..set month = 2`<br/>`..if [inRange(month,1,12)]` | 1
  | inSet(element,list)        | Return 1 if element is in a list of words       | `..set lpar = 'PRD1'`<br/>`..if [inset(lpar,'TST1 PRD1')]`<br/>ok<br/>`..end` | ok
  | intersect(set1,set2)       | Return elements common to both set1 and set2    | `..set rich = 'Gates Musk Cheesecake'`<br/>`..set famous = 'Einstein Musk Gates'`</br>`..say Rich and famous: [intersect(rich,famous)]` | Rich and famous: Gates Musk
  | isASCII(text)              | Return 1 if text is ASCII                       | `..say [isASCII('6162'x)]` | 1
  | isDatasetName(name)        | Return 1 if name is a valid dataset name        | `..say [isDatasetName('SYS1.PARMLIB')]` | 1
  | isDDName(name)             | Return 1 if name is a valid DD name             | `..say [isDDName('SYSIN')]` | 1
  | isHex(hex)                 | Return 1 if hex is valid hex                    | `..say [isHex('0C1')]` | 1
  | isIPAddr(addr)             | Return 1 if addr is a valid IP address          | `..say [isIPAddr(127.0.0.1)]` | 1
  | isMemberName(name)         | Return 1 if name is a valid member name         | `..say [isMemberName('$$README')]` | 1
  | isNum(n)                   | Return 1 if n is a whole number                 | `..say [isNum(3.14159265358979)]` | 0
  | isText(text)               | Return 1 if text is EBCDIC                      | `..say [isText('The cat sat on the mat')]` | 1
  | range(from,to,space,fill)  | Return a range of values between from and to    | `..say [range(1,3,2,'-')]`<br/>`..say [range(1,3)]` | 1--2--3<br/>1 2 3
  | replace(from,to,text)      | Return text after changing all occurrences of "from" to "to" | `..say [replace('ur',"you're",'ur good']` | you're good
  | sortStem(stem,ascending)   | Return "sorted." stem that indexes the elements of "stem." in ascending (1) or descending (0) order | `..set count = toArray('charlie bob alice','name.')`<br/>`..set alreadysorted = sortStem('name.')`<br/>`..set first = sorted.1`<br/>`..say First is [name.first] of [count]` | First is alice of 3
  | sortWords(words,ascending) | Return words sorted into ascending (1) or descending (0) order | `..say [sortWords('charlie bob alice')]` | alice bob charlie
  | toArray(text,stem,delim)   | Convert text delimited by "delim" into a REXX "stem."  | `..set count = toArray('charlie bob alice','name.')`<br/>`..say name.1='[name.1]'`<br/>`..say name.2='[name.2]'`<br/>`..say name.3='[name.3]'` | name.1='charlie'<br/>name.2='bob'<br/>name.3='alice'
  | toASCII(text)              | Convert text to ASCII from EBCDIC | `..say [c2x(toASCII('C1C2C3'x))]` | 414243
  | toBlock(text,stem,maxlen)  | Convert text into a REXX "stem." with elements no wider than "maxlen" | `size = toBlock('The quality of mercy is not strained','a.',12)`<br/>`..say Reblocked to [size] lines:`<br/>`..say a.1='[a.1]'`<br/>`..say a.2='[a.2]'`<br/>`..say a.3='[a.3]'` | Reblocked to 3 lines:<br/>a.1='The quality'<br/>a.2='of mercy is'<br/>a.3='not strained'
  | toEBCDIC(text)             | Convert text to EBCDIC from ASCII | `..say [c2x(toEBCDIC('414243'x))]` | C1C2C3
  | toLower(text)              | Convert text to lower case | `..say [toLower('ABC123')]` | abc123
  | toString(stem)             | Convert a REXX "stem." variable to a string | `..say '[toString('a.')]'` | 'The quality of mercy is not strained'
  | toUpper(text)              | Convert text to upper case | `..say toUpper('abc123')` | ABC123
  | union(set1,set2)           | Return the union of set1 and set2 | `..set rich = 'Gates Musk Cheesecake'`<br/>`..set famous = 'Einstein Musk Gates'`</br>`..say Rich or famous: [union(rich,famous)]` | Rich or famous: Gates Musk Cheesecake Einstein

*/
  call queueHelpFromLabel 'HelpVars:'

EvenMoreHelp:
/*

# How to use JAM in ISPF/EDIT

You should, before first use, do some initial set up as follows.

1. Get into ISPF/EDIT
2. Use the KEYS command to assign JAM to PF4.
   Now you can either press PF4 or enter JAM on the command line
   to process the JAM statements in the file you are editing.
3. Use the IMACRO command to set your initial macro to be JAMINIT.
   The JAMINIT macro will automatically run the JAM processor when
   you edit a file if the first line of that file contains
   `..auto [message]`.

# How to use JAM in BATCH

1. Edit the following JCL:

       //JAM      JOB ,,CLASS=A,MSGCLASS=X,NOTIFY=&SYSUID
       //STEP1   EXEC PGM=IKJEFT01,PARM='JAM'
       //SYSEXEC   DD DISP=SHR,DSN=your.exec.lib
       //SYSTSPRT  DD SYSOUT=*
       //IN        DD DATA,DLM=@@
       ..job
       ..br14
       ..* [user] does nothing yet again
       @@
       //OUT       DD SYSOUT=(*,INTRDR)

2. Insert any JCL and/or JAM statements after the IN DD and
   before the @@ delimiter. You do not need the delimiter if the
   IN DD points to a dataset.
*/
  call queueHelpFromLabel 'EvenMoreHelp:'
return

doIf:
/*
### ..IF        cond [action]

  This is part of an if-else-end construct:

    ..if [condition]
      output if condition evaluates to 1
    ..else
      output if condition evaluates to 0
    ..end

  This JAM verb will evaluate the REXX expression specified by "cond".
  The result of the evaluation must be either 1 (true) or 0 (false).

  If 1, execute the "action" (if present) and process all
  subsequent statements until the matching `..end` or
  `..else` JAM statement is found.

  If 0, ignore the "action" and all subsequent statements
  until the matching `..end` or `..else` statement
  is found.

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'If'
  else do
    parse var g.1 . bCondition sAction
    call pushStack 'IF' g.0EMIT bCondition '/'g.1
    g.0EMIT = g.0EMIT & bCondition
    if g.0EMIT
    then do
      if sAction <> ''
      then interpret sAction
    end
  end
return

doFor:
/*
### ..FOR       READ dsn MACRO macroname

  This invokes the macro called "macroname" once for each
  line in the dataset "dsn"

  Example (prints the contents of a member):

      ..macro define show nextline
      ..  say [nextline]
      ..macro end
      ..for read sys1.parmlib(ieasys00) macro show

### ..FOR       str1 str2 ... strn MACRO macroname

  Invokes the macro called "macroname" once for each
  string in the list "str1 str2 ... strn"

  Example (copies a member to several other members):

      ..macro define duplicate suffix .
      ..  copy sys1.parmlib(ieasys00) =(ieasys0[suffix])
      ..macro end
      ..for A B C macro duplicate

### ..FOR       x TO y [BY z] MACRO macroname

  Invokes the macro called "macroname" once for each
  number in the range "x" to "y" (by "z" steps)

  Example (lists the odd numbers between 1 and 10):

      ..macro define show #
      ..  say [#]
      ..macro end
      ..for 1 to 10 by 2 macro show


### ..FOR       n MACRO macroname

  Invokes the macro called "macroname" once for each
  number in the range 1 to "n"


  Example (creates some DD statements):

      ..macro define dd #
      //DD[#] DD DISP=SHR,DSN=MY.DATASET.SEQ[#]
      ..macro end
      ..for 5 macro dd

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'For'
  else do i = 1 to g.0
    parse upper var g.i . sOperands 'MACRO' sMac
    if sMac = ''
    then call queueHelpForVerb 'For'
    else do
      sOperands = strip(sOperands)
      parse var sOperands sOperand1 sOperand2 .
      sMac      = strip(sMac)
      select
        when sOperand1 = 'READ' then do /* for READ dsn MACRO mac */
          parse var sOperands . sDSN .
          nLines = readFile(getFileName(sDSN))
          do j = 1 to line.0
            call appendMacroOverride '..macro' sMac toStr(line.j) '[]'
          end
        end
        when sOperand1 = 'EXEC' then do /* for EXEC cmd MACRO mac */
          parse var sOperands . sCmd
          call quietly sCmd
          do j = 1 to o.0
            call appendMacroOverride '..macro' sMac toStr(o.j)'[]'
          end
        end
        when sOperand2 = 'TO'   then do /* for x TO y BY z MACRO mac */
          parse var sOperands nFrom 'TO' nTo 'BY' nBy .
          if \datatype(nFrom,'WHOLE') then nFrom = 1
          if \datatype(nTo,  'WHOLE') then nTo   = nFrom
          if \datatype(nBy,  'WHOLE') then nBy   = 1
          do n = nFrom to nTo by nBy
            call appendMacroOverride '..macro' sMac n
          end
        end
        when datatype(sOperands,'WHOLE') then do /* for n MACRO mac */
          parse var sOperands nCount .
          do n = 1 to nCount
            call appendMacroOverride '..macro' sMac n
          end
        end
        otherwise do                            /* for str1 str2... MACRO mac */
          do n = 1 to words(sOperands)
            call appendMacroOverride '..macro' sMac toStr(word(sOperands,n))
          end
        end
      end
    end
  end
return

appendMacroOverride: procedure expose g.
  parse arg sLine
  /*
    If we are in a MACRO...
      Then add the line to the list of lines that will override the
      current FOR statement being processed
    else we simply append the line as if it were INCLUDEd from a file

    Variables involved:

    g.0MACRUN  = 0|1    Running a macro? 0=No, 1=Yes
    g.0MACDEF  = 0|1    Defining a macro? 0=No, 1=Yes
    g.0MACRO   = xxx    Name of current MACRO being run
    g.0MACLINE = n      Line number within current macro
    g.0MAC.xxx.0 = x    Number of lines in MACRO xxx
    g.0MAC.xxx.n = ...  Content of line n of MACRO xxx

    g.0MAC.xxx.n.0      Number of override lines for macro line n
    g.0MAC.xxx.n.#0     Current override line number for macro line n
    g.0MAC.xxx.n.m      Line m of override of macro line n
                        All overrides are removed when the MACRO finishes
                        If an override exists for line m, then the next
                        line of the override lines is returned instead
                        of line m itself.
  */
  if g.0MACDEF then return /* nothing to do when we are defining a macro */

  sLine = normaliseSquareBrackets(sLine)
  if g.0MACRUN             /* if a macro is running */
  then do                  /* override the current macro line */
    sMac = g.0MACRO        /* get current macro name, or blank */
    n = g.0MACLINE
    if g.0MAC.sMac.n.0 = '' then g.0MAC.sMac.n.0 = 0
    o = g.0MAC.sMac.n.0 + 1
    g.0MAC.sMac.n.o = sLine
    g.0MAC.sMac.n.0 = o
    g.0MAC.sMac.n.0# = 0   /* reset current override line number */
/*  say '-->'sMac'.'n'.'o '=<'g.0MAC.sMac.n.o'>' */
  end
  else do                  /* append the text as an INCLUDEd line */
    i.0 = 1
    i.1 = sLine
    call appendIncludeLines
  end
return

readFile: procedure expose g. line.
  select
    when g.0ZOS then do
      parse upper arg sDSN
      sDSN = strip(sDSN,'BOTH',"'")
      sFileStatus = sysdsn("'"sDSN"'")
      if sFileStatus = 'OK'
      then do
        call quietly "ALLOCATE FILE(JAM) DSNAME('"sDSN"')",
                    'INPUT SHR REUSE'
        'EXECIO * DISKR JAM (FINIS STEM line.'
        call quietly 'FREE FILE(JAM)'
      end
      else do
        say '     JAM009W Could not read dataset:' sDSN '-' sFileStatus
        line.0 = 0
      end
    end
    otherwise do
      parse arg sFileName
      line.0 = 0
      hFile = openFile(sFileName)
      if g.0RC = 0
      then do i = 1 while g.0RC = 0
        sLine = getLine(hFile)
        if g.0RC = 0
        then do
          line.0 = i
          line.i = sLine
        end
      end
      else do
        say '     JAM009W Could not read file:' sFileName
      end
    end
  end
return line.0

doInclude:
/*
### ..INCLUDE   dsn
  This includes the contents of dataset "dsn"
  at this point in the JAM input file.

  Note: No attempt is made to detect recursive INCLUDEs.

### ..INCLUDE   dsn(member)
  This includes the contents of partitioned dataset "dsn" member "member"
  at this point in the JAM input file.

  Note: No attempt is made to detect recursive INCLUDEs.

### ..INCLUDE   (member)

  Includes the contents of "member" from the pds currently being edited
  at this point in the JAM input file.

  Note: No attempt is made to detect recursive INCLUDEs.

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Include'
  else do
    do i = 1 to g.0
      parse var g.i . sDSN .
      call getLinesFromFile getFileName(sDSN)
    end
  end
return

getFileName: procedure expose g. dataset
  parse arg sDSN
  if g.0ZOS
  then do
    /* Transform (member) into dataset(member) if necessary */
    if pos('(',sDSN) > 0
    then do
      parse var sDSN sDSN'('sMem')' .
      if sDSN = ''
      then sDSN = dataset'('sMem')'
      else sDSN = sDSN'('sMem')'
    end
  end
return sDSN

getLinesFromFile: procedure expose g.
  parse upper arg sDSN
  sFileStatus = sysdsn("'"sDSN"'")
  if sFileStatus = 'OK'
  then do
    g.0INC = g.0INC + 1
    sDD = 'INC'g.0INC  /* Build unique DD name for each included file */
    call quietly "ALLOCATE FILE("sDD") DSNAME('"sDSN"') INPUT SHR REUSE"
    'EXECIO * DISKR' sDD '(FINIS STEM i.'
    call quietly 'FREE FILE('sDD')'
    call insertIncludeLines
  end
  else do
    say 'JAM002W Could not INCLUDE dataset:' sDSN '-' sFileStatus
  end
return

insertIncludeLines: procedure expose g. i.
  if i.0 > 0 /* If there are any lines to inject */
  then do
    /*
           BEFORE                AFTER              LINES TO BE INJECTED
     -------------------   -------------------      --------------------
     g.0INCLO -> line w    g.0INCLO -> line A | <-- i.1 line A
                 line x                line B |     i.2 line B
                 line y                line C |     i.3 line C
     g.0INCHI -> line z                line w
                                       line x
                                       line y
                           g.0INCHI -> line z
    */
    /* Shift existing lines up to make room for lines to be included */
    k        = g.0INCHI         /* Initial "shift from" index */
    g.0INCHI = g.0INCHI + i.0   /* Initial "shift to" index   */
    do j = g.0INCHI to g.0INCLO + i.0 by -1
      g.0INC.j = g.0INC.k
      k = k - 1
    end
    /* Now inject the lines to be included */
    k = g.0INCLO
    do j = 1 to i.0
      g.0INC.k = normaliseSquareBrackets(i.j)
      k = k + 1
    end
  end
return

appendIncludeLines: procedure expose g. i.
  if i.0 > 0 /* If there are any lines to append */
  then do
    /*
           BEFORE                AFTER              LINES TO BE APPENDED
     -------------------   -------------------      --------------------
     g.0INCLO -> line w    g.0INCLO -> line w
                 line x                line x
                 line y                line y
     g.0INCHI -> line z                line z
                                       line A | <-- i.1 line A
                                       line B |     i.2 line B
                           g.0INCHI -> line C |     i.3 line C
    */
    /* Now append the lines to be included */
    k = g.0INCHI
    do j = 1 to i.0
      k = k + 1
      g.0INC.k = normaliseSquareBrackets(i.j)
    end
    g.0INCHI = k
  end
return

doJCL:
/* TODO: Get this working properly
### ..JCL       [stmt]

  This reformats the specified JCL statement with continuations
  if necessary.

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then do
    call queueHelpForVerb 'JCL'
  end
  else do /* TODO: implement the JCL logic */
    do i = 1 to g.0
      say 'doJCL' i 'of' g.0 '<<'g.i'>>'
      parse var g.i . __jcl
      call queueJCL __jcl
    end
  end
return

queueJCL:
  parse arg __stmt 1 __id +2 3 __char3 +1
  say 'id<'__id'> char3<'__char3'> stmt<'__stmt'>'
  __name = ''
  __oper = ''
  __parms = ''
  if __id = '//' & __char3 \= '*'
  then do
    if __char3 = ' '            /* if JCL name field is omitted */
    then parse var __stmt 3        __oper __parms
    else parse var __stmt 3 __name __oper __parms
    if pos('=',__oper) > 0
    then do
      __parms = __oper __parms
      __oper = ''
    end
    __parms = strip(__parms)
    say '  name<'__name'> oper<'__oper'> parms<'__parms'>'
    if pos(' ',__parms) > 0
    then __parms = hardBlanks(__parms)
    call getParmMap __parms
    __nameoper = justify(__name __oper,max(12,length(__name __oper)))
    queue '//'__nameoper g.0PARM.1
    do __i = 2 to g.0PARM.0
      queue '//             'g.0PARM.__i
    end
  end
  else do
    queue __stmt
  end
return

/* Replace blanks in quoted strings with 'fa'x so it is easier
   to parse later. For example:
             <---parameters----><----comments------>
       this: ABC,('D E F','GH'),     'QUOTED COMMENT'
    becomes: ABC,('D~E~F','GH'),     'QUOTED COMMENT'
    Where '~' is a "hard" blank ('fa'x)
*/
hardBlanks: procedure expose g.
  parse arg sLine
  sLine = strip(sLine,'LEADING')
  sOut = ''
  bInString = 0
  do i = 1 to length(sLine) until c = ' ' & \bInString
    c = substr(sLine,i,1)
    select
      when c = "'" & bInString then bInString = 0
      when c = "'"             then bInString = 1
      when c = ' ' & bInString then c = 'fa'x
      otherwise nop
    end
    sOut = sOut || c1
  end
  if i <= length(sLine)
  then do
    if bInString /* make trailing blanks 'hard' blanks */
    then sOut = sOut || translate(substr(sLine,i),'fa'x,' ')
    else sOut = sOut ||           substr(sLine,i)
  end
return strip(sOut)

softBlanks: procedure
  parse arg sLine
return translate(sLine,' ','fa'x)

/*
  JCL parameters consist of positional values followed by key=value
  pairs. Values can be bracketed or quoted. For example:
    <---------------parms-------------->
    A,(B,C),'D E,F',G=H,I=(J,K),L='M,N O'
    <--positional--><------keywords----->
  This routine parses parameters into stem variables as follows:
  g.0PARM.0 = number of parameters
  g.0PARM.n = parameter n
  ...where n = 1 to the number of parameters.
  The above example will be parsed as follows:
  g.0PARM.0 = 4
  g.0PARM.1 = "A,(B,C),'D E,F'"    <-- Positional values
  g.0PARM.2 = 'H'                  <-- Value of key G
  g.0PARM.3 = '(J,K)'              <-- Value of key I
  g.0PARM.4 = "'M,N O'"            <-- Value of key L
*/
getParmMap: procedure expose g.
  parse arg sParms
  sParms = strip(sParms)
  nParm = 0
  nComma  = pos(',',sParms)
  nEquals = pos('=',sParms)
  /* Process the positional operands */
  select
    when nComma = 0 & nEquals = 0 & sParms <> '' then do
      nParm = nParm + 1
      g.0PARM.nParm = softBlanks(sParms)
      sParms = ''
    end
    when nComma > 0 & nComma < nEquals then do
      nPos = lastpos(',',sParms,nEquals)
      sPositionals = left(sParms,nPos)
      nParm = nParm + 1
      g.0PARM.nParm = softBlanks(sPositionals)
      sParms = substr(sParms,nPos+1)
    end
    otherwise nop
  end
  /* Process the keyword=value operands */
  do while sParms <> '' & pos('=',sParms) > 0
    parse var sParms sKey'='sValue
    select
      when left(sValue,1) = '(' then do /* K=(...) */
        nValue = getInBracketsLength(sValue) + 1
        parse var sValue sValue +(nValue) sParms
      end
      when left(sValue,1) = "'" then do /* K='...' */
        nValue = getInQuotesLength(sValue) + 1
        parse var sValue sValue +(nValue) sParms
      end
      otherwise do /* K=V         */
                   /* K=S=(...)   */
                   /* K=S='...'   */
                   /* K=S=X       */
        sSymbol = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789@#$&.'
        nSymbol = verify(sValue' ',sSymbol,'NOMATCH')
        if nSymbol > 0
        then do
          c = substr(sValue,nSymbol,1)
          if c = '='
          then do /* K=S=...*/
            parse var sValue sSubKey'='sParms
            select
              when left(sParms,1) = '(' then do /* K=S=(...) */
                nSubValue = getInBracketsLength(sParms) + 1
                parse var sParms sSubValue +(nSubValue) sParms
              end
              when left(sParms,1) = "'" then do /* K=S='...' */
                nSubValue = getInQuotesLength(sParms) + 1
                parse var sParms sSubValue +(nSubValue) sParms
              end
              otherwise do                      /* K=S=... */
                nSymbol = verify(sParms,sSymbol,'NOMATCH')
                parse var sParms sSubValue +(nSymbol) sParms
              end
            end
            sValue = sSubKey'='sSubValue
          end
          else do /* K=V,... */
            parse var sValue sValue +(nSymbol) sParms
          end
        end
        else do /* K=V,... */
          parse var sValue sValue +(nSymbol) sParms
        end
      end
    end
    nParm = nParm + 1
    g.0PARM.nParm = sKey'='softBlanks(sValue)
  end
  if sParms <> '' /* residual comment */
  then do
    nParm = nParm + 1
    g.0PARM.nParm = softBlanks(strip(sParms))
  end
  g.0PARM.0 = nParm
return

/* (abc) --> 5 */
getInBracketsLength: procedure
  parse arg sValue
  nLvl = 0
  do i = 1 to length(sValue) until nLvl = 0
    c = substr(sValue,i,1)
    select
      when c = '(' then nLvl = nLvl + 1
      when c = ')' then nLvl = nLvl - 1
      otherwise nop
    end
  end
return i

/* 'abc' --> 5 */
getInQuotesLength: procedure
  parse arg sValue
  bEndOfString = 0
  do i = 2 to length(sValue) until bEndOfString
    if substr(sValue,i,2) = "''"
    then i = i + 1 /* Skip over '' */
    else bEndOfString = substr(sValue,i,1) = "'"
  end
return i

doJob:
/*
### ..JOB       [system[/jobnamesuffix] [description]]

  This generates a JOB card, an XEQ card and a JOBPARM card
  for the system specified by "system", or else for the
  system specified by the current value of the "alias" variable.

  If "system" is not a known system alias then it will
  become the first word of the "description"

  If "jobnamesuffix" is omitted, it defaults to a single
  character which is the last character of the system alias.

  The first 20 characters of the "description" is put in
  the programmer name field of the JOB card.

  If preceded by "..option hold" then TYPRUN=HOLD is appended
  to the job card.

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then do
    call queueHelpForVerb 'Job'
  end
  else do
    parse upper var g.1 . sAlias sDesc '/*' /* Ignore trailing comments   */
    parse var sAlias sAlias'/'sJobSuffix
    call setAlias sAlias       /* Use system alias specified on JAM statement */
    if jesnode.sAlias = '',   /* If a valid system alias was not specified */
     & sJobSuffix = ''     /* ...and no job suffix was specified        */
    then parse upper var g.1 . sDesc   /* Then parameters are all desc  */
    sDesc = strip(left(space(sDesc),20))
    if hold = 1
    then call queueJob 'TYPRUN=HOLD',sJobSuffix
    else call queueJob '',sJobSuffix
    call queueXEQ
  end
return


getJobName: procedure
  parse upper arg sJobSuffix
return toUpper(left(userid()sJobSuffix,8))

queueJob:
  /* TODO: Fix this so arbitrary job parameters can be continued */
  parse arg sJobOptions,sJobSuffix
  if g.0DLM = 1
  then do
    queue '@@'
    g.0DLM = 0 /* No outstanding DLM=@@ termination */
  end
  sProg = strip(sDesc)
  if sProg <> '' then sProg = "'"sProg"'"
  if sJobSuffix <> '' then jobname = getJobName(sJobSuffix)
  sJobCard = '//'left(jobname,8) 'JOB ,'sProg',' ||,
             'CLASS='jc',MSGCLASS='mc',NOTIFY=&SYSUID'
  if length(sJobCard) < 72 & sJobOptions = ''
  then queue sJobCard
  else do
    if sJobOptions = ''
    then queue '//'left(jobname,8) 'JOB ,'sProg','
    else queue '//'left(jobname,8) 'JOB ,'sProg','sJobOptions','
    queue left('//',15)'CLASS='jc',MSGCLASS='mc',NOTIFY=&SYSUID'
  end
return

doListcat:
/*
### ..LISTCAT   dsn [catalog] [options...]

  This generates a job step that will invoke IDCAMS to LIST dataset "dsn" in the
  specified catalog, or else in the standard catalog
  search order. The following pre-defined catalog
  variables can be used:

  | Variable | Description                               |
  | -------- | ----------------------------------------- |
  | cat      | The master catalog for the alias system.  |

*/
  call addJCLCommentBlock 'Listcat dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Listcat'
  else do
    queue getStep('PGM=IDCAMS')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD *'
    queue '  LISTCAT ENTRIES ( -'
    parse upper var g.1 . sDataset sPrevOptions
    do i = 1 to g.0
      parse upper var g.i . sDataset sOptions
      if sOptions = sPrevOptions
      then do
        queue '                   'left(sDataset,44) '-'
      end
      else do
        call queueIDCAMS sPrevOptions
        sPrevOptions = sOptions
        queue '  LISTCAT ENTRIES ( -'
        queue '                   'left(sDataset,44) '-'
      end
    end
    call queueIDCAMS sPrevOptions
    if setmaxcc0 = 1
    then queue '  SET MAXCC = 0'
    queue '/*'
  end
return

queueIDCAMS: procedure
  parse arg sOptions
  if sOptions = ''
  then do
    queue '                  )'
  end
  else do
    queue '                  ) -'
    sOption1 = word(sOptions,1)
    if pos('.',sOption1) > 0 & pos('(',sOption1) = 0
    then do
      parse var sOptions sCatalog sOptions
      sOptions = 'CATALOG('sCatalog')' sOptions
    end
    nOptions = words(sOptions)
    do j = 1 to nOptions-1
      sOption = word(sOptions,j)
      if pos('(',sOption) > 0
      then do
        parse var sOption sKey'('sValue')'
        sOption  = left(sKey,max(length(sKey),7)) '('strip(sValue)')'
      end
      queue '          'sOption '-'
    end
    queue '          'word(sOptions,nOptions)
  end
return

doListVTOC:
/*
### ..LISTVTOC  dsn volser

   This generates a job step that will list datasets specified by
   "dsn" in the VTOC of the volume specified by "volser".
   The dataset name can be generic (e.g. SYS1.XXX*).

*/
  call addJCLCommentBlock 'List VTOC'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'ListVTOC'
  else do
    drop z.
    z. = 0
    nDD = 0
    queue getStep('PGM=IEHLIST')
    queue '//SYSPRINT  DD SYSOUT=*'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      if \z.sVolser
      then do
        nDD = nDD + 1
        queue '//'left('DD'nDD,9) 'DD DISP=SHR,UNIT=3390,VOL=SER='sVolser
        z.sVolser = 1
      end
    end
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      call queueIEH 'LISTVTOC VOL=3390='sVolser',DSNAME='sDataset
    end
    queue '/*'
  end
return

doMacro:
/*
### ..MACRO     DEFINE macroname [parameters...]

  This defines a named group of lines, bounded by `..macro define`
  and `..macro end` statements, that can be subsequently
  included by invoking the `..macro macroname` statement.

  The optional "parameters" field is typically a list of
  parameter names but can be any valid operand of the
  REXX "parse value" statement.

### ..MACRO     EXIT
  This exits from the macro at run time, otherwise the macro terminates at
  the `..macro end` JAM statement.

### ..MACRO     END

  This terminates a named group of lines (bounded by `..macro define`
  and `..macro end` statements) that can be subsequently
  included by invoking the `..macro macroname` statement.

### ..MACRO     macroname [arguments]

  This invokes a macro called "macroname" that has been previously
  defined by a `..macro define` statement and terminated by
  a `..macro end` statement.

  The optional "arguments" field is typically a list of
  values to be assigned to the parameter names.

  For example:

      ...
      ... First define a macro expecting up to three parameters:
      ...
      ..macro define mymacro p1 p2 p3
      Job name is [p1]
      Step name is [p2]
      ..  if [p3 <> '']
        The date is [p3]
      ..  end
      ..macro end
      ...
      ... Now invoke it twice with different arguments:
      ...
      ..macro mymacro JOB1 STEP1 [date()]
      ..macro mymacro JOB2 STEP2

  which could result in:

      Job name is JOB1
      Step name is STEP1
        The date is 25 Feb 2020
      Job name is JOB2
      Step name is STEP2

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'Macro'
  else do
    parse upper var g.1 . sMacroVerb sMacroName sMacroParseArgs
    select
      when sMacroVerb = 'DEFINE' then do
        if sMacroName = '' then sMacroName = '_'
        g.0MAC.sMacroName.0 = 0 /* number of lines in this macro */
        g.0MAC.sMacroName.0ARGS = sMacroParseArgs
        g.0MACRO = sMacroName
        g.0MACDEF = 1 /* we are defining the macro */
      end
      when sMacroVerb = 'EXIT' then do /* return from macro */
        sMacroName = g.0MACRO
        g.0MACLINE = g.0MAC.sMacroName.0 /* go to end of macro */
        g.0MACRUN.sMacroName = 0 /* not running this macro now */
        call removeOverrides sMacroName
      end
      when sMacroVerb = 'END'  then do /* orphan MACRO END */
        say 'JAM008E No preceding MACRO DEFINE'
      end
      otherwise do /* invoke the macro */
        call pushStack 'MACRO' g.0EMIT g.0MACLINE g.0MACRO
        parse var g.1 . sMacroName sMacroParms
        sMacroName = toUpper(sMacroName)
        if g.0MACRUN.sMacroName = 1
        then do
          say 'JAM007E Recursion detected. Macro' sMacroName 'already running'
          call popStack   /* discard saved state */
        end
        else do
          g.0MACRUN.sMacroName = 1
          if sMacroName = '' then sMacroName = '_'
          if isNum(g.0MAC.sMacroName.0) /* size of macro in lines */
          then do
            g.0MACRUN = 1 /* retrieve next line(s) from the macro */
            interpret "parse value '"sMacroParms"' with" g.0MAC.sMacroName.0ARGS
            g.0MACRO = sMacroName    /* current macro name */
            g.0MACLINE = 0           /* current macro line */
          end
          else do
            say "JAM003E Macro '"sMacroName"' not defined"
            call popStack   /* discard saved state */
          end
        end
      end
    end
  end
return

removeOverrides: procedure expose g.
  parse arg sMac .
  do i = 1 to g.0MAC.sMac.0
    if g.0MAC.sMac.i.0 <> ''
    then do
      do j = 0 to g.0MAC.sMac.i.0
        g.0MAC.sMac.i.j = ''
/*      say '---' sMac i j 'override removed' */
      end
      g.0MAC.sMac.i.#0 = 0 /* reset current overline line number too */
    end
  end
return

doMap:
/*
### ..MAP       dsn

  This maps tabular data in dataset "dsn" (containing column headings) to REXX variables
  that are indexed by the value of the first column.

  For "dsn" you can specify either:
  - A fully qualified unquoted dataset name: dsn
  - A member in a partitioned dataset:       dsn(member)
  - A member in the dataset being edited:    (member)

  The column data in the file need not be aligned, but if a cell value
  contains spaces then that value must be enclosed in quotation marks
  or apostrophes. A useful convention is to use `.` to represent a
  null cell value, but you could just as easily assign a zero-length value
  by using `''`.

  If the column names are omitted from the `..map` statement then they
  are read from the first line of the dataset. For example:

  Assume member "users" contains tablular data with column headings on row 1:

      uid    phone          email
      ABC    "1234 567 890" user1@example.org
      XYZ    "2222 567 890" user2@example.org
      PQR    "3333 567 890" user3@example.org

  You can access the data using the column names in the file

      ..map (users)
      XYZ's phone is [phone.XYZ] and email is [email.XYZ]

  This outputs:

      XYZ's phone is 2222 567 890 and email is user2@example.org

### ..MAP       dsn column1 [column2 ... columnn]

  Maps the tabular data in a dataset (containing no column headings) to REXX variables.
  The column headings to be used are specified on the `..map` JAM statement.

  Reads the dataset "dsn" and generates REXX variables such that you
  can use the value in the first column of each row to access
  any other column by that value.

  For "dsn" you can specify either:
  - A fully qualified unquoted dataset name: dsn
  - A member in a partitioned dataset:       dsn(member)
  - A member in the dataset being edited:    (member)

  The column data in the file need not be aligned, but if a cell value
  contains spaces then that value must be enclosed in quotation marks
  or apostrophes. A useful convention is to use `.` to represent a
  null cell value, but you could just as easily assign a zero-length value
  by using `''`.

  If the column names are present on the `..map` statement then the
  first line of the dataset is considered to be data, not column names,
  since you are explicitly providing the column names to be used.
  For example:

  Assume member "users" contains tabular data with no column headings:

      ABC "1234 567 890" user1@example.org
      XYZ "2222 567 890" user2@example.org
      PQR "3333 567 890" user3@example.org

  You must now access the data using the column names specified on the
  `..map` statement:

      ..map (users) username telephone emailaddr
      XYZ's phone is [telephone.XYZ] and email is [emailaddr.XYZ]

  This outputs:

      XYZ's phone is 2222 567 890 and email is user2@example.org

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Map'
  else do i = 1 to g.0
    parse       var g.i . sDSN .
    parse upper var g.i . .    sFieldNames
    nRows = readFile(getFileName(sDSN))
    call reformatTableData
    /* Now parse the MAP data into variables */
    parse var sFieldNames sKey sFieldNames
    nFieldNames = words(sFieldNames) /* Number of columns  */
    interpret sKey'.0 =' nOutputRows /* Number of rows     */
    do c = 1 to nFieldNames          /* For each column    */
      sField = word(sFieldNames,c)   /* Get the field name */
      interpret 'drop' sField'.'
      interpret sField". = ''"       /* Default value is null */
    end
    or = 0                           /* Output row number  */
    do ir = nFirstRow to nRows       /* Input row number   */
      or = or + 1
      parse var line.ir sKeyValue sRest
      sKeyValue = toUpper(sKeyValue)
      interpret sKey'.'or "= '"sKeyValue"'"
      do c = 1 to nFieldNames        /* For each field     */
        sField = word(sFieldNames,c) /* Get the field name */
        c1 = left(word(sRest,1),1)   /* Values may be enclosed with quotes */
        select
          when c1 = "'" then parse var sRest "'"sValue"'" sRest
          when c1 = '"' then parse var sRest '"'sValue'"' sRest
          otherwise          parse var sRest    sValue    sRest
        end
        if sField <> '.'             /* If it is not a place holder */
        then do
          if c = nFieldNames & sRest <> '' /* If last field */
          then sValue = sValue strip(sRest) /* Append residual text */
          interpret sField".sKeyValue = '"toStr(sValue)"'"
        end
      end
    end
  end
return

doMount:
/*
### ..MOUNT     dsn path [options...]
### ..MOUNT     path dsn [options...]

  This generates a job step to mount file system "dsn" at mount point "path" using any
  "options" valid on the TSO MOUNT command. The "path"
  must contain a "/" character to distinguish it from the
  "dsn" operand.

*/
  call addJCLCommentBlock 'Mount file system'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Mount'
  else do
    call queueTSOStep
    do i = 1 to g.0
      parse var g.i . sDataset sDir sOptions
      if pos('/',sDataset) > 0
      then do /* user has specified the mount point first */
        sTemp = sDataset
        sDataset = sDir
        sDir = sTemp
      end
      sDataset = translate(sDataset)
      sOptions = translate(sOptions)
      queue "  MOUNT MOUNTPOINT('"sDir"') +"
      queue "        TYPE(ZFS) +"
      if sOptions <> ''
      then do
        queue '        FILESYSTEM('sDataset') +'
        queue '       ' sOptions
      end
      else do
        queue '        FILESYSTEM('sDataset')'
      end
    end
    queue '/*'
  end
return

doOption:
/*
### ..OPTION    [NO]option...
### ..OPTION    PUSH
### ..OPTION    POP

  This sets (or resets) one or more named option flags.

  A REXX variable is created for each specified option and is
  set to 1. If "no" is prefixed to the option then the
  REXX variable is set to 0. The `..option` statement is exactly the
  same as issuing `..set someoption = 1` or `..set someoption = 0`.

  PUSH will save the current value of all known options
       on a stack.

  POP  will restore the set of options that was last
       saved by PUSH.

  You can specify any name for an option but the following options have
  special meaning to the JAM processor:

  | Option   | Action when set
  | -------- | ----------------------------------------------
  | trunc    | Truncate input lines at column 71.
  | blanks   | Honours blank input cards. Use "noblanks" to cause blank input lines to be ignored.
  | comments | Honours JCL comment cards. Use "nocomments" to cause JCL comment input lines to be ignored.
  | debug    | Show JAM statements trace messages.
  | hold     | Append TYPRUN=HOLD to job cards that are generated by subsequent `..job` or `..runon` statements.
  | quiet    | Do not generate comments describing the JCL being generated.
  | verbose  | Copy input JAM statements to output.
  | useftp   | Use FTP to submit jobs instead of NJE even when the source and target systems are in the same NJE network.

  Any flags set in this way can be used in subsequent `..if` statements. For example:

      ..option debug nohold myflag
      ..if [debug]
      ..  say The value of hold is [hold]
      ..  say The value of myflag is [myflag]
      ..end
      ..option push
      ..say Options pushed:   debug=[debug] hold=[hold] myflag=[myflag]
      ..option nodebug
      ..say Debug is now off: debug=[debug] hold=[hold] myflag=[myflag]
      ..option pop
      ..say Options popped:   debug=[debug] hold=[hold] myflag=[myflag]

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then do
    call queueHelpForVerb 'Option'
  end
  else do
    parse upper var g.1 . sOptions
    do i = 1 to words(sOptions)
      sOption = word(sOptions,i)
      select
        when sOption = 'PUSH' then call pushOptions
        when sOption = 'POP'  then call popOptions
        otherwise do
          if left(sOption,2) = 'NO'
          then do
            sOption = substr(sOption,3)
            call setOption sOption 0
          end
          else do
            call setOption sOption 1
          end
        end
      end
    end
  end
return

doOtherwise:
/*
### ..OTHERWISE [action]

  This is part of a select-when-otherwise-end construct:

    ..select [expr]
    ..  when 'value1'
          output if expr = 'value1'
    ..  when 'value2'
          output if expr = 'value2'
        .
        .
        .
    ..  otherwise
          output if expr does not equal any of the above values
    ..end

  The `..otherwise` clause is processed if all previous `..when` clauses
  of a `..select` statement are evaluated as false. First process the
  otherwise "action" (if present) and all subsequent statements until `..end`
  statement that closes the corresponding `..select` statement is found

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'Otherwise'
  else do
    parse var g.1 . sAction
    parse value peekStack() with sClause bEmit .
    g.0EMIT = bEmit & \g.0WHEN /* If emitting when SELECT was seen,
                                  and WHEN condition not met for this SELECT
                                  then we emit OTHERWISE input
                                  else we don't emit OTHERWISE input
                               */
    if g.0EMIT
    then do
      if sAction <> ''
      then interpret sAction
    end
  end
return

doPut:
/*
### ..PUT       fromdsn tosystem [todsn [siteoptions...]]

  This generates a job step that uses FTP to transfer local dataset
  "fromdsn" to system "tosystem" and optionally
  store it in a dataset called "todsn" on the target
  system. Use "siteoptions" to specify options for the FTP client
  SITE subcommand (for example, to specify RECFM=VB etc).

*/
  call addJCLCommentBlock 'Put dataset using FTP'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Put'
  else do
    parse var g.1 . sSource sSystem sTarget sSiteOptions
    call queueFTPStep
    queue getHost(sSystem)
    call queueFTP 'site ispfstats' sSiteOptions
    if binary = 1
    then queue 'binary'
    else queue 'ebcdic'
    sLastSystem = sSystem
    do i = 1 to g.0
      parse var g.i . sSource sSystem sTarget sSiteOptions
      if sSystem <> sLastSystem
      then do
        queue 'close'
        queue 'open' getHost(sSystem)
        call queueFTP 'site ispfstats' sSiteOptions
        if binary = 1
        then queue 'binary'
        else queue 'ebcdic'
      end
      if sTarget = ''
      then call queueFTP "put '"sSource"'"
      else call queueFTP "put '"sSource"' '"sTarget"'"
      sLastSystem = sSystem
    end
    queue 'quit'
    queue '/*'
  end
return

doQueue:
/*
### ..QUEUE     [line]

  This appends the specified line to a queue of input JAM
  statements to be processed when a subsquent `..queued`
  JAM statement is encountered.

  Note that if you need to queue a line that ends with a
  JAM statement continuation character (`,` or `-`) then
  you will need to do a trick to stop the JAM processor
  treating that character as a continuation: simply
  append [] to the line to be queued. The JAM processor
  will evaluate the expression inside the parentheses as null and
  will not detect the trailing continuation character. For example:

      ..queue //SYSUT1 DD DISP=SHR, []
      ..queue //          DSN=MY.DATASET, []
      ..queue //          RECFM=V,BLKSIZE=27990,LRECL=255

*/
  parse upper var g.1 . sParms
  if sParms = '?'
  then call queueHelpForVerb 'Queue'
  else do
    i.0 = g.0
    n = g.0QUEUE.0
    do i = 1 to g.0
      parse var g.i . sParms
      n = n + 1
      g.0QUEUE.n = sParms
    end
    g.0QUEUE.0 = n
  end
return

doQueued:
/*
### ..QUEUED

  This processes any JAM statements that were queued by
  earlier `..queue` JAM statements and then clears the
  queue.

  This is useful if you have a macro that has queued some
  output using `..queue` but you want to place it in a particular place
  after the macro has been invoked.

*/
  parse upper var g.1 . sParms
  if sParms = '?'
  then call queueHelpForVerb 'Queued'
  else do
    i.0 = g.0QUEUE.0
    do i = 1 to i.0
      i.i = g.0QUEUE.i
    end
    call appendIncludeLines
  end
  drop g.0QUEUE.
  g.0QUEUE.0 = 0
return

doQuit:
/*
### ..QUIT      [CANCEL] [quitmsg]

  This stops the JAM processor. If "quitmsg" is present
  then that message is displayed as the reason for
  quitting. If "CANCEL" is present then the ISPF EDIT
  session is also cancelled.
*/
  parse upper var g.1 . sParms . sQuitMsg
  if sParms = '?'
  then call queueHelpForVerb 'Quit'
  else do
    g.0QUIT = 1
    g.0CANCEL = sParms = 'CANCEL'
    if g.0CANCEL
    then parse var g.1 . . sQuitMsg
    else parse var g.1 . sQuitMsg
    if sQuitMsg <> ''
    then say sQuitMsg
    call initStack /* discard any stacked IF statements etc */
  end
return

doRecover:
/*
### ..RECOVER   dsn fromvol tovol [options...]

  This generates a job step that will copy dataset "dsn" from
  volume "fromvol" to volume "tovol" using ADRDSSU
  options "option" (for example, REPLACE, TOL(ENQF) etc).

*/
  bError = 0
  parse upper var g.1 . sDataset sFromVol sToVol sOptions
  do i = 1 to g.0
    parse var g.i . sDataset .
    if sDataset = '?' | sDataset = ''
    then bError = 1
  end
  if bError
  then do
    call queueHelpForVerb 'Recover'
    return
  end
  call addJCLCommentBlock 'Recover dataset'
  queue getStep('PGM=ADRDSSU')
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//SYSIN     DD *'
  queue '  COPY DATASET -'
  queue '       ( -'
  queue '         INCLUDE -'
  queue '         ( -'
  do i = 1 to g.0
    parse upper var g.i . sDataset .
    queue '          'sDataset '-'
  end
  queue '         ) -'
  queue '       ) -'
  queue '       LOGINDYNAM  ('sFromVol') -'
  queue '       OUTDYNAM    ('sToVol') -'
  queue '       ALLDATA (*) -'
  queue '       ALLEXCP -'
  queue '       PROCESS (SYS1) -'
  do i = 1 to words(sOptions)
    queue '       'word(sOptions,i) '-'
  end
  queue ' '
  queue '/*'
return

doRename:
/*
### ..RENAME    dsn todsn [volser]
### ..RENAME    pds(mem1[,mem2...]) (new1[,new2...]) [volser]
### ..RENAME    path topath

  This generates a job step that will rename a dataset, members in a
  partitioned dataset, or a Unix System Services file or directory.

*/
  parse var g.1 . sSource sTarget sVolser .
  sPrevUtil = getRenameUtility(sSource,sTarget,sVolser)
  nFrom = 1
  nTo = 0
  /* Group similar types of rename operation into the same utility */
  do nThis = 1 to g.0
    parse var g.nThis . sSource sTarget sVolser .
    sThisUtil = getRenameUtility(sSource,sTarget,sVolser)
    if sThisUtil <> sPrevUtil
    then do
      nTo = nThis - 1
      call performRenameOperation nFrom nTo
      nFrom = nThis
      sPrevUtil = sThisUtil
    end
  end
  if nTo < g.0 then call performRenameOperation nTo+1 g.0
return

getRenameUtility: procedure
  parse arg sSource,sTarget,sVolser
  sFromTo = getFileType(sSource) getFileType(sTarget)
  if sVolser = ''
  then select
    when sFromTo = 'PTH PTH' then sUtility = 'BPXBATCH'
    when sFromTo = 'MEM MEM' then sUtility = 'IDCAMS'
    when sFromTo = 'DSN DSN' then sUtility = 'IDCAMS'
    otherwise sUtility = 'ERROR'
  end
  else select
    when sFromTo = 'PTH PTH' then sUtility = 'BPXBATCH'
    when sFromTo = 'MEM MEM' then sUtility = 'IEHPROGM'
    when sFromTo = 'DSN DSN' then sUtility = 'IEHPROGM'
    otherwise sUtility = 'ERROR'
  end
return sUtility

performRenameOperation:
  parse arg nFrom nTo
  select
    when sPrevUtil = 'BPXBATCH' then call RenameUss   nFrom nTo
    when sPrevUtil = 'IEHPROGM' then call RenameOnVol nFrom nTo
    when sPrevUtil = 'IDCAMS'   then call RenameDsn   nFrom nTo
    otherwise call queueHelpForVerb 'Rename'
  end
return

RenameUss:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Rename Unix file',nFrom,nTo
  do i = nFrom to nTo
    parse var g.i sVerb sOperands
    g.i = sVerb 'mv -v' sOperands
  end
  call queueScript 'su<',nFrom,nTo
return

RenameOnVol:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Rename',nFrom,nTo
  drop z.
  z. = ''
  z.0DD.0 = 0
  queue getStep("PGM=BYPASSNQ,PARM='IEHPROGM'")
  queue '//SYSPRINT  DD SYSOUT=*'
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sVolser .
    if isNotCached('',sVolser)
    then do
      sDD = getDD('',sVolser)
      queue '//'left(sDD,9) 'DD DISP=OLD,UNIT=3390,VOL=SER='sVolser
    end
  end
  queue '//SYSIN     DD *'
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sVolser .
    parse var sSource sSource'('sMemberList')'
    if sMemberList = ''
    then call queueIEH 'RENAME DSNAME='sSource,
                              'NEWNAME='sTarget,
                              'VOL=3390='sVolser
    else do
      parse var sTarget '('sTargetList')'
      sMemberList = translate(sMemberList,' ',',')
      sTargetList = translate(sTargetList,' ',',')
      sDD  = getDD('',sVolser)
      do j = 1 to words(sMemberList)
        sSourceMember = word(sMemberList,j)
        sTargetMember = word(sTargetList,j)
        if sTargetMember <> ''
        then call queueIEH 'RENAME DSNAME='sSource,
                                  'MEMBER='sSourceMember,
                                  'NEWNAME='sTargetMember,
                                  'VOL=3390='sVolser
      end
    end
  end
  queue '/*'
return

RenameDsn:
  parse arg nFrom nTo
  call addJCLCommentBlock 'Rename dataset',nFrom,nTo
  queue getStep('PGM=IDCAMS')
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//SYSIN     DD *'
  drop z.
  z. = ''
  z.0DD.0 = 0
  do i = nFrom to nTo
    parse upper var g.i . sSource sTarget sOptions

    parse var sSource sSource'('sMemberList')'
    parse var sTarget        '('sTargetList')'
    sUniqueDD = getDD(sSource)
    if sMemberList <> ''
    then do
      sMemberList = translate(sMemberList,' ',',')
      sTargetList = translate(sTargetList,' ',',')
      do j = 1 to words(sMemberList)
        sSourceMember = word(sMemberList,j)
        sTargetMember = word(sTargetList,j)
        if sTargetMember <> ''
        then do
          queue '  ALTER    'sSource'('sSourceMember') -'
          queue '  NEWNAME ('sSource'('sTargetMember')) -'
          if sOptions = ''
          then do
            queue '  FILE    ('sUniqueDD')' /* Effectively bypass enqueue */
          end
          else do
            queue '  FILE    ('sUniqueDD') -'
            queue ' ' sOptions
          end
          queue
        end
      end
    end
    else do
      queue '  ALTER    'sSource '-'
      queue '  NEWNAME ('sTarget') -'
      if sOptions = ''
      then do
        queue '  FILE    ('sUniqueDD')'
      end
      else do
        queue '  FILE    ('sUniqueDD') -'
        queue ' ' sOptions
      end
      queue
    end
  end
  queue '/*'
  queue '//* Bypass enqueues on the following datasets...'
  do i = 1 to z.0DD.0       /* For each unique DSN name found */
    sDSN = z.0DD.i          /* Get the dataset name           */
    sDD  = z.0DD.sDSN       /* Get the unique DD name for it  */
    queue '//'left(sDD,8) 'DD DISP=SHR,DSN='sDSN /* Emit a DD */
  end
return

doRepro:
/*
### ..REPRO     fromdsn todsn [fromrec [count]]

  This generates a job step that will copy dataset "fromdsn" to
  dataset "todsn" starting at record number "fromrec"
  and continuing for "count" records (or End Of File)

*/
  call addJCLCommentBlock 'Repro dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Repro'
  else do
    queue getStep('PGM=IDCAMS')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sSource sTarget nFrom nCount .
      if datatype(sTarget,'WHOLE')
      then parse upper var g.i . sSource nFrom nCount .
      sFrom = ''
      sCount = ''
      if datatype(nFrom,'WHOLE')  then sFrom  = 'SKIP('nFrom-1')'
      if datatype(nCount,'WHOLE') then sCount = 'COUNT('nCount')'
      queue '  REPRO INFILE(I'i') OUTFILE(O'i')' sFrom sCount
    end
    queue '/*'
    do i = 1 to g.0
      parse upper var g.i . sSource sTarget nFrom nCount .
      if sTarget = '' | datatype(sTarget,'WHOLE')
      then do
        parse upper var g.i . sSource nFrom nCount .
        queue '//'left('O'i,9) 'DD SYSOUT=*,LRECL=255'
      end
      else queue '//'left('O'i,9) 'DD DISP=SHR,DSN='sTarget
      if sSource = '*'
      then queue '//'left('I'i,9) 'DD *'
      else queue '//'left('I'i,9) 'DD DISP=SHR,DSN='sSource
    end
  end
return

doRestore:
/*
### ..RESTORE   dsn [backup | =]  [options...]

  This generates a job step that will restore datasets "dsn" from
  a backup dataset "backup" that was created by ADRDSSU
  DUMP. If the "backup" dataset is omitted (or specified
  as "=") then the most recently specified "backup"
  dataset is used.

*/
  bError = 0
  sBackupPrev = userid()'.BACKUP'
  parse upper var g.1 . sDataset sBackup sOptions
  do i = 1 to g.0
    parse var g.i . sDataset .
    if sDataset = '?' | sDataset = ''
    then bError = 1
  end
  if bError
  then do
    call queueHelpForVerb 'Restore'
    return
  end
  call addJCLCommentBlock 'Restore dataset'
  if run = 0
  then queue getStep("PGM=ADRDSSU,PARM='TYPRUN=NORUN'")
  else queue getStep("PGM=ADRDSSU PARM='TYPRUN=NORUN'")
  queue '//SYSPRINT  DD SYSOUT=*'
  if sBackup = '' | sBackup = '='
  then sBackup = sBackupPrev
  sBackupPrev = sBackup
  queue '//BACKUP    DD DISP=SHR,DSN='sBackup
  queue '//SYSIN     DD *'
  queue '  RESTORE DATASET -'
  queue '       ( -'
  queue '         INCLUDE -'
  queue '         ( -'
  do i = 1 to g.0
    parse upper var g.i . sDataset .
    queue '          'sDataset '-'
  end
  queue '         ) -'
  queue '       ) -'
  queue '       REPLACEUNCONDITIONAL -'
  queue '       INDD     (BACKUP) -'
  queue '       TGTALLOC (SOURCE) -'
  queue '       CATALOG -'
  do i = 1 to words(sOptions)
    queue '       'word(sOptions,i) '-'
  end
  queue '       TOLERATE  (ENQFAILURE) -'
  queue ' '
  queue '/*'
return

doREXX:
/*
### ..REXX      statement

  This executes the specified REXX statement.

*/
  parse var g.1 . sParms
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'REXX'
  else do
    sStatements = ''
    do _i = 1 to g.0
      parse var g._i . sStatement
      sStatements = sStatements';' sStatement
    end
    interpret sStatements
  end
return

doRunOn:
/*
### ..RUNON     [system[/jobnamesuffix][/via]] [description]

  This generates JCL to run the following JCL on the system
  alias specified by "system".

  If "system" is not a known system alias then it will
  become the first word of the "description"

  If "jobnamesuffix" is omitted, it defaults to a single
  character which is the last character of the system alias.

  If "via" is specified, then the JCL will be submitted
  to the "via" system, which will submit it to the system
  and a copy of the output will be returned to the "via"
  system. Use this if you want, for example, output for
  jobs run on X, Y and Z, to be available on X.

  The first 20 characters of the "description" is put in
  the programmer name field of the JOB card.

  The job output will be written to a DD which has been
  automatically created as part of the FTP step:

      //alias DD SYSOUT=*

  If preceded by `..option hold` then the job is
  submitted to the target system with TYPRUN=HOLD (and no
  output is retrieved).

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then do
    call queueHelpForVerb 'RunOn'
  end
  else do
    step = 0                           /* Reset job step number              */
    parse upper var g.1 . sSystem sDesc '/*'  /* Ignore trailing comments    */
    parse var sSystem sSystem'/'sJobSuffix'/'sViaSystem
    if sysname.sSystem = '',           /* If a valid alias was not specified */
     & sJobSuffix = ''                 /* ...and no job suffix specified     */
    then do
      parse upper var g.1 . sDesc '/*'  /* Then parameters are all desc   */
      sSystem = ''
    end
    else do                            /* Parameters are alias/suffix/via    */
      if sViaSystem = ''
      then sViaSystem = sSystem
    end
    sDesc = strip(left(space(sDesc),20))
    if g.0DLM = 1
    then do
      queue '@@'
      g.0DLM = 0 /* No oustanding DLM=@@ termination */
    end
    call setAlias '*' /* Set alias REXX variables for the current system */
    /* If the "runon" system is not specified, or if the output
       is to be returned to the "via" system and the "via" system is
       this system - AND - if the "useftp" flag is not explicitly set
       ...then submit the job using NJE
       ...otherwise submit the job using FTP
       In summary: use NJE when possible, and use FTP if allowed */
    if (sSystem = '' | njenet.sViaSystem = njenet.alias) & useftp = 0
    then call doJob /* use NJE to submit job */
    else do /* use FTP to submit job to a TCPIP-connected NJE network, or when useftp is specified */
      job = job + 1 /* Generate unique (maybe) job suffix */
      if sJobSuffix = ''
      then call queueJob ,job         /* System generated job suffix */
      else call queueJob ,sJobSuffix  /* User-specified job suffix   */
      call queueXEQ                   /* Create XEQ/JOBPARM for the local system */
      if hold = 1
      then do                         /* Cannot retrieve output when held */
        say 'JAM001W Output cannot be retrieved by FTP when you specify',
            '"..option hold"'
        call addJCLComment 'Use FTP to submit job'
        call submitUsingFTP    '//DD:JCL' sSystem
      end
      else do
        call addJCLComment 'Use FTP to submit job and retrieve output'
        call submitUsingFTP    '//DD:JCL' sSystem '//DD:'sSystem sViaSystem
        queue '//'left(sSystem,9) 'DD SYSOUT=*'
      end
      call addJCLComment 'Put JCL to be submitted here'
      queue '//JCL       DD DATA,DLM=@@'
      call setAlias sSystem /* Set alias REXX variables for the execution system */
      if hold = 1
      then call queueJob 'TYPRUN=HOLD'
      else call queueJob
      call queueXEQ                   /* Create XEQ/JOBPARM for the execution system */
      g.0DLM = 1 /* Indicate DLM=@@ is unterminated */
    end
    step = 0
  end
return

doSay:
/*
### ..SAY       text

  This displays the specified message text on the user's terminal.

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'Say'
  else do
    do i = 1 to g.0
      parse var g.i . sMessage
      say sMessage
    end
  end
return

doScratch:
/*
### ..SCRATCH   dsn volser

  This generates a job step that will delete dataset "dsn" from
  volume "volser".

*/
  call addJCLCommentBlock 'Scratch dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Scratch'
  else do
    queue getStep("PGM=IEHPROGM")
    queue '//SYSPRINT  DD SYSOUT=*'
    drop bSeen.
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      if bSeen.sVolser <> 1
      then do
        queue '//'left('DD'i,9) 'DD VOL=SER='sVolser',UNIT=3390,DISP=SHR'
        bSeen.sVolser = 1
      end
    end
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sVolser .
      call queueIEH 'SCRATCH DSNAME='sDataset 'VOL=3390='sVolser 'PURGE'
    end
    queue '/*'
  end
return

doSelect:
/*
### ..SELECT    [expr]

  This is part of a select-when-otherwise-end construct:

    ..select [expr]
    ..  when 'value1'
          output if expr = 'value1'
    ..  when 'value2'
          output if expr = 'value2'
        .
        .
        .
    ..  otherwise
          output if expr does not equal any of the above values
    ..end

  If "expr" is present then the `..select` clause will evaluate the REXX
  expression and search for a `..when` clause expression that matches that
  value.

  If "expr" is omitted then subsequent `..when` clause expressions are
  evaluated and the first clause evaluating to 1 (true) will be processed.
  All remaining `..when` clauses (and the `..otherwise` clause, if present)
  will be ignored.

  If no matching `..when` clause is found then the `..otherwise`
  clause (if present) for this `..select` statement will be
  processed. An `..otherwise` clause is not mandatory.

  The following examples are equivalent:

  Example 1 - A `..select` clause with `expr` omitted:

      ..select
      ..  when [lpar = 'TEST']
      This is a test LPAR
      ..  when [lpar = 'PROD']
      This is a production LPAR
      ..  otherwise
      This is an unknown LPAR
      ..end

  Example 2 - A `..select` clause with `expr` present:

      ..select [lpar]
      ..  when 'TEST'
      This is a test LPAR
      ..  when 'PROD'
      This is a production LPAR
      ..  otherwise
      This is an unknown LPAR
      ..end

*/
  parse var g.1 . sParms .
  if sParms = '?'
  then call queueHelpForVerb 'Select'
  else do
    parse var g.1 . sSelValue
    call pushStack 'SELECT' g.0EMIT g.0WHEN g.0SELVALUE '/'g.1
    interpret "g.0SELVALUE = '"sSelValue"'"
    g.0WHEN = 0
  end
return

doSet:
/*
### ..SET       var = expr

  This evaluates the expression "expr" and assigns the result to a REXX
  variable called "var". Any valid REXX expression can be used.
  The variable named "alias" is special. If you set this
  variable then several other variables will be updated
  with information about that system alias. Use
  `..set ?` to list these variables.

*/
  parse var g.1 . sParms
  if sParms = '' | sParms = '?'
  then do
    call queueHelpForVerb 'Set'
    if sParms = '?'
    then call queueHelpFromLabel 'HelpVars:'
  end
  else do
    do i = 1 to g.0
      parse var g.i . sVarName'='sExpression
      sVarName = strip(sVarName)
      if sExpression = '' then sExpression = "''"
      interpret sVarName '=' sExpression
      if debug = 1
      then do
        say ' 'VarName "= '"value(sVarName)"'"
      end
      if translate(sVarName) = 'ALIAS' /* Special case: ..set alias = system */
      then call setAlias alias         /* Set variables for this system */
    end
  end
return

setAlias:
  /* An alias is a user-defined friendly name for a system */
  /* All aliases are defined in the "lpars" table */
  parse upper arg id            /* id is either an alias or a sysname */
  if sysname.id <> ''           /* If alias is in the lpars map */
  then alias = id               /* Then use the specified alias */
  else do                       /* Else use a suitable default */
    if g.0ZOS
    then alias = g.0SYSNAME     /* On z/OS use SYSNAME of this system */
    else alias = alias.1        /* Else use the first alias in the map */
  end
  sysname  = sysname.alias         /* SysName                  */
  jc       = jc.alias              /* Job class                */
  mc       = mc.alias              /* Message class            */
  sysclone = sysclone.alias        /* SysClone                 */
  sysplex  = sysplex.alias         /* SysPlex                  */
  jesname  = jesname.alias         /* JES2 node name           */
  jesnode  = jesnode.alias         /* JES2 node number         */
  cat      = cat.alias             /* Catalog name             */
  tags     = tags.alias            /* Miscellaneous tags       */
  host     = host.alias            /* Host name                */
  jobname  = getJobName(right(alias,1)) /* Job name with alias suffix */
  if debug = 1
  then do
    say "  alias    = '"alias"'"
    say "  sysname  = '"sysname"'"
    say "  jc       = '"jc"'"
    say "  mc       = '"mc"'"
    say "  sysclone = '"sysclone"'"
    say "  sysplex  = '"sysplex"'"
    say "  jesname  = '"jesname"'"
    say "  jesnode  = '"jesnode"'"
    say "  njenet   = '"njenet"'"
    say "  cat      = '"cat"'"
    say "  tags     = '"tags"'"
    say "  host     = '"host"'"
  end
return


doShip:
/*
### ..SHIP      dsn tosystem [todsn [options...]]

  This transfers a dataset to another system.

  Generates JCL to:
  - Convert "dsn" to NETDATA format using the TSO TRANSMIT command,
  - Transfer that archive to remote system "tosystem" using FTP,
  - Receive the archive on the remote system into a dataset called "todsn"
    using the TSO RECEIVE command.

  Optionally, you can set TSO RECEIVE command options by
  setting "options". Typically, you would set the todsn
  disposition (OLD, NEW or SHR).

  You may ship selected members of a PDS by specifying a list of
  member names in parentheses after the dataset name. For example,
  `dsn(member1 member2...)`.

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Ship'
  else do
    do i = 1 to g.0
      parse upper var g.i . sParms
      sMembers = ''
      parse upper var g.i . sDSN sSystem sToDSN sOther
      if pos('(',sDSN) > 0
      then parse upper var g.i . sDSN'('sMembers')' sSystem sToDSN sOther
      if sToDSN = '' then sToDSN = sDSN
      call addJCLCommentBlock 'Ship' sDSN 'to' sSystem
      call addJCLCommentBlock 'Create XMIT archive on' alias
      call queueTSOStep
      if sMembers <> ''
      then sMembers = 'MEMBERS('space(translate(sMembers,'',','))')'
      call queueTSO 'TRANSMIT' jesname.alias'.'userid,
                    "DATASET('"sDSN"')" sMembers,
                    "OUTFILE(XMIT) SYSOUT(Z) NONOTIFY NOLOG"
      queue '/*'
      if g.0ZOS
      then rc = listdsi("'"sDSN"' RECALL")
      if debug = 1
      then do
        say 'Allocation information for' sDSN
        say '  SYSDSNAME    =' SYSDSNAME
        say '  SYSVOLUME    =' SYSVOLUME
        say '  SYSUNIT      =' SYSUNIT
        say '  SYSDSORG     =' SYSDSORG
        say '  SYSRECFM     =' SYSRECFM
        say '  SYSLRECL     =' SYSLRECL
        say '  SYSBLKSIZE   =' SYSBLKSIZE
        say '  SYSKEYLEN    =' SYSKEYLEN
        say '  SYSALLOC     =' SYSALLOC
        say '  SYSUSED      =' SYSUSED
        say '  SYSUSEDPAGES =' SYSUSEDPAGES
        say '  SYSPRIMARY   =' SYSPRIMARY
        say '  SYSSECONDS   =' SYSSECONDS
        say '  SYSUNITS     =' SYSUNITS
        say '  SYSEXTENTS   =' SYSEXTENTS
      end
      select
        when SYSUNITS = 'BLOCK' then,
          sSpace = '('sysblksize',('sysprimary','sysseconds'),RLSE)'
        when SYSUNITS = 'CYLINDER' then,
          sSpace = '(CYL,('sysprimary','sysseconds'),RLSE)'
        when SYSUNITS = 'TRACK' then,
          sSpace = '(TRK,('sysprimary','sysseconds'),RLSE)'
        otherwise,
          sSpace = '(CYL,(100,50),RLSE)    <-- Is this enough?'
      end
      queue '//XMIT      DD DISP=(NEW,PASS),DSN=&&XMIT,'
      queue '//             SPACE='sSpace
      call addJCLCommentBlock 'FTP the archive to' sSystem
      call queueFTPStep
      queue getHost(sSystem)
      queue 'binary'
      sXmitDSN = getTempFileName()'.XMIT'
      call queueFTP "put '//DD:XMIT'" sXmitDSN
      queue 'quit'
      queue '/*'
      queue '//XMIT      DD DISP=(OLD,DELETE,DELETE),DSN=&&XMIT'
      call addJCLCommentBlock 'Use FTP to run a receive job on' sSystem
      queue '//          IF (STEP'step'.RC = 0) THEN'
      call submitUsingFTP '//DD:JCL'i sSystem '//DD:'sSystem
      queue '//'left(sSystem,9) 'DD SYSOUT=*'
      call addJCLCommentBlock 'Job to receive the archive at' sSystem
      queue '//'left('JCL'i,9) "DD DATA,DLM='\\'"
      lastAlias = alias
      call setAlias sSystem
      lastDLM = g.0DLM
      g.0DLM = 0 /* Suppress DLM=@@ termination during SHIP */
      call queueJob
      g.0DLM = lastDLM
      call queueXEQ
      call queueTSOStep
      call queueTSO 'RECEIVE INFILE(XMIT)'
      call queueTSO "DATASET ('"sToDSN"')" sOther
      queue '/*'
      queue '//XMIT      DD DISP=(OLD,DELETE,DELETE),'
      queue '//             DSN=&SYSUID..'sXmitDSN
      queue '\\'
      queue '//       ENDIF'
      call setAlias lastAlias
    end
  end
return

doStep:
/*
### ..STEP      args

  This generates an EXEC card with the specified arguments.

  For example,

      ..step PGM=IEFBR14
      ..step PGM=IEBCOPY

  generates:

      //STEP1    EXEC PGM=IEFBR14
      //STEP2    EXEC PGM=IEBCOPY

  The step number is automatically incremented each time the
  `..step` JAM statement is used.

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Step'
  else do
    do i = 1 to g.0
      parse var g.i . sParms
      queue getStep(sParms)
    end
  end
return

doStyle:
/*
### ..STYLE     name[=width,first,borderleft,borderfill,borderright,commentleft,commentright,last]

  This creates a named set of global REXX variables that are used by the
  `..*` JAM statements to generate styled comment lines.

  To illustrate where where the parameter texts are placed you can code:

      ..style test=60,1,2,3,4,5,6,7
      ..* This shows where each parameter appears in the output

  which generates:

      1
      233333333333333333333333333333333333333333333333333333333334
      5 This shows where each parameter appears in the output    6
      233333333333333333333333333333333333333333333333333333333334
      7

  The parameters passed to the `..style` JAM statement are:

  | Parameter   | Meaning
  | ---------   | --------------------------------------------
  | name        | The name of the set of variables to be defined and/or activated
  | width       | The width of the comment line
  | first       | The characters (if any) to emit before the top border line
  | borderleft  | The leftmost characters of the top and bottom border line
  | borderfill  | The single fill character of the top and bottom border line
  | borderright | The rightmost characters of the top and bottom border line
  | commentleft | The leftmost characters of each comment line
  | commentright| The rightmost characters of each comment line
  | last        | The characters (if any) to emit after the bottom border line

  If you omit the parameters then the named set becomes the active set for
  subsequent `..*` JAM statements. For example, to set assembler-style comments:

      ..style asm

  If the named set does not exist then the default comment style (jcl) becomes active.

  If you only specify the width parameter, then only the width is updated for this style.
  For example, the following sets the comment width to 40 characters for the asm style:

      ..style asm=40

  If you omit the borderfill parameter then no top or bottom border will be generated.

  The following styles are pre-defined:

  | name | width | first   | border<br/>left | border<br/>fill | border<br/>right | comment<br/>left | comment<br/>right | last    |
  | ---- | ----- | ------- | -----------     | --------------- | ---------------- | ---------------- | ----------------- | ----    |
  | asm  | 71    | *       | *               | -               | *                | *                | *                 | *       |
  | box  | 71    |         | **              | *               | **               | **               | **                |         |
  | c    | 80    | //      | //              | -               | -                | //               | -                 | //      |
  | jcl  | 71    | //&ast; | //&ast;         | -               | *                | //&ast;          | *                 | //&ast; |
  | js   | 80    | //      | //              | -               |                  | //               |                   | //      |
  | rexx | 80    | /&ast;  | &nbsp;*         | -               | &ast;/           | &nbsp;*          | &ast;/            | &ast;/  |
  | xml  | 80    | <!--    |                 |                 |                  |                  |                   | -->     |

  Note that xml comments cannot contain double hypens (--).

  To override a built-in default style:

      ... Override the built-in asm default style:
      ..style asm=60,*,*,-,*,*,*,*
      ..* This is an assembler comment

      ... Create a new style called myxml:
      ..style myxml=60,,,,<!--,-->,
      ..* This is my xml comment

      ... Reset to the (overridden) asm style:
      ..style asm
      ..* This is also an assembler comment

  generates comments as follows (limited to 60 characters):

      *
      *----------------------------------------------------------*
      * This is an assembler comment                             *
      *----------------------------------------------------------*
      *

      <!-- This is my xml comment                              -->

      *
      *----------------------------------------------------------*
      * This is also an assembler comment                        *
      *----------------------------------------------------------*
      *

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Style'
  else do
    do i = 1 to g.0
      parse var g.i . sName'='nWidth','sFirst','sBorderLeft','sBorderFill','sBorderRight','sLeft','sRight','sLast
      call setStyle sName,nWidth,sFirst,sBorderLeft,sBorderFill,sBorderRight,sLeft,sRight,sLast
    end
  end
return

setStyle: procedure expose g.
  parse arg sName,nWidth,sFirst,sBorderLeft,sBorderFill,sBorderRight,sLeft,sRight,sLast
  /* say 'setStyle <'sName','nWidth','sFirst','sBorderLeft','sBorderFill','sBorderRight','sLeft','sRight','sLast'>' */
  g.0STYLE = sName /* Activate this style */
  if isNum(nWidth) & nWidth > 0
  then g.0STYLE_WIDTH.sName = nWidth
  if length(sFirst||sBorderLeft||sBorderFill||sBorderRight||sLeft||sRight||sLast) > 0
  then do
    g.0STYLE_FIRST.sName = sFirst
    g.0STYLE_BORDER_LEFT.sName  = sBorderLeft
    g.0STYLE_BORDER_FILL.sName  = sBorderFill
    g.0STYLE_BORDER_RIGHT.sName = sBorderRight
    g.0STYLE_LEFT.sName  = sLeft
    g.0STYLE_RIGHT.sName = sRight
    g.0STYLE_LAST.sName  = sLast
    if isNum(nWidth) & nWidth > 0
    then g.0STYLE_WIDTH.sName = nWidth
  end
  if \isNum(g.0STYLE_WIDTH.sName) | g.0STYLE_WIDTH.sName <= 0
  then g.0STYLE_WIDTH.sName = 71
return


doSubmit:
/*
### ..SUBMIT    dsn [tosystem [outdsn]]

  This submits the JCL in dataset "dsn" to the system alias specified by
  "tosystem", or to the system specified by the
  value of the "alias" REXX variable if "tosystem" is omitted.

  Optionally, if "outdsn" is specified, you can retrieve
  the output from the remote system and store it in local
  dataset called "outdsn". Note: If you want to retrieve
  the output then the job name on the target system must
  be your userid with a single character appended. This
  is required when the target system FTP server is configured to use
  JESINTERFACELEVEL 1 (which is the default). If the FTP server
  JESINTERFACELEVEL level is configured as 2, then you can choose any job
  name that you want (subject to RACF authorisations).

*/
  call addJCLCommentBlock 'Submit job'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Submit'
  else do
    do i = 1 to g.0
      parse upper var g.i . sJob sSystem sLocalDSN
      if (sSystem = '' | njenet.sSystem = njenet.xeq) & useftp = 0
      then call submitUsingINTRDR sJob
      else call submitUsingFTP    sJob sSystem sLocalDSN
    end
  end
return

submitUsingINTRDR:
  parse upper arg sJob
  queue getStep('PGM=IEBGENER')
  queue '//SYSIN     DD DUMMY'
  queue '//SYSPRINT  DD SYSOUT=*'
  if pos(':',sJob) > 0
  then do
    parse var sJob ':'sDDName .
    queue '//SYSUT1    DD DDNAME='sDDName
  end
  else queue '//SYSUT1    DD DISP=SHR,DSN='sJob
  queue '//SYSUT2    DD SYSOUT=(*,INTRDR)'
return

submitUsingFTP:
  parse upper arg sJob sSystem sLocalDSN sViaSystem .
  call queueFTPStep
  if sViaSystem = ''
  then sViaSystem = sSystem
  queue getHost(sViaSystem)
  if sLocalDSN  = ''
  then do
    queue 'site filetype=jes'
    queue 'ebcdic'
    call queueFTP "put '"sJob"'"
  end
  else do
    queue 'site filetype=seq'
    queue 'ebcdic'
    sRemoteJob = getTempFileName()'.JCL'
    call queueFTP "put '"sJob"'" sRemoteJob
    call queueSite
    call queueFTP 'get' sRemoteJob "'"sLocalDSN"' (replace"
    queue 'site filetype=seq'
    call queueFTP 'delete' sRemoteJob
  end
  queue 'quit'
  queue '/*'
return

doSudo:
/*
### ..SUDO      unixcommand

  This generates a job step to execute the specified command as superuser in the Unix
  System Services environment. The invoker will need to be permitted
  RACF READ access to BPX.SUPERUSER (or have uid=0) for this to be
  effective.

*/
  call addJCLCommentBlock 'Execute Unix command as superuser'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Sudo'
  else do
    if g.0 = 1 /* if only one USS command */
    then do
      parse var g.1 . sCommand
      sParm = "'SH echo" '"'strip(sCommand)'"'"|su'"
      if length(sParm) > 102 /* 100 max plus quotes */
      then call queueScript 'su<'
      else do
        queue getStep('PGM=BPXBATCH,REGION=0M,')
        call queueSTR '//             PARM='sParm
        queue '//STDOUT    DD SYSOUT=*,RECFM=V,LRECL=512'
        queue '//STDERR    DD SYSOUT=*,RECFM=V,LRECL=512'
        queue '/*'
      end
    end
    else do
      call queueScript 'su<'
    end
  end
return

doTable:
/*
### ..TABLE     dsn
  This reads tabular data in dataset "dsn" (containing column headings) into REXX variables
  that are indexed by row number. See below for more detail.

### ..TABLE     dsn col1 [col2 ...]

  This reads tabular data in dataset "dsn" (with no column headings) into REXX variables
  that are indexed by row number. The column headings are supplied on the `..table`
  command itself.

  For "dsn" you can specify either:
  - A fully qualified unquoted dataset name: dsn
  - A member in a partitioned dataset:       dsn(member)
  - A member in the dataset being edited:    (member)

  Table data is stored in REXX variables in the traditional manner as follows:

      col1.0 col2.0 ... colx.0    <-- Number of rows
      col1.1 col2.1 ... colx.1    <-- Values of row 1 fields
      col1.2 col2.2 ... colx.2    <-- Values of row 2 fields
        .      .
        .      .
      col1.y col2.y ... colx.y    <-- Values of row y fields


  * Example 1:

        ..table (mytab) user phone email
        ..say Number of rows is [user.0]
        ..say Row 2 of the table contains:
        ..say   user: [user.2]
        ..say  phone: [phone.2]
        ..say  email: [email.2]

  * Example 2:

        ..macro define list user phone email
        ..  say u=[user] p=[phone] e=[email]
        ..macro end
        ..table (mytab) user phone email
        ..for 1 to [user.0] macro list

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Table'
  else do i = 1 to g.0
    parse       var g.i . sDSN .
    parse upper var g.i . .    sFieldNames
    nRows = readFile(getFileName(sDSN))
    call reformatTableData
    /* Now parse the TABLE data into variables */
    nFieldNames = words(sFieldNames)
    or = 0                           /* Output row number  */
    do ir = nFirstRow to nRows       /* Input row number   */
      or = or + 1
      sRest = line.ir
      do c = 1 to nFieldNames        /* For each field     */
        sField = word(sFieldNames,c) /* Get the field name */
        c1 = left(word(sRest,1),1)   /* Values may be enclosed with quotes */
        select
          when c1 = "'" then parse var sRest "'"sValue"'" sRest
          when c1 = '"' then parse var sRest '"'sValue'"' sRest
          otherwise          parse var sRest    sValue    sRest
        end
        if sField <> '.'             /* If it is not a place holder */
        then do
          if c = nFieldNames & sRest <> '' /* If last field */
          then sValue = sValue strip(sRest) /* Append residual text */
          call assign sField'.'or,sValue
        end
      end
    end
    do c = 1 to nFieldNames          /* For each field     */
      sField = word(sFieldNames,c)   /* Get the field name */
      if sField <> '.'               /* If it is not a place holder */
      then do
        interpret sField'.0 =' or    /* Set number of rows for this field */
      end
    end
  end
return

/* This overcomes a limitationof REXX: longest token length is 250 */
assign:
  parse arg __Name,__Value
  if length(__Name)+length(__Value)+3 < 250
  then do
    interpret __Name"='"toStr(__Value)"'"
  end
  else do
    interpret __Name"=''"
    do while length(__Value) > 0
      parse var __Value __Chunk +100 __Value
      interpret __Name"="__Name"||'"toStr(__Chunk)"'"
    end
  end
return

reformatTableData:
  /* 1. Handle continuations, if any */
  bContMap = 0
  sContMap = ''
  or = 0
  do ir = 1 to nRows
    line.ir = strip(line.ir) /* ignore leading and trailing spaces */
    if right(line.ir,1) = '-'
    then do /* continuation */
      line.ir = left(line.ir,length(line.ir)-1) /* remove trailing '-' */
      if \bContMap /* new continuation */
      then do
        or = or + 1
        line.or = line.ir
        bContMap = 1
      end
      else do      /* continued continuation */
        line.or = line.or line.ir
      end
    end
    else do /* no continuation */
      if bContMap /* last line of a continuation block */
      then do
        line.or = line.or line.ir
        bContMap = 0
      end
      else do
        if line.ir <> '' /* ignore blank lines */
        then do
          or = or + 1
          line.or = line.ir
        end
      end
    end
  end
  nRows = or
  /* 2. Handle specified field names, if any */
  if sFieldNames = ''
  then do
    sFieldNames = toUpper(line.1)  /* Field names are on row 1   */
    nFirstRow = 2                  /* First row of data is row 2 */
    nOutputRows = nRows - 1
  end
  else do                          /* Field names are specified  */
    nFirstRow = 1                  /* First row of data is row 1 */
    nOutputRows = nRows
  end
return

doTSO:
/*
### ..TSO       tsocommand

  This generates a job step to executes the specified TSO command in batch

*/
  call addJCLCommentBlock 'Execute TSO command'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'TSO'
  else do
    call queueTSOStep
    do i = 1 to g.0
      parse var g.i . sCommand
      call queueTSO sCommand
    end
    queue '/*'
  end
return

doUncatalog:
/*
### ..UNCATALOG dsn [catalog]

  This generates a job step to uncatalogs dataset "dsn" from the specified catalog, or
  else from the catalog appropriate for the "alias" system.

*/
  call addJCLCommentBlock 'Uncatalog dataset'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Uncatalog'
  else do
    queue getStep('PGM=IDCAMS')
    queue '//SYSPRINT  DD SYSOUT=*'
    queue '//SYSIN     DD *'
    do i = 1 to g.0
      parse upper var g.i . sDataset sCatalog .
      if sCatalog = '' then sCatalog = cat
      queue '  DELETE' sDataset '-'
      queue '         NOSCRATCH -'
      queue '         CATALOG   ('sCatalog')'
    end
    queue '/*'
  end
return

doUnmount:
/*
### ..UNMOUNT   dsn [options...]

  This generates a job step to unmount file system "dsn" using any "options" valid on
  the TSO UNMOUNT command.

*/
  call addJCLCommentBlock 'Unmount file system'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'Unmount'
  else do
    call queueTSOStep
    do i = 1 to g.0
      parse upper var g.i . sDataset sOptions
      call queueTSO '  UNMOUNT FILESYSTEM('sDataset')' sOptions
    end
    queue '/*'
  end
return

doUSS:
/*
### ..USS       unixcommand

  This generates a job step to executes the specified command in the Unix System
  Services environment.

*/
  call addJCLCommentBlock 'Execute Unix command'
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'USS'
  else do
    if g.0 = 1 /* if only one USS command */
    then do
      parse var g.1 . sCommand
      sParm = "'SH "strip(sCommand)"'"
      if length(sParm) > 102 /* 100 max plus quotes */
      then call queueScript
      else do
        queue getStep('PGM=BPXBATCH,REGION=0M,')
        call queueSTR '//             PARM='sParm
        queue '//STDOUT    DD SYSOUT=*,RECFM=V,LRECL=512'
        queue '//STDERR    DD SYSOUT=*,RECFM=V,LRECL=512'
        queue '/*'
      end
    end
    else do
      call queueScript
    end
  end
return

queueScript:
  parse arg sCmd,nFrom,nTo
  if nFrom = '' then nFrom = 1
  if nTo   = '' then nTo   = g.0
  call queueTSOStep
  queue 'OCOPY INDD(CMDS) OUTDD(SCRIPT) TEXT'
  queue '/*'
  queue '//CMDS      DD *'
  queue 'set -x' /* Trace commands executed in this script */
  do i = nFrom to nTo
    parse var g.i . sCommand
    call queueUSS sCommand
  end
  queue '/*'
  sScript = '/tmp/'toLower(userid)'.sh'
  queue "//SCRIPT    DD PATH='"sScript"',"
  queue '//             PATHMODE=SIRWXU,'
  queue '//             PATHOPTS=(OWRONLY,OCREAT,OTRUNC)'
  if quiet = 0
  then call addJCLComment 'Run script'
  queue getStep('PGM=BPXBATCH,REGION=0M,')
  queue "//             PARM='sh" sCmd || sScript"'"
  queue "//SCRIPT    DD PATH='"sScript"',"
  queue '//             PATHOPTS=ORDONLY,PATHDISP=(DELETE,KEEP)'
  queue '//STDOUT    DD SYSOUT=*,RECFM=V,LRECL=512'
  queue '//STDERR    DD SYSOUT=*,RECFM=V,LRECL=512'
return

doWhen:
/*
### ..WHEN      expr [action]

  This is part of a select-when-otherwise-end construct:

    ..select [expr]
    ..  when 'value1'
          output if expr = 'value1'
    ..  when 'value2'
          output if expr = 'value2'
        .
        .
        .
    ..  otherwise
          output if expr does not equal any of the above values
    ..end

  The REXX expression specified by "expr" is evaluated. The result could
  be either 1 (true), 0 (false) or a string that could match a
  subsequent `..when` clause.

  If "expr" evaluates as 1, execute the "action" (if present) and process all
  statements in this `..when` clause and then ignore all
  statements until the `..end` of the owning
  `..select` JAM statement is found.

  If "expr" evaluates as 0, ignore all statements in this `..when` clause.

  If "expr" evaluates as neither 0 nor 1, compare the value of "expr" to the
  value of the variable specified on the owning `..select` and
  process the resulting 0 or 1 as decribed above.

  The following examples are equivalent:

  Example 1 - A `..select` clause with `expr` omitted:

      ..select
      ..  when [lpar = 'TEST']
      This is a test LPAR
      ..  when [lpar = 'PROD']
      This is a production LPAR
      ..  otherwise
      This is an unknown LPAR
      ..end

    Example 1 - A `..select` clause with `expr` present:

      ..select [lpar]
      ..  when 'TEST'
      This is a test LPAR
      ..  when 'PROD'
      This is a production LPAR
      ..  otherwise
      This is an unknown LPAR
      ..end

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'When'
  else do
    parse var g.1 . bCondition sAction 0 . sConst .
    if g.0SELVALUE <> '' /* If value specified on SELECT clause */
    then do
      c = left(sConst,1) /* Convert A to 'A' */
      if c = "'" | c = '"'
      then parse var g.1 . (c) sConst (c) sAction
      else c = "'"
      interpret 'bCondition = g.0SELVALUE =' c||sConst||c
    end
    else do
      if bCondition <> '1' & bCondition <> '0'
      then bCondition = 0
    end
    parse value peekStack() with sClause bEmit .
    g.0EMIT = bEmit & bCondition & \g.0WHEN   /* If emitting at SELECT,
                                                 and WHEN condition is met,
                                                 and no prior WHEN was met
                                                 then we emit this WHEN input
                                                 else we don't emit WHEN input
                                              */
    if g.0EMIT
    then do
      g.0WHEN = 1 /* Remember WHEN already executed for this SELECT */
      if sAction <> ''
      then interpret sAction
    end
  end
return

doXEQ:
/*
### ..XEQ      [alias]

  This will generate XEQ and JOBPARM cards for the specified system alias,
  or else from the system currently specified by the "alias" variable.
  If you specify "alias" then the "alias" variable will be set to that
  value, so the following:

      ..set alias = 'TST1'
      ..xeq

  is equivalent to:

      ..xeq TST1

  and will generate JCL, for example:

      /&ast;XEQ N11                 (JESTST1)
      /&ast;JOBPARM SYSAFF=R2D2     (TST1)

  Note: This verb is rarely needed. See `..runon` and `..job`.

*/
  parse upper var g.1 . sAlias .
  select
    when sAlias = '' then do
      call queueXEQ
    end
    when jesnode.sAlias <> '' then do
      call setAlias sAlias
      call queueXEQ
    end
    otherwise do
      call queueHelpForVerb 'XEQ'
    end
  end
return

doXMIT:
/*
### ..XMIT      dsn [system [userid [options...]]]

   This generates JCL to issue the TSO TRANSMIT to send dataset
   "dsn" to userid "userid" at system "system".
   The "options" can be any options valid for the TRANSMIT
   command (but you must specify system and userid too).
   The default "system" is the invoking system.
   The default "userid" is the invoking userid.

*/
  parse var g.1 . sParms .
  if sParms = '' | sParms = '?'
  then call queueHelpForVerb 'XMIT'
  else do
    do i = 1 to g.0
      parse upper var g.i . sDSN sSystem sUserid sOptions
      if sUserid = '' then sUserid = userid
      call addJCLCommentBlock 'Transmit dataset'
      call queueTSOStep
      call queueTSO 'TRANSMIT' nodename(sSystem)'.'sUserid,
                    "DATASET('"sDSN"')" sOptions
      queue '/*'
    end
  end
return
/*
 *---------------------------------------------------------------------
 * Output functions
 *---------------------------------------------------------------------
*/
queueTSOStep:
  queue getStep('PGM=IKJEFT01')
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//SYSTSPRT  DD SYSOUT=*'
  queue '//SYSTSIN   DD *'
return

queueFTPStep:
  queue getStep("PGM=FTP,PARM='(EXIT'")
  /* Note: The SYSTCPD DD has not been required since the RESOLVER
           address space was implemented with z/OS 1.9. However,
           this FTP JCL may still need a SYSFTPD DD if the site has
           local FTP client customisation requirements.
  */
  queue '//SYSPRINT  DD SYSOUT=*'
  queue '//OUTPUT    DD SYSOUT=*'
  queue '//INPUT     DD *'
return

queueXEQ:
  queue left('/*XEQ' jesnode.alias,30)            '('jesname.alias')'
  queue left('/*JOBPARM SYSAFF='sysname.alias,30) '('alias')'
return

queueFTP: procedure
  parse arg sCommand
  call queueContinuation sCommand,'',71,'+'
return

queueTSO: procedure
  parse arg sCommand
  call queueContinuation sCommand,'',71,'+'
return

queueAMS: procedure
  parse arg sCommand
  call queueContinuation sCommand,'',71,'-'
return

queueJCLComment: procedure
  parse arg sComment
  call queueContinuation sComment,'//* ',71,' '
return

queueContinuation: procedure
  parse arg sString,sPrefix,nLength,sCont
  nMaxLength = nLength - length(sPrefix) - length(sCont) - 1
  sProposedLine = sPrefix || sString
  if length(sProposedLine) <= nMaxLength
  then queue sProposedLine
  else do
    sOut = ''
    do i = 1 to words(sString)
      sWord = word(sString,i)
      sProposedLine = sPrefix || sOut sWord
      if length(sProposedLine) > nMaxLength
      then do
        queue sPrefix || strip(sOut sCont)
        sOut = sWord
      end
      else sOut = sOut sWord
    end
    queue sPrefix || sOut
  end
return

queueUSS: procedure
  parse arg sCommand
  if length(sCommand) < 80
  then queue sCommand
  else do
    do while length(sCommand) >= 80
      parse var sCommand sOut +79 sCommand
      queue sOut'\' /* USS continuation is a backslash in column 80 */
    end
    if sCommand <> ''
    then queue sCommand
  end
return

queueSTR: procedure
  parse arg sStatement
  if length(sStmt) < 72
  then queue sStmt
  else do
    parse var sStmt sOut +71 sStmt
    queue sOut
    do while length(sStmt) >= 56
      parse var sStmt sOut +56 sStmt
      queue '//             'sOut
    end
    if sStmt <> ''
    then queue '//             'sStmt
  end
return

queueIEH: procedure expose g.
  parse arg sCommand sParms
  nParms = words(sParms)
  if nParms = 1
  then queue '  'left(sCommand,13)sParms
  else do
    queue left('  'left(sCommand,13)word(sParms,1)',',71)'-'
    do i = 2 to nParms-1
      queue left('',15)left(word(sParms,i)',',56)'-'
    end
    queue left('',15)word(sParms,nParms)
  end
return

addJCLCommentBlock:
  parse arg sComment,nFrom,nTo
  if quiet = 0
  then do
    call addJCLComment sComment
    if verbose = 1
    then do
      if nFrom = '' then nFrom = 1
      if nTo   = '' then nTo   = g.0
      do i = nFrom to nTo
        call queueJCLComment strip(g.i)
      end
      queue '//*'
    end
  end
return

/*
 *---------------------------------------------------------------------
 * Initialisation
 *---------------------------------------------------------------------
*/
Prolog:
  g.0HARDBLANK = 'ff'x /* Hard blank to help with parsing                */
  g.0DELTA  = 0        /* Progress counter                               */
  g.0VERBS = ''        /* List of JAM verbs found in the program */
  /* Identify help text for each JAM verb */
  do i = 1 until sourceline(i) = 'eof:'
    sLine = sourceline(i)
    select
      when left(sLine,2) = 'do' then do
        parse upper var sLine 'DO'sVerb':'
        g.0VERBS = g.0VERBS sVerb
        g.0HELPBEG.sVerb = i+2 /* Point to the first line of help */
        do i = i+2 while sourceline(i) <> '*/'
        end
        g.0HELPEND.sVerb = i-1
      end
      otherwise nop
    end
  end
  /* EBCDIC characters that can typically be displayed by ISPF EDIT */
  g.0EBCDIC  = '40'x                   ||, /* (space)    */
               '4A4B4C4D4E4F50'x       ||, /* ¢.<(+|&    */
               '5A5B5C5D5E5F6061'x     ||, /* !$*);^-/   */
               '6A6B6C6D6E6F'x         ||, /* |,%_>?     */
               '7A7B7C7D7E7F'x         ||, /* :#@'="     */
               '818283848586878889'x   ||, /* abcdefghi  */
               '919293949596979899'x   ||, /* jklmnopqr  */
               'A1A2A3A4A5A6A7A8A9'x   ||, /* ~stuvwxyz  */
               'ADBA'x                 ||, /* [[         */
               'BDBB'x                 ||, /* ]]         */
               'C0C1C2C3C4C5C6C7C8C9'x ||, /* {ABCDEFGHI */
               'D0D1D2D3D4D5D6D7D8D9'x ||, /* }JKLMNOPQR */
               'E0'x                   ||, /* \          */
               'E2E3E4E5E6E7E8E9'x     ||, /* STUVWXYZ   */
               'F0F1F2F3F4F5F6F7F8F9'x     /* 0123456789 */

  /* The above EBCDIC characters translated to ASCII */
  g.0ASCII   = '20'x                   ||, /* (space)    */
               'A22E3C282B7C26'x       ||, /* ¢.<(+|&    */
               '21242A293B5E2D2F'x     ||, /* !$*);^-/   */
               '4F2C255F3E3F'x         ||, /* |,%_>?     */
               '3A2340273D22'x         ||, /* :#@'="     */
               '616263646566676869'x   ||, /* abcdefghi  */
               '6A6B6C6D6E6F707172'x   ||, /* jklmnopqr  */
               '7E737475767778797A'x   ||, /* ~stuvwxyz  */
               '5B5B'x                 ||, /* [[         */
               '5D5D'x                 ||, /* ]]         */
               '7B414243444546474849'x ||, /* {ABCDEFGHI */
               '7A4A4B4C4D4E4F505152'x ||, /* }JKLMNOPQR */
               '5C'x                   ||, /* \          */
               '535455565758595A'x     ||, /* STUVWXYZ   */
               '30313233343536373839'x     /* 0123456789 */

  /* EBCDIC characters that are duplicated in character constants */
  g.0APOST  = '7D'x                        /* '          */
  g.0APOST2 = '7D7D'x                      /* ''         */
  g.0AMP    = '50'x                        /* &          */
  g.0AMP2   = '5050'x                      /* &&         */
  g.0LINE = 0

  g.0QUEUE.0 = 0   /* Number of QUEUEd JAM statements    */
  g.0INC     = 0   /* Next DDNAME suffix to include       */
  g.0INCLO   = 0   /* Index of next include line          */
  g.0INCHI   = -1  /* Index of last include line          */
  g.0SEQ     = 0
  if g.0ZOS
  then g.0SYSNAME = mvsvar('SYSNAME')
  g.0QUIT    = 0   /* QUIT command seen                   */
  g.0CANCEL  = 0   /* QUIT CANCEL command seen            */
  g.0MACRUN  = 0   /* Not executing a macro at the moment */
  g.0MACDEF  = 0   /* Not defining a macro at the moment  */
  g.0MACLINE = 0   /* Line number within current macro    */
  g.0OPTIONS = ''  /* List of all option names ever seen  */
  g.0OPTIONS.0 = 0 /* Current options stack index         */
  g.0SOCKETS = 0   /* TCP/IP sockets not initialised      */

  /*                           -----border--------  ---comment---                */
  /*            name    width  first   bleft  bfill bright  left   right   last  */
  call setStyle 'asm'  ,71    ,'*'    ,'*'   ,'-'  ,'*'    ,'*'   ,'*'    ,'*'
  call setStyle 'box'  ,71    ,''     ,'**'  ,'*'  ,'**'   ,'**'  ,'**'   ,''
  call setStyle 'c'    ,80    ,'//'   ,'//'  ,'-'  ,''     ,'//'  ,''     ,'//'
  call setStyle 'js'   ,80    ,'//'   ,'//'  ,'-'  ,''     ,'//'  ,''     ,'//'
  call setStyle 'rexx' ,80    ,'/*'   ,' *'  ,'-'  ,''     ,' *'  ,''     ,'*/'
  call setStyle 'xml'  ,80    ,'<!--' ,''    ,''   ,''     ,''    ,''     ,'-->'
  call setStyle 'jcl'  ,71    ,'//*'  ,'//*' ,'-'  ,'*'    ,'//*' ,'*'    ,'//*'
  /* The jcl style is defined last to make it active initially */


  call 'jamsite'  /* Retrieve local site customisation data */
  g.0INCLO = 1
  g.0INCHI = 0
  do i = 1 to queued()
    parse pull sLine
    g.0INC.i = sLine
    g.0INCHI = i
  end

  member     = ''
  dataset    = ''
  g.0WHEN    = 0
  call initStack
  g.0EMIT = 1
  call pushStack 'FILE' g.0EMIT
  if g.0ZOS
  then 'SUBCOM ISREDIT'
  else rc = 1
  g.0EDITENV = rc = 0
  if g.0EDITENV
  then do /* can use ISPF EDIT */
    address ISREDIT 'MACRO (sArgs)'
    if rc <> 0 /* if not already editing a file */
    then do    /* then edit a temporary file */
      sTempFile = getTempFileName()'E'
      call quietly 'ALLOCATE FILE(OUT) DATASET('sTempFile')',
                   'RECFM(V B) BLKSIZE(27990) LRECL(259)',
                   'SPACE(1,1) TRACKS REUSE'
      address ISPEXEC 'EDIT DATASET('sTempFile') MACRO(JAMEDIT)'
      call quietly 'DELETE' sTempFile
      exit
    end
    address ISREDIT '(member) = MEMBER'   /* Member currently being edited */
    address ISREDIT '(dataset) = DATASET' /* Dataset currently being edited */
    address ISPEXEC 'CONTROL ERRORS RETURN'
    g.0LINE = 0
    g.0TEMPDSN = getTempFileName()
    call quietly 'ALLOCATE FILE(OUT) DATASET('g.0TEMPDSN')',
                 'RECFM(V B) BLKSIZE(27920) LRECL(259)',
                 'SPACE(1,1) CYLINDERS REUSE'
  end


  userid   = userid()
  user     = userid
  u        = userid
  prog     = getProgrammer()
  username = getUserName()
  me       = getInitials()
  firstname = getCamelCase(sFirstName)
  lastname  = getCamelCase(sLastName)
  step     = 0
  job      = 0
  call pushOptions
return

setOption:
  parse arg sKey sValue
  arg sKey .
  interpret sKey '=' sValue
  if wordpos(sKey,g.0OPTIONS) = 0
  then g.0OPTIONS = g.0OPTIONS sKey
return

pushOptions:
  _n = n
  _i = i
  n = g.0OPTIONS.0 + 1
  g.0OPTIONS.0 = n
  do i = 1 to words(g.0OPTIONS)
    sKey = word(g.0OPTIONS,i)
    bValue = value(sKey) = 1 /* force to be either 0 or 1 */
    g.0OPTIONS.n = g.0OPTIONS.n sKey'='bValue';'
  end
  n = _n
  i = _i
return

popOptions:
  _n = n
  n = g.0OPTIONS.0
  if n > 0
  then do
    interpret g.0OPTIONS.n /* k1=v1; k2=v2; ... */
    g.0OPTIONS.n = ''
    g.0OPTIONS.0 = n - 1
  end
  n = _n
return

quietly: procedure expose g.
  parse arg sCommand
  if g.0ZOS
  then do
    rc = outtrap('o.')
    address TSO sCommand
    g.0RC = rc
    rc = outtrap('off')
  end
  else do
    sCommand
  end
return

/*
 *---------------------------------------------------------------------
 * Stack functions
 *---------------------------------------------------------------------
*/

initStack: procedure expose g.
  g.0T = 0              /* set top of stack index     */
return

pushStack: procedure expose g.
  parse arg item
/*say left('',2*g.0T) 'push' item */
  tos = g.0T + 1        /* get new top of stack index */
  g.0E.tos = item       /* set new top of stack item  */
  g.0T = tos            /* set new top of stack index */
return

popStack: procedure expose g.
  tos = g.0T            /* get top of stack index     */
  item = g.0E.tos       /* get item at top of stack   */
  g.0T = max(tos-1,1)
/*say left('',2*g.0T) 'pop ' item */
return item

peekStack: procedure expose g.
  tos = g.0T            /* get top of stack index */
  item = g.0E.tos       /* get item at top of stack */
/*say left('',2*g.0T) 'peek' item */
return item

dumpStack: procedure expose g.
  do i = 1 to g.0T      /* get top of stack index */
    say left('',2*g.0T) 'item' i g.0E.i
  end
return item

getStackSize: procedure expose g.
return g.0T

L: procedure
  /* Return 4 byte integer from the specified memory address */
  arg nAddr
return c2d(storage(d2x(nAddr),4))

MVC: procedure
  /* Return n bytes from the specified memory address */
  arg nAddr,nLength
return storage(d2x(nAddr),nLength)

/*
 *---------------------------------------------------------------------
 * Helper functions
 *---------------------------------------------------------------------
*/
getProgrammer: programmer: procedure expose g.
  /* z/OS only: */
  if g.0ZOS
  then do
    pPSA     = 0                   /* -> Prefix Symbol Area            */
    pTCB     = L(pPSA+540)         /* -> Task Control Block            */
    pJSCB    = L(pTCB+180)         /* -> Job/Step Control Block        */
    pJCT     = L(pJSCB+260)        /* -> Job Control Table             */
    pACT     = c2d(MVC(pJCT+56,3)) /* -> Automatic Command Table       */
    sSYSPGMR = MVC(pACT+24,20)     /* Programmer Name field (SYSPGMR)  */
    sSYSPGMR = strip(sSYSPGMR)
  end
  else sSYSPGMR = ''
return sSYSPGMR

getStep: procedure expose step
  parse arg sExecArgs
  step = step + 1
return '//'left('STEP'step,7) 'EXEC' sExecArgs

getUserName: username: procedure expose g.
  /* z/OS only: */
  if g.0ZOS
  then do
    pPSA     = 0                   /* -> Prefix Symbol Area            */
    pASCB    = L(pPSA+548)         /* -> Home ASCB                     */
    pASXB    = L(pASCB+108)        /* -> ASCB Extension                */
    pACEE    = L(pASXB+200)        /* -> ACEE                          */
    pACCEUNAM = L(pACEE+100)       /* -> AL1(len),CL(len)'username'    */
    nUserName = c2d(MVC(pACCEUNAM,1)) - 1    /* Length of username     */
    sUserName = MVC(pACCEUNAM+1, nUserName)  /* User name              */
  end
  else sUserName = ''
return sUserName

getTempFileName: procedure expose g.
  sYYMMDD = substr(date('STANDARD'),3)
  sHHMMSS = space(translate(time(),,':'),0)
return 'JAM.D'sYYMMDD'.T'sHHMMSS'.S'getSeq(3,'0')

getHostByName: procedure expose g.
  parse arg sName
  if g.0ZOS
  then sAddr = Socket('GetHostByName',sName)
  else sAddr = sName
return sAddr

getAddr: procedure expose g.
  parse arg sAddr
  if g.0ZOS
  then sName = Socket('GetHostByAddr',sAddr)
  else sName = sAddr
return sName

getHost: procedure expose g. host.
  parse arg sAddrOrName
  if g.0ZOS
  then do /* Resolve the host name from the given address or name */
    sAddrAndName = Socket('Resolve',sAddrOrName)
    sHost = word(sAddrAndName,2)
    if g.0SOCKETRC = 0
    then sHost = toLower(sHost)
    else sHost = word(sAddrAndName,1)
  end
  else do /* Get the host name for this system alias */
    if host.sAddrOrName = ''
    then sHost = sAddrOrName
    else sHost = host.sAddrOrName
  end
return sHost

getCamelCase: camelcase: procedure
  parse arg sName
  sName = toLower(sName)
  sFirstLetter = left(sName,1)
return toUpper(sFirstLetter)substr(sName,2)

getDirSize: procedure expose st_size st_type s_isdir s_isreg
  /* z/OS only: */
  parse arg sPath
  nTotal = 0
/*say 'getDirSize('sPath')'  */
  address SYSCALL 'readdir (sPath) dir. stat.'
  do i = 3 to dir.0
/*  say '  'sPath'/'dir.i    */
    select
      when stat.i.st_type = s_isdir then do
        nTotal = nTotal + getDirSize(sPath'/'dir.i)
      end
      when stat.i.st_type = s_isreg then do
        nBytes = stat.i.st_size
        if right(nBytes,1) = 'M'
        then nBytes = strip(nBytes,'TRAILING','M') * 1024 * 1024
        nTotal = nTotal + nBytes
      end
      otherwise nop /* Ignore symlinks and special files */
    end
  end
return nTotal

getInitials: initials:
  parse upper var username sLastName','sFirstName
  sFirstName = strip(sFirstName)
  sLastName = strip(sLastName)
return left(sFirstName,1)left(sLastName,1)

getSeq: procedure expose g.
  parse arg nWidth,sPad
  g.0SEQ = g.0SEQ + 1
  if datatype(nWidth,'WHOLE')
  then nSeqNo = right(g.0SEQ,nWidth,left(sPad,1,'0'))
  else nSeqNo = g.0SEQ
return nSeqNo

hasMember: procedure
  parse arg sDataset'('sMember')'
return isMemberName(sMember)

inRange: procedure
  arg n,lo,hi
return datatype(n,'WHOLE') & n >= lo & n <= hi

inSet: procedure
  parse arg sElement,sSet
return wordpos(sElement,sSet) > 0

intersect: procedure
  parse arg sSet1,sSet2
  sSet = ''
  do i = 1 to words(sSet2)
    sWord = word(sSet2,i)
    if wordpos(sWord,sSet1) > 0
    then sSet = sSet sWord
  end
return strip(sSet,'BOTH')

isASCII: procedure expose g.
  parse arg sData
return verify(sData,g.0ASCII,'NOMATCH') = 0

isDatasetName: procedure
  parse upper arg sFile 0 sDataset'('sMem')'sRest
  if sRest <> '' then return 0
  if sDataset = '' then return 0
  if length(sDataset) > 44 then return 0
  do while sDataset <> ''
    parse var sDataset sToken'.'sDataset
    nToken = length(sToken)
    if nToken = 0 then return 0
    if nToken > 8 then return 0
    if pos(left(sToken,1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$') = 0
    then return 0
    if verify(sToken,'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$0123456789','NOMATCH') > 0
    then return 0
  end
  nLP = pos('(',sFile)
  nRP = pos(')',sFile)
  if nLP = 0 & nRP = 0 then return 1 /* No member specified */
  if nLP > 0 & nRP > 0 & nLP < nRP
  then return isMemberName(sMem)
return 0

isDDName: procedure
  parse upper arg sFile 0 'DD:'sDD sRest
  if sRest <> '' then return 0
  nDD = length(sDD)
  if nDD = 0 then return 0
  if nDD > 8 then return 0
  sFirst = left(sDD,1)
  if verify(sFirst,'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$','NOMATCH') > 0
  then return 0
return verify(sDD,'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$0123456789','NOMATCH') = 0

isHex: procedure expose g.
  parse arg xData
return xData \= '' & datatype(xData,'X')

isIPAddr: procedure expose g.
  parse arg d1'.'d2'.'d3'.'d4 sRemainder
  if sRemainder <> ''      then return 0
  if \datatype(d1,'WHOLE') then return 0
  if \datatype(d2,'WHOLE') then return 0
  if \datatype(d3,'WHOLE') then return 0
  if \datatype(d4,'WHOLE') then return 0
  if d1 < 0 | d1 > 255     then return 0
  if d2 < 0 | d2 > 255     then return 0
  if d3 < 0 | d3 > 255     then return 0
  if d4 < 0 | d4 > 255     then return 0
return 1

isMemberName: procedure
  parse arg sMem
  nMem = length(sMem)
  if nMem = 0 then return 0
  if nMem > 8 then return 0
  if pos(left(sMem,1),'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$') = 0
  then return 0
return verify(sMem,'ABCDEFGHIJKLMNOPQRSTUVWXYZ@#$0123456789','NOMATCH') = 0

isNum: procedure
  parse arg nData
return datatype(nData,'WHOLE')

isPath: procedure
  parse arg sDataset
  if pos('/',sDataset) > 0 then return 1
return 0

isText: procedure expose g.
  parse arg sData
return verify(sData,g.0EBCDIC,'NOMATCH') = 0

quote: procedure expose g.
  parse arg s
  if pos(g.0APOST,s) > 0 then s = replace(g.0APOST,g.0APOST2,s)
  if pos(g.0AMP,s) > 0 then s = replace(g.0AMP,g.0AMP2,s)
  if \g.0EBCDIC_ENVIRONMENT then s = toASCII(s)
return "'"s"'"

range: procedure
  parse arg nFrom,nTo,nSpace,sFill
  if \isNum(nSpace)
  then nSpace = 1
  sText = ''
  select
    when isNum(nFrom) & isNum(nTo) then do
      if nFrom > nTo
      then do i = nFrom to nTo by -1
        sText = sText i
      end
      else do i = nFrom to nTo
        sText = sText i
      end
    end
    otherwise nop
  end
return space(sText,nSpace,left(sFill,1))

replace: procedure
  parse arg sFrom,sTo,sText
  nTo = length(sTo)
  nFrom = pos(sFrom,sText)
  do while nFrom > 0
    sText = delstr(sText,nFrom,length(sFrom))
    sText = insert(sTo,sText,nFrom-1)
    nFrom = pos(sFrom,sText,nFrom+nTo)
  end
return sText

sort: procedure expose (stem) sorted. g.
  /* Perform a quick sort without modifying the passed stem.
     Instead, return a fixed "sorted." stem containing indexes
     into the passed stem that, if traversed, would access the
     passed stem in the desired order.
     For example:
       in.0 = 3      -->   sorted.0 = 3 (number of items)
       in.1 = 'the'        sorted.1 = 2 (cat)
       in.2 = 'cat'        sorted.2 = 3 (sat)
       in.3 = 'sat'        sorted.3 = 1 (the)
  */
  parse arg array,ascending
  ascending = (ascending <> 0)
  bAlreadySorted = 1
  /* Initialise sorted. array indexes */
  sorted.0 = value(array'0') /* Number of items to be sorted */
  do i = 1 to sorted.0
    sorted.i = i
  end
  /* Push 1 and number of items onto stack  */
  s = 1               /* Stack pointer      */
  L.1 = 1             /* Left window bound  */
  R.1 = sorted.0      /* Right window bound */
  do while s <> 0
    /* Pop L and R from stack */
    L = L.s
    R = R.s
    s = s - 1
    do until L >= R
      i = L
      j = R
      mid = (L + R) % 2
      middleItem = value(array||sorted.mid)
      do until i > j
        if ascending
        then do
          do while value(array||sorted.i) < middleItem
            i = i + 1
          end
          do while middleItem < value(array||sorted.j)
            j = j - 1
          end
        end
        else do
          do while value(array||sorted.i) > middleItem
            i = i + 1
          end
          do while middleItem > value(array||sorted.j)
            j = j - 1
          end
        end
        if i <= j
        then do /* Swap i and j items */
          bAlreadySorted = 0
          p = sorted.i
          sorted.i = sorted.j
          sorted.j = p
          i = i + 1
          j = j - 1
        end
      end
      if i < R
      then do /* Push i and R onto stack */
        s = s + 1
        L.s = i
        R.s = R
      end
      R = j
    end
  end
return bAlreadySorted

sortStem:
  parse arg stem,ascending
return sort(stem,ascending)

sortWords: procedure
  parse arg sWords,bAscending
  array.0 = words(sWords)
  do i = 1 to array.0
    array.i = word(sWords,i)
  end
  call sortStem 'array.',bAscending
  sSorted = ''
  do i = 1 to sorted.0
    n = sorted.i
    sSorted = sSorted word(sWords,n)
  end
return strip(sSorted)

split: procedure
  parse arg sFile,sPathSep,sExtSep
  nPathSep = lastpos(sPathSep,sFile)
  if nPathSep = 0
  then do
    sPath = ''
    sRest = sFile
  end
  else do
    sPath = substr(sFile,1,nPathSep)
    sRest = substr(sFile,nPathSep+1)
  end
  nExtSep = lastpos(sExtSep,sRest)
  if nExtSep = 0
  then do
    sFileName = sRest
    sExt = ''
  end
  else do
    sFileName = substr(sRest,1,nExtSep-1)
    sExt = substr(sRest,nExtSep)
  end
return sPath || 'ff'x || sFilename || 'ff'x || sExt

toArray:
  parse arg __text,__stem,__delim
  if __stem = ''
  then __stem = 'a'
  else __stem = strip(__stem,'TRAILING','.')
  if __delim = ''
  then do
    __size = words(__text)
    do __i = 1 to __size
      interpret __stem'.'__i '= "'word(__text,__i)'"'
    end
  end
  else do
    __size = 0
    do until length(__text) = 0
      parse var __text __element (__delim) __text
      __size = __size + 1
      interpret __stem'.'__size '= __element'
    end
  end
  interpret __stem'.0 =' __size
return __size

toASCII: procedure expose g.
  parse arg s
return translate(s,g.0ASCII,g.0EBCDIC)

toBlock:
  /* reblocks text into an array of no more than n chars per line */
  parse arg __text,__stem,__maxlen
  __size = 0
  __line = ''
  if \datatype(__maxlen,'WHOLE')
  then __maxlen = 72
  if __stem = ''
  then __stem = 'a'
  else __stem = strip(__stem,'TRAILING','.')
  do __i = 1 to words(__text)
    __word = word(__text,__i)
    if length(__line __word) > __maxlen
    then do
      __size = __size + 1
      interpret __stem'.'__size '= "'__line'"'
      __line = __word
    end
    else __line = space(__line __word)
  end
  if __line <> '' /* any residual words? */
  then do
    __size = __size + 1
    interpret __stem'.'__size '= "'__line'"'
  end
  interpret __stem'.0 =' __size
return __size

toEBCDIC: procedure expose g.
  parse arg s
return translate(s,g.0EBCDIC,g.0ASCII)

toLower: procedure
  parse upper arg sString
  sString = translate(sString,,
                      'abcdefghijklmnopqrstuvwxyz',,
                      'ABCDEFGHIJKLMNOPQRSTUVWXYZ')
return sString

toStr: procedure
  /* Remove surrounding single quotes (if any) from a string, and duplicate
     all remaining single quotes (e.g. 'You can't' becomes: You can''t)
  */
  parse arg sLine
  if length(sLine) >= 2 & left(sLine,1) = "'" & right(sLine,1) = "'"
  then sLine = substr(sLine,2,length(sLine)-2) /* remove enclosing quotes */
  if pos("'",sLine) > 0
  then do
    sLine = translate(sLine,'ff'x,"'") /* convert quotes to hard quotes */
    do while pos('ff'x,sLine) > 0
      parse var sLine sLeft 'ff'x sRight
      sLine = sLeft"''"sRight          /* convert hard quotes to ''     */
    end
  end
return sLine

toString:
  /* Convert a REXX stem to a string */
  parse arg __stem .
  __string = ''
  interpret '__size =' __stem'.0'
  if datatype(__size,'WHOLE') & __size > 0
  then do __i = 1 to __size
    interpret '__word =' __stem'.'__i
    __string = __string __word
  end
return strip(__string)

toUpper: procedure
  parse upper arg sString
return sString

union: procedure
  parse arg sSet1,sSet2
  do i = 1 to words(sSet2)
    sWord = word(sSet2,i)
    if wordpos(sWord,sSet1) = 0
    then sSet1 = sSet1 sWord
  end
return strip(sSet1,'BOTH')

/*
 *---------------------------------------------------------------------
 * Termination
 *---------------------------------------------------------------------
*/

Epilog:
/*call dumpStack */
  if g.0ZOS
  then do
    'EXECIO 0 DISKW OUT (FINIS'
  end
  else do
    if sFileOut = ''
    then sFileOut = sPathOut || sFileNameOut || sExtOut
    call emitFile sFileOut,'output'
  end
  do i = 2 to getStackSize()
    parse value popStack() with sClause . '/'sStatement
/*  say 'epilog' sClause sStatement */
    say 'JAM004E' sClause 'not terminated by END:' strip(sStatement)
  end
  if g.0EDITENV
  then do
    call quietly 'FREE FILE(OUT)'
    if \g.0CANCEL
    then address ISPEXEC 'EDIT DATASET('g.0TEMPDSN')'
    call quietly 'DELETE' g.0TEMPDSN
  end
return

/*
 *---------------------------------------------------------------------
 * z/OS TCP/IP Sockets functions
 *---------------------------------------------------------------------
*/
Socket: procedure expose g.
  parse arg a,c,d,e,f,g,h,i,j,k
  if \g.0SOCKETS
  then do
    parse value 'SOCKET'('Terminate') with g.0SOCKETRC sResp
    parse value 'SOCKET'('Initialize','MySet') with g.0SOCKETRC sResp
    g.0SOCKETS = 1
  end
  parse value 'SOCKET'(a,c,d,e,f,g,h,i,j,k) with g.0SOCKETRC sResp
return sResp

eof: