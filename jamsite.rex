/*REXX*****************************************************************
**                                                                   **
** NAME     - JAMSITE                                                **
**                                                                   **
** TITLE    - JAM LOCAL SITE CONFIGURATION DATA                      **
**                                                                   **
** VERSION  - 1.00                                                   **
**                                                                   **
** FUNCTION - Sets REXX variables that are customised for the local  **
**            site. These variables are defined by queueing JAM      **
**            statements that will be processed by the JAM processor **
**            before processing any other JAM input statements.      **
**                                                                   **
**                                                                   **
** NOTES    - 1. This is implemented as a REXX procedure so that     **
**               local site configuration data can be stored in a    **
**               separate file rather than having to update the JAM  **
**               REXX procedure or manually nominate a configuration **
**               file or dataset name at run time.                   **
**                                                                   **
** HISTORY  - Date     By  Reason (most recent at the top please)    **
**            -------- --- ----------------------------------------- **
**            20201207 AJA Initial version                           **
**                                                                   **
**********************************************************************/

  do k = 1 until sourceline(k) = 'BEGIN-LOCAL-SITE-DATA'
  end
  do k = k+1 while sourceline(k) \= 'END-LOCAL-SITE-DATA'
    sLine = sourceline(k)
    if left(sLine,3) \= '...'
    then queue sLine /* Queue JAM statement to be processed later */
  end
return

/*
BEGIN-LOCAL-SITE-DATA
...
... Set your default configuration file name:
...
..if [g.0ZOS]
..  set jam_config = 'SYS1.JAM.CNTL'
..  set jam_lpars  = 'SYS1.JAM.CNTL(LPARS)'
..else
..  set jam_config = './'
..  set jam_lpars  = './lpars'
..end
...
... Set your options that have special meaning to the JAM processor
...
..option nobinary   -
..         blanks   -
..         comments -
..       nodebug    -
..       nohold     -
..         ibm1047  -
..       noquiet    -
..         run      -
..       notrunc    -
..       nouseftp   -
..       noverbose
...
... Load the details about your LPARs from a table
...
... The "map" JAM verb is used to read the lpars table and map the columns 
... in it for each LPAR alias. Once this is done the value of each cell is
... available in REXX variables called "x.y" where x is the column name
... and y is the first column value ("alias" in this case). For example,
... the JES node name for the TST1 system alias can be obtained by using:
...   ..set myalias = 'TST1'
...   [jesname.myalias]
... or, if the tst1 REXX variable has not been assigned a value yet, simply:
...   [jesname.tst1]
...
... set alias. = ''
..map [jam_lpars]
...
... Enable '..job [sysname]' to work the same as '..job [alias]'
...
..macro define alt #
..  set _alias   = alias.#
..  set _sysname = sysname._alias
..  set alias._sysname = _alias
..  set alias._alias   = _alias
..macro end
..for [alias.0] macro alt
END-LOCAL-SITE-DATA

The lpars file must contain at least the following column names - in any order
except for 'alias' which must be the first column:

alias sysname jc mc sysclone sysplex jesname jesnode njenet cat tags host

...where:

column   | description
-------- | ----------------------------------------------------------------
alias    | The user-defined alias for this LPAR
cat      | The master catalog dataset name for this LPAR
host     | The TCPIP host name of this LPAR
jc       | The default job class letter to be used when submitting JCL
jesname  | The name of this LPAR's JES2 system
jesnode  | The node number of this LPAR's JES2 system
mc       | The default job message class letter
njenet   | The user-defined NJE network name of which this LPAR is a member
sysclone | The z/OS sysclone suffix for this LPAR
sysname  | The z/OS sysname (smfid) of this LPAR
sysplex  | The sysplex name of which this LPAR is a member
tags     | User-defined tags associated with the LPAR (e.g. PROD, TEST etc)

The njenet column is used to determine whether JCL can be submitted via NJE
from one LPAR to another LPAR. If LPARs share the same njenet value then 
it is assumed that they have NJE connectivity. The njenet value can be any
user-defined string and is not related to any z/OS configuration value.
If no NJE connectivity exists, then FTP will be used to submit JCL (assuming
that there is TCPIP connectivity). The use of FTP instead of NJE can be forced
by setting the "useftp" option: `..option useftp` (or `..set useftp = 1`).

You can add your own columns, but the above are the minimum required for JAM
to be useful on z/OS.

*/

