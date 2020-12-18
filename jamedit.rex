/*REXX*****************************************************************
**                                                                   **
** NAME     - JAMEDIT                                                **
**                                                                   **
** TITLE    - JAM PROCESSOR ISPF/EDIT SETUP MACRO                    **
**                                                                   **
** FUNCTION - The JAM processor invokes this edit macro when the user**
**            invokes JAM when not already editing a file. It inserts**
**            a "..job" JAM statement and a hint of how to get more  **
**            help on how to have a JAM session.                     **
**                                                                   **
**********************************************************************/
trace off
  address ISREDIT
  'MACRO (sParms)'
  'NUMBER OFF'
  'CAPS OFF'
  'JAMINIT'
  'LINE_AFTER 0 = "..job"'
  'LINE_AFTER 1 = "... Type JAM to convert this to JCL."'
  'LINE_AFTER 2 = "... For all help, use: ..help"'
  'LINE_AFTER 3 = "... For statement help, use (for example): ..job ?"'
  'INSERT 4 100'
exit