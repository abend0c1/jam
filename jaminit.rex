/*REXX*****************************************************************
**                                                                   **
** NAME     - JAMINIT                                                **
**                                                                   **
** TITLE    - JAM PROCESSOR ISPF/EDIT INITIAL MACRO                  **
**                                                                   **
** FUNCTION - This is an ISPF/EDIT macro which will automatically    **
**            invoke the JAM processor if a '..auto' JAM statement   **
**            is found on line 1 of the file being edited. This is   **
**            highly recommended as it is very low overhead and      **
**            allows you to process a JAM file simply by editing it. **
**                                                                   **
**            You can set this as your initial edit macro by issuing:**
**                                                                   **
**            IMACRO JAMINIT                                         **
**                                                                   **
**            You can remove the initial macro by issuing:           **
**                                                                   **
**            IMACRO NONE                                            **
**                                                                   **
**********************************************************************/
trace off
  address ISREDIT
  'MACRO (sParms)'

  /* Insert any user-supplied edit commands here... */
  'DEFINE E ALIAS EDIT'
  'HILITE AUTO'

  /* Invoke the JAM processor if "..auto" is found on line 1 */
  '(sFirstLine) = LINE 1'       /* Get first line into REXX variable */
  if left(sFirstLine,2) = '..'
  then do
    parse upper var sFirstLine '..'sJAMStmt .
    if strip(sJAMStmt) = 'AUTO' /* If "..auto msg" or ".. auto msg"  */
    then 'JAM' sParms           /* Then run the JAM processor        */
  end                           /* Else user can manually type JAM   */
exit