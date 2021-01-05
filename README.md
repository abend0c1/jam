# JAM - Just Another Macro language for z/OS

## TABLE OF CONTENTS

- [Overview](#OVERVIEW)
- [Prerequisites](#PREREQUISITES)
- [Installation](#INSTALLATION)
- [How JAM Works](#HOW-JAM-WORKS)
- [Example](#EXAMPLE)
- [Command Syntax](#COMMAND-SYNTAX)
  - [On z/OS only](#On-zOS-only)
  - [On Linux or Windows](#On-Linux-or-Windows)
- [List of JAM Verbs](#List-of-JAM-verbs)
- [Usage](#USAGE)
  - [Defining JAM variables](#Defining-JAM-variables)
  - [Using JAM variables](#Using-JAM-variables)
  - [Pre-defined variables](#Pre-defined-variables)
  - [Built-in functions](#Built-in-functions)
  - [How to use JAM in ISPF/EDIT](#How-to-use-JAM-in-ISPF/EDIT)
  - [How to use JAM in BATCH](#How-to-use-JAM-in-BATCH)


## OVERVIEW
JAM is a z/OS productivity tool that transforms an input file, containing a 
mixture of JAM statements and ordinary text, to an output file containing the 
transformed content. 

JAM statements are those lines that have `..` in columns 1 
and 2 and comprise a verb and operands. For example, 

    ..say Hello, World!

In this case, the JAM verb is `say`, the operands are `Hello, World!` and the result is to 
bore the user with an inane message.

Although JAM was initially intended to produce JCL from simple input commands, 
it does not have to be used solely to generate JCL. It can be used to generate 
configuration files, XML files, JSON files, YAML files, CSV files, simple columnar tables, 
emails, or anything that is textual. 

If you want to add functionality to JAM all you need to do is add a REXX subroutine called 
doXXX where XXX is the name of the JAM verb you would like to implement. 
All the I/O is done for you, so you can focus on the transformation logic.

JAM is a **one-pass interpreter**, so it does not support looping constructs per se.
However it does support some loop-like constructs (for example, `..for 1 to 10 macro sayHello`) 
that are realised in a manner akin to laying down bricks on the fly before walking on them. 
Because of this, large loops can be a little slow but in practice that is not normally an issue. 

## PREREQUISITES

1. To run JAM on z/OS you should:
    * Store the JAM, JAMINIT and JAMEDIT rexx procedures in a RECFM=VB LRECL=255 
       dataset in your SYSEXEC concatenation
    * Assign PF4 to "JAM" using the KEYS command in ISPF/EDIT
    * Assign PF4 to "TSO JAM" using the KEYS command in other ISPF environments

    Assigning PF4 is optional but highly recommended as it will improve your
    work flow.

1. To run JAM on Linux or windows, you will need to install a REXX interpreter 
   such as:
    * Regina REXX      (http://regina-rexx.sourceforge.net)
    * Open Object REXX (http://www.oorexx.org/)

    On Linux, there is usually a REXX package that you can install using your 
    package manager. On Ubuntu you can install it by issuing:

        sudo apt install regina-rexx

    The REXX interpreter can then be invoked in a command window by issuing, for example:
    
        rexx jam.rex recipe.jam jamtart.txt


## INSTALLATION

1. Install the following REXX files in your REXX library (or execution path):

   | File    | Description | Operating system
   | ----    | ----------- | -----
   | JAM     | The main JAM processor | z/OS, Linux, Windows
   | JAMEDIT | An ISPF/EDIT macro that is run when the JAM processor is invoked from TSO. It will populate a temporary JAM file with a `..job` verb and some brief guidance on how to get help about JAM verbs. | z/OS only
   | JAMINIT | An ISPF/EDIT initial macro. This will cause the JAM processor to run automatically when you edit a JAM file containing `..auto` on the first line. | z/OS Only
   | JAMSITE | Contains JAM statements that configure local site options (including the name of your JAM configuration library) | z/OS, Linux, Windows


2. Install the following files in your JAM configuration library:

   | File    | Description
   | ----    | -----------
   | LPARS   |  A table of system-related information (one row for each of your LPARs).

3. Customise the sample LPARS file for your site. It contains the following fictitious system definitions as a guide:

        alias sysname jc mc sysclone sysplex  jesname jesnode njenet cat             tags       host
        TST1  R2D2    A  X  T1       TESTPLEX JESTST1 N11     NJEA   SYS1.MASTER.CAT 'DEV TEST' r2d2.example.org
        TST2  R2D3    A  X  T2       TESTPLEX JESTST2 N12     NJEA   SYS1.MASTER.CAT 'UAT'      r2d3.example.org  
        PRD1  C3P0    A  X  P1       PRODPLEX JESPRD1 N21     NJEB   SYS1.MASTER.CAT 'PROD'     c3p0.example.org  
        PRD2  C3P1    A  X  P1       PRODPLEX JESPRD2 N22     NJEB   SYS1.MASTER.CAT 'PROD DR'  c3p1.example.org

    The columns can be in any order except for the first column which **must** be the system alias that you have
    assigned to each LPAR. The remaining columns are the minimum required to make JAM useful on z/OS sites. 
    You can add your own columns if you wish. This table is "mapped" (i.e. loaded into REXX variables) by 
    the JAMSITE REXX procedure. The cell values can be accessed by REXX stem variables indexed by
    the system alias, for example: [`sysname.TST1`] will resolve to `R2D2`. If any cell needs to have a null
    value then you can either code `''` or (in my opinion) a slightly cleaner looking `.` to represent
    the missing value - whichever you prefer.


## HOW JAM WORKS

JAM scans the input file looking for two things:
- `..` in columns 1 and 2 which indicate JAM verbs to be processed - some of which may generate multiple output lines.
- REXX expressions surrounded by square brackets (e.g. `[date()]`). JAM replaces each instance (including the brackets) with the evaluated expression.

Any other input is simply copied to the output file as-is.

In more detail, it performs the following types of transformation on the input file content:

1. JAM performs REXX expression substitution. Anything specified between square
   brackets is passed to REXX for evaluation. It doesn't matter which square
   brackets you use on z/OS - both code page IBM037 and IBM1047 work the same way.

   The beauty of using square brackets to enclose REXX expressions on z/OS is that 
   mainframers have traditionally shunned them because they did not 
   exist on the original IBM 3270 Display System keyboards, so the chance of 
   clashing with existing JCL is minimal.

   For example,

        ..set name = 'Andrew'
        ..say Hey [name], your lucky number is [random(1,99)]

    displays a random number on the user's terminal:

        Hey Andrew, your lucky number is 13

    Note that if you put an invalid expression between square brackets (e.g. 
    `[1/0]`) then the JAM processor will die after issuing a diagnostic message. 
    To avoid premature death, you should perform your own input validation. 
    There are a  number of [JAM built-in functions](#built-in-functions)
    that you can invoke to help you accomplish that.


1. JAM interacts with the user via the terminal (yes, in a JAM session) using the 
   `..ask`, `..asku`, `..askq`, `..askqu` and `..say` verbs. For example,

        ..set name = 'Andrew'
        ..askqu lucky_number 7 Hey [name], what's your lucky number?
        ..say So your lucky number is [lucky_number]?

    prompts the user for a number, offering a default if they reply with a null 
    line, and assigns the user's response (converted to upper case) to a REXX 
    variable called "lucky_number". 

        Hey Andrew, what's your lucky number? (7):
        three
        So your lucky number is THREE?

    If the user had pressed Enter, the default lucky number (7) would be assigned
    to the "lucky_number" REXX variable.

    If the user had replied with "q" (for quit) then the JAM processor would 
    have exited immediately. This is an easy way for the user to abort a 
    JAM session. If you don't want this escape mechanism then you could use the 
    `..asku` JAM verb, and if you don't want the conversion to upper case 
    then you could use the `..ask` JAM verb. And finally, you could use the
    `..askq` verb to avoid case conversion but still allow the user to enter
    "q" to quit.

    To write a message to the user's terminal you use the `..say` verb.


1. JAM performs conditional transformation using the `..if`, `..else`, `..end` and 
   `..select`, `..when`, `..otherwise`, `..end` constructs. These should already be familiar
   to REXX programmers so the learning curve is minimal.
   
   For example,

        ..if [lucky_number = 13]
        ..  say That's my lucky number too!
        ..end

    Anything that is not written to the terminal is written to the output file,
    so if you coded:

        ..select [lucky_number]
        ..  when 4
        Four is not so lucky in some parts of the world
        ..  when 13
        That's my lucky number too!
        ..  otherwise
        I'm afraid, [lucky_number] is not lucky at all
        ..end

    then a message would be appended to the output file instead of
    being displayed on the user's terminal. This is a good way to conditionally 
    build output text files.

1. JAM can load table data from files using either the `..table` verb or the `...map` verb.

   This makes it easy to produce complex output from simple tabular input.

   - The `..table` verb stores each column in an array using the traditional REXX 
      convention of having the number of elements stored in `column.0` and each 
      element stored in `column.row` (where `row` is from `1` to `[column.0]`). 
      For example, assume you had a file called my.user.list containing: 
      
          userid  name         phone
          U001   'Don Juan'    555-1111
          U002   'Don Quixote' 555-2222
          U003   'Don McLean'  555-3333
    
        You can access the details of the second row by:

          ..table my.user.list
          [userid.2]'s name is [name.2] and phone is [phone.2]
          The number of rows in this table is [userid.0]
      
        and the output would be:

            U002's name is Don Quixote and phone is 555-2222
            The number of rows in this table is 3

      If row 1 of your table file does not contain column headings, then you must
      supply the column headings on the `..table` verb itself. For example,

          ..table my.user.list userid name phone

      Generally, it's better to have column headings in the table file because 
      the file then becomes self-documenting.

   - An alternative to accessing tabular data by row number is to use the `..map` verb to 
      map the data by key. When you map tabular data the value 
      in column 1 is assumed to be a unique key for that row, and each column can be 
      referenced by that row's key by using `[column.key]`.


      For example, using the above table again, the key column is 
      "userid" and the other columns are indexed by userid. You can now access U002's 
      details by name:

          ..map my.user.list
          U002's name is [name.U002] and phone is [phone.U002]
          The number of rows in this table is [userid.0]

        and the output will still be:

            U002's name is Don Quixote and phone is 555-2222
            The number of rows in this table is 3


1. JAM can transform some simple JAM statements into more complex JCL fragments. This provides a 
   productivity boost for z/OS fan boys because you never have to remember the 
   sometimes (mostly?) arcane JCL syntax for doing relatively simple things. 
   For example,

        ..copy my.dataset your.dataset

    appends the following job step to the output file in order to copy one dataset to another:

        //STEP1   EXEC PGM=ADRDSSU
        //SYSPRINT  DD SYSOUT=*
        //SYSIN     DD *
        COPY DATASET -
            ( -
                INCLUDE -
                ( -
                'MY.DATASET' -
                ) -
            ) -
            RENAMEUNCONDITIONAL -
            ( -
                ( -
                'MY.DATASET', -
                'YOUR.DATASET'-
                ) -
            ) -
            TOLERATE(ENQFAILURE)
        /*

    The particular z/OS utility program chosen to perform the copy function depends on the
    operands (and may not be your preferred utility, but all is not lost). For example, 
    
        ..copy ~/my.file ~/some/path/some.file
    
    would generate a BPXBATCH job step to issue the simple Unix `cp` 
    command. Yes, Unix got it right...simple *is* better!

    If you ever find that the canned JAM verbs do not produce the JCL that you
    want, then that is not a show stopper. You have some alternatives:
    - Use the `..include` JAM verb to include your beloved JCL into a JAM input 
      file and let JAM simply substitute the desired parameters in the appropriate 
      places.
    - Modify the JAM exec to create your own preferred JCL (this is free open source software afterall).
    - Create your own JAM verb by adding a `doXXX` procedure to the JAM exec 
      to do what you want. Submitting a git pull request afterwards would be nice but is not required.
    - Don't use JAM, but write your own &lt;insert language of your choice here&gt; program instead.
    - Don't use JAM, or write a program, but continue using the same bizarre JCL
      syntax you've been using since the 1970's (you are already using the best expensive closed source software afterall).

1. Certain JAM statements can be chained together by appending a comma (`,`).

    This is useful when, for example, you want to copy several datasets but
    don't want to create a single job step per copy operation. To do this you would 
    chain them together by coding:

        ..copy my.pds(mem1) your.pds,
        ..copy my.pds(mem2) your.pds,
        ..copy my.pds(mem3) your.pds
        
    which results in a single job step that performs all three operations containing
    (in a nutshell):

        COPY INDD=((DD1,R)),OUTDD=DD2
        SELECT MEMBER=MEM1
        SELECT MEMBER=MEM2
        SELECT MEMBER=MEM3

    Side note: the above can actually be done by a single JAM statement:

        ..copy my.pds(mem1,mem2,mem3) your.pds

1. JAM has a rather intuitive text justification feature that enables you to 
   generate tabular output easily. For example suppose you wanted to create
   a stock market report (yes, this is a thing that people get paid to do) 
   where the stock name is left-justified, the company name is centred and the 
   dollar value is right-justified. The following JAM input file (with 
   REXX stem variables appropriately set):

    q   NASDAQ      Company            Value
       [s.1 ] [     com.1     ] [  value.1]
       [s.2 ] [     com.2     ] [  value.2]
       [s.3 ] [     com.3     ] [  value.3]

    would produce output formatted like:

       NASDAQ      Company            Value
       MSFT   Microsoft Corpora     $210.39
       AAPL       Apple Inc         $117.34
       AMZN    Amazon.com, Inc.    $3099.40

    The text justification method depends on whether there is at least one leading 
    and/or trailing space around the REXX expression as follows:

    | Example   | Resulting justification |
    | --------- | ----------------------- |
    | ``[s ]``  | Left justified          |
    | ``[ s]``  | Right justified         |
    | ``[ s ]`` | Centred                 |
    | ``[s]``   | No justification        |
    
    
    When justification *is* used, the width of each output column equals the number of 
    characters bounded by the square brackets including the square brackets
    themselves. Any content longer than that width is truncated. When justification
    is not used, no truncation occurs and the width of the output cell is simply
    the width of the content.


1. JAM has a useful macro facility which can be used to encapsulate frequently 
   generated content. For example, suppose you wanted to concatenate varying 
   numbers of GDG datasets. You could do something like:

       ..askqu first_gdg_number -8 Enter starting GDG number
       ..askqu last_gdg_number   0 Enter ending GDG number
       ..askqu dsn MY.GDG Enter GDG dataset name
       ..macro define gendd #
       ..  if [# = first_gdg_number]
       //GDG       DD DISP=SHR,DSN=[dsn]([#])
       ..  else
       //          DD DISP=SHR,DSN=[dsn]([#])
       ..  end
       ..macro end
       ..for [first_gdg_number] to [last_gdg_number] macro gendd

   which, if the defaults were accepted by the user, would generate:

       //GDG       DD DISP=SHR,DSN=MY.GDG(-8)
       //          DD DISP=SHR,DSN=MY.GDG(-7)
       //          DD DISP=SHR,DSN=MY.GDG(-6)
       //          DD DISP=SHR,DSN=MY.GDG(-5)
       //          DD DISP=SHR,DSN=MY.GDG(-4)
       //          DD DISP=SHR,DSN=MY.GDG(-3)
       //          DD DISP=SHR,DSN=MY.GDG(-2)
       //          DD DISP=SHR,DSN=MY.GDG(-1)
       //          DD DISP=SHR,DSN=MY.GDG(0)

   Because JAM is a one-pass interpreter, you must define any macros before
   you use them. Sometimes it is cleaner to use `..include` to include any
   macro definitions that you want to use before you use them.

## EXAMPLE

An example JAM input file is:

<pre>
..auto Building JCL to copy a file
..askqu dsnin MY.INPUT.FILE Enter the input dataset name
..askqu dsnout MY.OUTPUT.FILE Enter the output dataset name
..if [dsnin = dsnout]
..  quit cancel You can't copy a file to itself
..else
..  say Creating JCL to copy [dsnin] to [dsnout]
..end
..runon test
..copy [dsnin] [dsnout]
..say Examine the JCL and submit when ready
</pre>

The meanings of the JAM statements are:

Statement | Meaning
---|---
<b style="color:red;">..auto</b>|When you Edit or View a JAM file in ISPF/EDIT, this verb causes the JAM processor to begin running soon as you edit or view the file. This requires a small ISPF/EDIT initial macro called JAMINIT to be configured by issuing the `IMACRO JAMINIT` command. JAMINIT will automatically invoke JAM if the first line of the input file being viewed or edited is an '..auto' JAM verb.
<b style="color:red;">..askqu</b>|This prompts the user for some input. It converts the response to uppercase and assigns it to the REXX variable "dsnin". If the user presses Enter, then the default input is "MY.INPUT.FILE". If the user responds with Q (i.e. quit) then the JAM processor exits.
<b style="color:red;">..if</b>|This is the classic if/then/else clause. The REXX expression inside the square brackets is evaluated to determine which block of statements is to be processed next. If the result is 1 the "then" clause is executed. If the result is 0 the "else" clause is executed (if present). The "then" clause comprises all lines after the "..if" and before the next "..else" or "..end" statement that is at the same nesting level. There are effectively unlimited nesting levels.
<b style="color:red;">..quit</b>|This causes the JAM processor to exit. The "cancel" operand, if present, causes the temporary output file to be deleted. The text after "cancel" is displayed to the user to indicate the reason for termination.
<b style="color:red;">..else</b>|The "else" clause comprises all lines after the "..else" and before the next "..end" statement that is at the same nesting level.
<b style="color:red;">..say</b>|This displays a message on user user's terminal after evaluating any REXX expressions in the message text that are enclosed in square brackets.
<b style="color:red;">..runon</b>|This generates the JES/JCL statements that are required to run a job on a user-defined system alias called "test". System aliases allow the user to refer to a z/OS instance by a memorable name, so if you want to call a z/OS instance "prod" instead of "C3PO" or "R2D2" then aliases are the way to go. Some sample default system aliases are defined in the JAMSITE REXX procedure which loads a table of LPAR information from a file called LPARS. For complex sites with many sysplexes and LPARs this saves the user from having to remember details about each system or even the often bizarre system names.
<b style="color:red;">..copy</b>|This generates JCL statements to copy a file to another file. The exact JCL generated depends on the operands. For example, different JCL is generated to copy a dataset to a Unix System Services file.

Running JAM against this input file would product something like:

    //U12345A  JOB ,'',CLASS=U,MSGCLASS=T,NOTIFY=&SYSUID
    /*XEQ N11                     (JES2TEST)
    /*JOBPARM SYSAFF=SY3          (TEST/SY3)
    //STEP1   EXEC PGM=IEBGENER
    //SYSPRINT  DD SYSOUT=*
    //I1        DD DISP=SHR,DSN=MY.INPUT.FILE
    //O1        DD DISP=SHR,DSN=MY.OUTPUT.FILE

You: "hmmm...but isn't the resulting JCL simpler than the input JAM statements?"

Me:  Well, yes - in this case. But imagine that you had to generate JCL to copy a 
table of 1000 datasets. The number of JAM statements to do that would be in the
10's of lines - and would be reusable for many different tables of datasets.

More examples of input and output files can be found in the /samples folder of this repository.



## COMMAND SYNTAX

### On z/OS only
On z/OS you can use JAM either as an ISPF/EDIT macro or a standalone REXX. The syntax is:

`JAM arguments...`

In batch, JAM reads from DD:IN and writes to DD:OUT.


### On Linux or Windows
On Linux or Windows the syntax is:

`JAM filein [fileout | -] [--options...]`


Where,

* `filein` - Identifies the JAM input file.
* `fileout` - Identifies the transformed output file to be created.
              The default is the path and file name of the input file with a `.txt` extension appended.
              If `-` is specified then the output is written to the terminal.
* `options` are specified after a double-dash:


# List of JAM verbs

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

       //*
       //*-------------------------------------------------------------------*
       //* Copy members                                                      *
       //*-------------------------------------------------------------------*
       //*
       //STEP1   EXEC PGM=IEBCOPY
       //SYSPRINT  DD SYSOUT=*
       //DD1       DD DISP=SHR,DSN=MY.PDS
       //DD2       DD DISP=SHR,DSN=YOUR.PDS
       //SYSIN     DD *
         COPY INDD=((DD1,R)),OUTDD=DD2
         SELECT MEMBER=(MEM1)
         SELECT MEMBER=(MEM2)
         SELECT MEMBER=(MEM3)
       /*

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

- [...](#---------jam-comment)
- [..*](#---------comment)
- [..ARGS](#args------var-var)
- [..ASK](#ask-------var-default-prompt)
- [..AUTO](#auto------text)
- [..BACKUP](#backup----dsn-backup--------options)
- [..BR14](#br14------label)
- [..CATALOG](#catalog---dsn-volser-catalog)
- [..COMPRESS](#compress--dsn-volser)
- [..COPY](#copy------fromdsn-todsn)
- [..DATEVARS](#datevars--date--days)
- [..DELETE](#delete----dsn-catalog-options)
- [..ELSE](#else------action)
- [..END](#end)
- [..GET](#get-------fromdsn-fromsys-todsn-locsiteoptions)
- [..GETOUT](#getout----fromdsn-fromsys-todsn)
- [..HELP](#help)
- [..IF](#if--------cond-action)
- [..FOR](#for-------read-dsn-macro-macroname)
- [..INCLUDE](#include---dsn)
- [..JCL](#jcl-------stmt)
- [..JOB](#job-------systemjobnamesuffix-description)
- [..LISTCAT](#listcat---dsn-catalog-options)
- [..LISTVTOC](#listvtoc--dsn-volser)
- [..MACRO](#macro-----define-macroname-parameters)
- [..MAP](#map-------dsn)
- [..MOUNT](#mount-----dsn-path-options)
- [..OPTION](#option----nooption)
- [..OTHERWISE](#otherwise-action)
- [..PUT](#put-------fromdsn-tosystem-todsn-siteoptions)
- [..QUEUE](#queue-----line)
- [..QUEUED](#queued)
- [..QUIT](#quit------cancel-quitmsg)
- [..RECOVER](#recover---dsn-fromvol-tovol-options)
- [..RENAME](#rename----dsn-todsn-volser)
- [..REPRO](#repro-----fromdsn-todsn-fromrec-count)
- [..RESTORE](#restore---dsn-backup----options)
- [..REXX](#rexx------statement)
- [..RUNON](#runon-----systemjobnamesuffixvia-description)
- [..SAY](#say-------text)
- [..SCRATCH](#scratch---dsn-volser)
- [..SELECT](#select----expr)
- [..SET](#set-------var--expr)
- [..SHIP](#ship------dsn-tosystem-todsn-options)
- [..STEP](#step------args)
- [..STYLE](#style-----namewidthfirstborderleftborderfillborderrightcommentleftcommentrightlast)
- [..SUBMIT](#submit----dsn-tosystem-outdsn)
- [..SUDO](#sudo------unixcommand)
- [..TABLE](#table-----dsn)
- [..TSO](#tso-------tsocommand)
- [..UNCATALOG](#uncatalog-dsn-catalog)
- [..UNMOUNT](#unmount---dsn-options)
- [..USS](#uss-------unixcommand)
- [..WHEN](#when------expr-action)
- [..XEQ](#xeq------alias)
- [..XMIT](#xmit------dsn-system-userid-options)
### ...         [JAM comment]

This is used to add comments to a JAM input file. These comments
are ignored by the JAM processor and do not produce any output.

For example, the following simply builds a job card and ignores the
preceding comments:

    ...
    ... Build a job card
    ...
    .. job

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

      //*
      //*-------------------------------------------------------------------*
      //* This is a JCL comment                                             *
      //*-------------------------------------------------------------------*
      //*


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

### ..BACKUP    dsn backup        [options...]

  This will generate a job step that will backup dataset(s) "dsn" to
  a backup dataset called "backup" using ADRDSSU DUMP.

### ..BR14      [label]

  This will generate a dummy (IEFBR14) JCL step.

  If the step label is omitted then the next
  sequential step number (in the REXX variable "step") is used to
  create a label for you.

  For example,

    ..br14

  generates

    //STEP1   EXEC PGM=IEFBR14

### ..CATALOG   dsn volser [catalog]

  This will generate a job step that will catalog dataset "dsn" on
  volume "volser" in the specified catalog, or else in
  the catalog appropriate for the "alias" system. The
  following pre-defined catalog variables can be used:

  | Variable | Description |
  | -------- | ----------- |
  | cat      | The master catalog for the alias system. |

### ..COMPRESS  dsn [volser]

  This will generate a job step that will compress partitioned dataset "dsn" on
  volume "volser", or else will compress the cataloged dataset
  if the volume is omitted.

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

### ..DELETE    dsn [catalog] [options...]

  This will generate a job step that will delete dataset "dsn" from the
  specified catalog, or else from the catalog appropriate
  for the "alias" system. You can also specify any options
  valid for IDCAMS DELETE in "options".

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

### ..END

  This closes the previous matching `..if` or `..select` JAM statement.

### ..GET       fromdsn fromsys [todsn [locsiteoptions...]]

  This generates a job step that uses FTP to get a dataset called
  "fromdsn" from system "fromsys" and optionally store
  it locally in dataset "todsn". Use "locsiteoptions" to
  specify options for the FTP client's LOCSITE subcommand (for example
  RECFM=VB etc).

### ..GETOUT    fromdsn fromsys todsn

  This generates a job step that uses FTP to submit dataset called
  "fromdsn" on system "fromsys" and store the resulting
  output locally in dataset "todsn".

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

### ..JCL       [stmt]

  This reformats the specified JCL statement with continuations
  if necessary.

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

### ..LISTCAT   dsn [catalog] [options...]

  This generates a job step that will invoke IDCAMS to LIST dataset "dsn" in the
  specified catalog, or else in the standard catalog
  search order. The following pre-defined catalog
  variables can be used:

  | Variable | Description                               |
  | -------- | ----------------------------------------- |
  | cat      | The master catalog for the alias system.  |

### ..LISTVTOC  dsn volser

   This generates a job step that will list datasets specified by
   "dsn" in the VTOC of the volume specified by "volser".
   The dataset name can be generic (e.g. SYS1.XXX*).

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

### ..MOUNT     dsn path [options...]
### ..MOUNT     path dsn [options...]

  This generates a job step to mount file system "dsn" at mount point "path" using any
  "options" valid on the TSO MOUNT command. The "path"
  must contain a "/" character to distinguish it from the
  "dsn" operand.

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

### ..PUT       fromdsn tosystem [todsn [siteoptions...]]

  This generates a job step that uses FTP to transfer local dataset
  "fromdsn" to system "tosystem" and optionally
  store it in a dataset called "todsn" on the target
  system. Use "siteoptions" to specify options for the FTP client
  SITE subcommand (for example, to specify RECFM=VB etc).

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

### ..QUEUED

  This processes any JAM statements that were queued by
  earlier `..queue` JAM statements and then clears the
  queue.

  This is useful if you have a macro that has queued some
  output using `..queue` but you want to place it in a particular place
  after the macro has been invoked.

### ..QUIT      [CANCEL] [quitmsg]

  This stops the JAM processor. If "quitmsg" is present
  then that message is displayed as the reason for
  quitting. If "CANCEL" is present then the ISPF EDIT
  session is also cancelled.
### ..RECOVER   dsn fromvol tovol [options...]

  This generates a job step that will copy dataset "dsn" from
  volume "fromvol" to volume "tovol" using ADRDSSU
  options "option" (for example, REPLACE, TOL(ENQF) etc).

### ..RENAME    dsn todsn [volser]
### ..RENAME    pds(mem1[,mem2...]) (new1[,new2...]) [volser]
### ..RENAME    path topath

  This generates a job step that will rename a dataset, members in a
  partitioned dataset, or a Unix System Services file or directory.

### ..REPRO     fromdsn todsn [fromrec [count]]

  This generates a job step that will copy dataset "fromdsn" to
  dataset "todsn" starting at record number "fromrec"
  and continuing for "count" records (or End Of File)

### ..RESTORE   dsn [backup | =]  [options...]

  This generates a job step that will restore datasets "dsn" from
  a backup dataset "backup" that was created by ADRDSSU
  DUMP. If the "backup" dataset is omitted (or specified
  as "=") then the most recently specified "backup"
  dataset is used.

### ..REXX      statement

  This executes the specified REXX statement.

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

### ..SAY       text

  This displays the specified message text on the user's terminal.

### ..SCRATCH   dsn volser

  This generates a job step that will delete dataset "dsn" from
  volume "volser".

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

### ..SET       var = expr

  This evaluates the expression "expr" and assigns the result to a REXX
  variable called "var". Any valid REXX expression can be used.
  The variable named "alias" is special. If you set this
  variable then several other variables will be updated
  with information about that system alias. Use
  `..set ?` to list these variables.

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
  | jcl  | 71    | //* | //*         | -               | *                | //*          | *                 | //* |
  | js   | 80    | //      | //              | -               |                  | //               |                   | //      |
  | rexx | 80    | /*  | &nbsp;*         | -               | */           | &nbsp;*          | */            | */  |
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

### ..SUDO      unixcommand

  This generates a job step to execute the specified command as superuser in the Unix
  System Services environment. The invoker will need to be permitted
  RACF READ access to BPX.SUPERUSER (or have uid=0) for this to be
  effective.

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

### ..TSO       tsocommand

  This generates a job step to executes the specified TSO command in batch

### ..UNCATALOG dsn [catalog]

  This generates a job step to uncatalogs dataset "dsn" from the specified catalog, or
  else from the catalog appropriate for the "alias" system.

### ..UNMOUNT   dsn [options...]

  This generates a job step to unmount file system "dsn" using any "options" valid on
  the TSO UNMOUNT command.

### ..USS       unixcommand

  This generates a job step to executes the specified command in the Unix System
  Services environment.

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

      /*XEQ N11                 (JESTST1)
      /*JOBPARM SYSAFF=R2D2     (TST1)

  Note: This verb is rarely needed. See `..runon` and `..job`.

### ..XMIT      dsn [system [userid [options...]]]

   This generates JCL to issue the TSO TRANSMIT to send dataset
   "dsn" to userid "userid" at system "system".
   The "options" can be any options valid for the TRANSMIT
   command (but you must specify system and userid too).
   The default "system" is the invoking system.
   The default "userid" is the invoking userid.


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
