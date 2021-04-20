![JAMZ icon](../images/jamz.png)
# Samples

* On Linux or Windows, the JAM files in the `./samples` directory can be run by issuing the following command:

    rexx jam.rex ./samples/*filename*.jam -

* On z/OS you can issue:

    tso rexx *samples*(*filename*) my.output.dataset

    or

    Edit *samples*(*filename*) and type JAM to process the file being edited.

    ...assuming the sample files have been stored in a partitioned dataset without the `.jam` suffix.



| File          | Description |
| ----          | ----------- |
| alias.jam     | [How to use system aliases](#aliasjam) |
| ask.jam       | [How to get input from a user](#askjam) |
| comments.jam  | [How to generate comments in various formats](#commentsjam) |
| datevars.jam  | [How to use dates and date arithmetic](#datevarsjam) |
| for.jam       | [How to invoke JAM macros using the `..for` JAM verb](#forjam) |
| getmulti.jam  | [How to get a dataset from multiple systems](#getmultijam) |
| help.jam      | [How to display all JAM help information](#helpjam) |
| history.jam   | [How to update the history comments in a file](#historyjam) |
| ieasys00.jam  | [How to read keyword=value parameters from a file](#ieasys00jam) |
| inline.jam    | [How to request user input using inline expressions](#inlinejam) |
| intersect.jam | [How to use the `intersect` built-in function](#intersectjam) |
| map.jam       | [How to access table data by key](#mapjam) |
| only.jam      | [How to filter strings using the `only` built-in function](#onlyjam) |
| queue.jam     | [How to explicitly place queued data in the output](#queuejam) |
| range.jam     | [How to generate a range of numbers](#rangejam) |
| recipe.jam    | [How to generate a jam tart recipe](#recipejam) |
| runon.jam     | [How to run a job on a specified system](#runonjam) |
| sortstem.jam  | [How to sort a REXX stem variable](#sortstemjam) |
| sortwords.jam | [How to sort words in a REXX string](#sortwordsjam) |
| table.jam     | [How to access table data by row number](#tablejam) |
| toblock.jam   | [How to convert a string to an array (and vice versa)](#toblockjam) |
| union.jam     | [How to use the `union` built-in function](#unionjam) |


## alias.jam
This sample JAM file shows how to use system aliases.


System aliases are the names you assign to each z/OS system and are not
related to any z/OS configuration item.  You can define your own personal
system aliases in a file called LPARS. The LPARS file is loaded by the
JAMSITE REXX procedure into REXX variables each time you run the JAM REXX
procedure. In this way, information about each of the LPARs at your site
is readily available to any JAM files you create.

A sample LPARS file is distributed with JAM. You should customise it for
your site. The sample LPARS file contains:

    alias sysname jc mc sysclone sysplex  jesname jesnode njenet  cat             tags        host
    TST1  R2D2    A  X  T1       TESTPLEX JESTST1 N11     NJEA    SYS1.MASTER.CAT 'DEV TEST'  r2d2.example.org
    TST2  R2D3    A  X  T2       TESTPLEX JESTST2 N12     NJEA    SYS1.MASTER.CAT 'UAT'       r2d3.example.org
    PRD1  C3P0    A  X  P1       PRODPLEX JESPRD1 N21     NJEB    SYS1.MASTER.CAT 'PROD'      c3p0.example.org
    PRD2  C3P1    A  X  P1       PRODPLEX JESPRD2 N22     NJEB    SYS1.MASTER.CAT 'PROD DR'   c3p1.example.org

The first column must be called "alias". The remaining columns are accessed by using
that alias as a key. For example, if the `lpar` REXX variable contains `PRD1` then you can access
the sysname of that system using `[sysname.lpar]`. Alternatively, you could
take advantage of the REXX feature whereby the value of unassigned variables
is the upper case name of that variable by using `[sysname.PRD1]`.

You can add your own columns if you wish. For example, you may
want to add a column called "owner" containing the email address of the contact point
for that system.

The alias information is used by the `..job` and `..runon` JAM verbs to build an
appropriate JOB card.


## ask.jam

This sample JAM file shows how to use the `..ask`, `..askq`, `..asku` and `..askqu` JAM verbs.

They all prompt the user for a response and the response is optionally converted to upper case
(`..asku` and `..askqu`) before it is assigned to the nominated REXX variable. In addition, the `..askq` and `..askqu` verbs allow the user to reply `q` or `Q` to quit the JAM session.

If the user simply presses Enter without keying a reply then the specified default value is assigned
to the REXX variable. If the default value is null or contains spaces, then it must be enclosed with
apostrophes, for example `..ask myvar '' Enter a value`.

## comments.jam

This sample JAM file shows how to generate a comment in a specified style.

The JAM verb for a comment is always `..*` but the style of the generated
comment depends on what you most recently specified on the `..style` JAM verb. There are a number
of built-in comment styles but you can always define your own named style by using the `..style` JAM verb.
The default style is `jcl`.

## datevars.jam

The `..datevars` JAM verb takes a date that you specify (in pretty much any format you like) and creates
a number of REXX variables each representing some aspect of that date. The complete list of REXX variables
created is:

  | Variable  | Example         | Description                         |
  | --------  | --------------  | ----------------------------------- |
  | datevar   | 25/2/1966       | The input date passed to `..datevars` |
  | basedate  | 717756          | Days since 0001/01/01               |
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

You can then either use these REXX variables as-is or build your own date formats.
For example, you can build an ISO 8601 datetime format by:

    ..datevars easter 2021
    ..set datetime = '[yyyy]-[mm]-[dd]T[time('LONG')]+1000'

## for.jam

This sample JAM file shows how to invoke a JAM macro for a range of values.

The `..for` JAM verb can be used to invoke the nominated JAM macro for:

| Description | Example |
| ----------- | ------------- |
| each word in a list of words | `..for A B C macro mydd`
| a range of numbers | `..for 1 to 5 macro mydd`
| a range of numbers at equal steps | `..for 2 to 6 by 2 macro mydd`
| a fixed number of times | `..for 3 macro mydd`
| each (optionally filtered) line of a file | `..for read my.file macro mydd`

In all cases, each word, number, or line of the file is passed to the nominated JAM
macro for processing.

The `..for read` syntax can optionally limit the range of lines read from 
the file and/or only pass those lines that satisfy a filter condition as follows:

    ..FOR READ dsn [limit1 [limit2]] [PARSE template WHERE condexpr] MACRO macroname

This reads lines from the file "dsn" constrained by the limits "limit1" and "limit2"
and invokes the macro called "macroname" once for each selected line as follows:

| limit1    | limit2    | Lines read                                                     |
| ------    | ------    | ----------                                                     |
| (omitted) | (omitted) | Reads all lines (i.e. no limits)                               |
| n         | (omitted) | Reads the first "n" lines                                      |
| n         | m         | Reads lines starting at line "n" for "m" lines                 |
| n         | string    | Reads lines starting at line "n" until "string" is found       |
| string    | m         | Reads "m" lines starting at the first line containing "string" |
| string1   | string2   | Reads the first block of lines bounded by the strings "string1" and "string2"     |

If a string argument contains spaces then it must be enclosed with `'` or `"` characters.

If `PARSE template WHERE condexpr` is also specified, then each of the selected lines are further
parsed (using the REXX `parse var line [template]` statement) and the conditional 
expression "condexpr" is applied (using the REXX `if [condexpr]` statement). When `condexpr`
evaluates to 1 (true) then the line is selected, else it is not selected.
This enables lines to be selected using more complex logic.

## getmulti.jam

This sample JAM file shows how to retrieve a specified dataset from multiple
systems. A good use for this is to obtain local copy of SYS1.PARMLIB from
various systems so that you can more easily scan the libraries for certain
strings.

## help.jam

This sample JAM file will display help information for all JAM verbs.

## history.jam

This sample JAM file shows how to extract the change history comments from
a specified file, then prompt for a new change history comment and insert it 
with today's date.

For this example, the format of each change history line needs to be:

    ** HISTORY  - Date     By  Reason (most recent at the top please)    **
    **            -------- --- ----------------------------------------- **
    **            yyyymmdd xxx Some reason                               **

Because not all comments can fit on one line, the sample shows how to use
the `toBlock` built-in function to split long comments into REXX stem 
variables so that they can span multiple lines.


## ieasys00.jam

This sample JAM file shows how to parse `keyword=value` lines from a file.

The `..for read` JAM verb is used to read lines from a file, filtered by a
`where` clause. The variables in the `where` clause have had their
values set by the REXX `parse` clause on the `..for read` statement.
Only those lines that satisfy the specified condition are passed to the
nominated JAM macro (called  `kv` in this case).

The `kv` macro parses the line passed to it in order to extract the
key and value, then outputs the original input line followed by the
key and value. For example,

    Input                       Keyword Value
    --------------------       -------- --------------------
    ALLOC=(A1,A2),                ALLOC (A1,A2)
    AUTOR=OFF,                    AUTOR OFF
    .
    .


## inline.jam

This JAM sample file shows how to prompt the user for input by using
the inline variable syntax. 

An inline variable is a REXX variable name suffixed by a question mark
surrounded by square brackets. For example, `[reply?]`. When the JAM
processor sees this it will prompt the user with `Enter reply:`.

You can supply your own prompt by coding, for example, `[reply What is your answer?]`
which will prompt the user with `What is your answer?`. 

In both cases, the user's reply will be assigned to the nominated REXX 
variable (`reply` in this case).

## intersect.jam

This sample JAM file shows how to use the `intersect` built-in function.

The `intersect` built-in function returns a list of words that are common
to the two supplied lists of words.

It could be useful to remove duplicate words from a list.

See also the [union JAM verb](#unionjam) to combine words into a single list.

## map.jam

This sample JAM file shows how to access a table of columnar data by key.

An alternative is to [access table data by row number](#tablejam) using the `..table` JAM verb.

The table data should have headings and the first column is the key.
The remaining columns can be accessed by the key value of each row.

For example, if the following table data is in a file called *mytab*:

    user     phone             email              name
    U001     555-1111          u001@example.org   Don McLean
    U002     '+61 2 5555 2222' u002@example.org   Don Quixote
    U003     555-3333          u003@example.org   Don Juan

Then user U002's data can be accessed by:

    ..map mytab
    U002: [phone.U002] [email.U002] [name.U002]

...as long as the REXX variable U002 contains `'U002'`.

The following produces exactly the same output: 

    ..map mytab
    ..set key = 'U002'
    [key]: [phone.key] [email.key] [name.key]




## only.jam

This sample JAM file shows how to translate a string to leave only selected characters
using the `only` built-in function.

## queue.jam

This sample JAM file shows how to explicitly place some queued lines in the JAM output.

This is useful when the lines created by a JAM macro are themselves JAM statements and
you want to process them at a particular place, rather than as they are created.

A handy tip if you are generating "linked" JAM statements is to append `,[]` to each
queued line. That prevents the JAM processor from trying to process the "linked" statements 
at creation time (i.e. the lines so queued do not end in a comma, but when they
are subsequently emitted by the `..queued` JAM verb they *will* end in a comma and
therefore be treated as "linked" JAM statements).  For example,

    ..queue ..copy file.a file.b ,[]
    ..queue ..copy file.c file.d ,[]
    ..queue ..copy file.e file.f
    ..* Copying some files...
    ..queued

The JAM processor will treat the `..copy` statements as linked (i.e. generate a single
job step to process all the statements), but not the `..queue` statements
because each `..queue` statement does not end in a comma.

## range.jam

This sample JAM file shows how to use the built-in `range` function.

The `range` built-in function generates a range of numbers optionally separated by 
a specified number of pad characters.

## recipe.jam

This sample JAM file shows how to solicit input from the user and generate some
output based on that input.

## runon.jam

The sample JAM file shows how to run a job on a specified system.

The `..runon` JAM verb treats the first operand as the alias for the system on which to 
run the subsequent JCL. If the first operand is not a known alias, then the JCL will 
run on the submitter's system. The remaining operands become the 'programmer name' field
on the JOB card.

The job name is automatically built based on the submitter's userid and the last character
of the submitting system's alias. However, a single character job name suffix can be supplied 
after a forward slash (`/`). For example:

    ..runon prd1/A this is my A job
    ..br14
    ..runon prd1/B this is my B job
    ..br14

The job class and message class used is specified in the LPARS file that is loaded by the
JAMSITE REXX procedure. If you want to override this or any other LPAR-related parameter, 
then you can set it yourself using:

    ..set jc.PRD1 = 'B' /* Set job class for the PRD1 alias */
    ..set mc.PRD1 = 'X' /* Set message class for the PRD1 alias */
    ..runon prd1 runs in class B

## sortstem.jam

This sample JAM file shows how to sort a REXX stem variable.

The stem variable must have element 0 containing the number of elements in the array to be sorted.
For example,

    ..set list.0 = 3
    ..set list.1 = 'the'
    ..set list.2 = 'cat'
    ..set list.3 = 'sat'

Alternatively, you can build your array using the `toArray` built-in function:

    ..set elements = toArray('the cat sat','list.')

Or you can load your array as a table using the `..table` JAM verb. For example,
assuming that `myfile` contains:

    the
    cat
    sat
    
You could then load the list of words using:

    ..table myfile list

Now you can invoke the `sortstem` built-in function to sort the stem variable. 

    ..set alreadysorted = sortstem('list.')

The second operand of the `sortstem` built-in function is the sort order boolean (0 = descending,
1 = ascending). The default is 1 (ascending). The return value is a boolean (0 = not already sorted,
1 = already sorted).

The `sortstem` built-in function does **not** modify the original stem variables. Instead
it returns a stem variable called `sorted.` which contains the number of elements in `sorted.0`
and the index of the next input element in the desired sort order. For example:

| n     | list.n | sorted.n   | Description        |
| ----- | -----  | -------    | -----------        |
| 0     | 3      | 3          | Number of elements |
| 1     | the    | 2          | list.2 = cat       |
| 2     | cat    | 3          | list.3 = sat       |
| 3     | sat    | 1          | list.1 = the       |


## sortwords.jam

This sample JAM file shows how to sort a list of words in a string.

## table.jam

This sample JAM file shows how to access a table of columnar data by row number.

An alternative is to [access table data by key](#mapjam) using the `..map` JAM verb.


## toblock.jam

This sample JAM file shows how to re-block a string into multiple rows of data.

The `toBlock` built-in function can be used to take a long string of words and
transform it into an array of lines, each line containing no more than a specified
number of characters.

This is useful when formatting comments in JCL or any file that has a fixed width.

This same also shows how to perform the reverse transformation: to convert an
array of strings into a single string using the `toString` built-in function.


## union.jam

This sample JAM file shows how to use the `union` built-in function.

The `union` built-in function returns a list all of words that are in two supplied lists
but with no duplicates.

See also the [intersect built-in function](#intersectjam) to extract words common to two lists.

