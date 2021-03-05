* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* VIRGILIO CALIMLIM HTTP://WWW.LINKEDIN.COM/IN/VIRGILIOCALIMLIM       *
* ------------------------------------------------------------------- *
* STRING FORMATTING UTILITY CALLED FROM A COBOL PROGRAM               *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* MODULE   : SRVSTRNG                                                 *
* FUNCTIONS: 1. FOR NUMERIC (N) AND SIGNED (+) FORMAT, UNDERSCORES &  *
*               LOW-VALUES ARE IGNORED OR REPLACED BY ZEROS DEPENDING *
*               ON ITS EXISTENCE BEFORE OR AFTER SIGNIFICANT DIGITS.  *
*               IF FOUND ACCEPTABLE AS NUMERIC, IT IS REFORMATTED AND *
*               ADJUSTED ACCORDING TO THE ASSIGNED FORMAT. AN OPTION  *
*               FOR DECIMAL PLACES IS INCLUDED.                       *
*            2. FOR CHARACTER FORMATS, UNDERSCORES AND LOW-VALUES ARE *
*               REPLACED BY SPACES. THE STRING IS THEN MANIPULATED    *
*               ACCORDING TO REQUIRED FORMAT, WHETHER LEFT, CENTER    *
*               OR RIGHT JUSTIFIED. A COMPRESS OPTION IS ALSO         *
*               INCLUDED TO ALLOW JUST ONE SPACE BETWEEN WORDS.       *
* OPTIONS  : FMT = N - NUMERIC DATA                                   *
*                = + - SIGNED NUMERIC DATA                            *
*                = A - RETURNED 'AS IS' AFTER UNDERSCORES AND         *
*                      LOW-VALUES ARE REPLACED BY SPACES              *
*                = < - COMPRESSED AND LEFT JUSTIFIED                  *
*                = C - COMPRESSED AND CENTERED                        *
*                = > - COMPRESSED AND RIGHT JUSTIFIED                 *
*                = L - LEFT JUSTIFIED                                 *
*                = M - MIDDLE OR CENTERED                             *
*                = R - RIGHT JUSTIFIED                                *
* RESTRCTN : STAT WILL CONTAIN ANY OF THE FOLLOWING VALUES UPON       *
*            RETURN TO THE CALLING PROGRAM :                          *
*            0 - SUCCESSFUL PROCESSING                                *
*            1 - INVALID FORMAT                                       *
*            2 - VALID FORMAT BUT LEN IS 0 OR GREATER THAN 256        *
*            3 - FORMAT IS NUMERIC/SIGNED; DECPL IS GREATER THAN LEN  *
*            4 - FORMAT IS NUMERIC/SIGNED; STRING WAS EVALUATED AS    *
*                NOT NUMERIC                                          *
*            5 - FORMAT IS NUMERIC/SIGNED; WORKAREA EXECEEDED DUE TO  *
*                ADJUSTMENT FOR DECPL
*                                                         VBC-03MAY89 *
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *
* COBOL FORMAT:                                                       *
*   CALL 'SRVSTRNG' USING SRVSTRNG-PARAMETERS STRING.                 *
* PARAMETERS:                                                         *
*   01  SRVSTRNG-PARAMETERS.                                          *
*       03  STR-STAT               PIC 9.                             *
*       03  STR-FMT                PIC X.                             *
*           88  STR-FMT-VAL                  VALUE 'N' '+' 'A'        *
*                                                  '<' 'C' '>'        *
*                                                  'L' 'M' 'R'.       *
*       03  STR-LEN                PIC 9(4) COMP.                     *
*       03  STR-DECPL              PIC 9(4) COMP.                     *
*   01  STRING                     PIC X(256).                        *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
* EXAMPLES :
* ---------------------------------------------------------------------
*  FMT : LEN : DPL : INP STRING : OUT STRING : EDIT FIELD : SCREEN VAL
*      :     :     : ....+....1 : ....+....1 : ....+....1 : ....+....1
* ---------------------------------------------------------------------
*   N     10     2   123456____   0000123456   ZZZ,ZZ9.99     1,234.56
*   N     10     2   ___123456_   0000123456   ZZZ,ZZ9.99     1,234.56
*   N     10     2   1234.567__   0000123456   ZZZ,ZZ9.99     1,234.56
*   N     10     2   1,234.____   0000123400   ZZZ,ZZ9.99     1,234.00
*   +     10     2   __123456__   000012345F   +ZZ,ZZ9.99    +1,234.56
*   +     10     2   1234.567__   000012345F   +ZZ,ZZ9.99    +1,234.56
*   +     10     2   1,234.____   000012340:   +ZZ,ZZ9.99    +1,234.00
*   +     10     2   __$1234.__   000012340:   $+Z,ZZ9.99   $+1,234.00
*   +     10     2   $-1,234.__   000012340:   $+Z,ZZ9.99   $-1,234.00
*   A     10         _THE__BOY_    THE  BOY    X(10)         THE  BOY
*   L     10         _THE__BOY_   THE  BOY     X(10)        THE  BOY
*   M     10         THE__BOY__    THE  BOY    X(10)         THE  BOY
*   R     10         THE__BOY__     THE  BOY   X(10)          THE  BOY
*   <     10         __THE__BOY   THE BOY      X(10)        THE BOY
*   C     10         __THE__BOY    THE BOY     X(10)         THE BOY
*   >     10         THE__BOY__      THE BOY   X(10)           THE BOY
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
SRVSTRNG CSECT ,
BEGIN    STM   14,12,12(13)
         BALR  12,0
         USING *,12
         ST    13,SAVEREG+4
START    EQU   *
         LM    2,3,0(1)
         MVC   STAT(2),0(2)
         LA    4,0
         ST    4,LEN
         ST    4,DECPL
         MVC   LEN+2(2),2(2)
         MVC   DECPL+2(2),4(2)
         L     4,LEN
         C     4,=F'1'
         BL    ERROR2
         C     4,=F'256'
         BH    ERROR2
         MVI   0(2),X'F0'              INITIALIZE STAT TO 0
         CLI   TYPE,C'N'               NUMERIC DATA ?
         BE    ZEROFILL
         CLI   TYPE,C'+'               SIGNED NUMERIC DATA ?
         BE    ZEROFILL
*
SPACFILL LR    5,3                     LOAD START BYTE DATAADDR TO REG5
         LA    6,WORKAREA              LOAD START BYTE WA-ADDR TO REG6
         LR    7,6                     INIT FIRST SIGNI CHAR
         LR    8,6                     INIT LAST SIGNI CHAR
         MVI   SWSIGNI,X'00'           SET SIGNI SW TO OFF
SPA010   CLI   0(5),X'00'              CHAR = LOW VALUE ?
         BE    SPA020                  ...YES, GOTO MOVE SPACE
         CLI   0(5),X'6D'              CHAR = UNDERSCORE ?
         BE    SPA020                  ...YES, GOTO MOVE SPACE
         CLI   0(5),X'40'              CHAR = SPACE ?
         BE    SPA020                  ...YES, GOTO MOVE SPACE
         MVC   0(1,6),0(5)             MOVE SIGNIFICANT CHAR
         LR    8,6                     SAVE ADDR OF LAST SIGNI CHAR
         CLI   SWSIGNI,X'01'           SIGNI SW ON ?
         BE    SPA030                  ...YES, CHECK FOR ANOTHER LOOP
         MVI   SWSIGNI,X'01'           SET SIGNI SW TO ON
         LR    7,6                     SAVE ADDRESS OF FIRST SIGNI CHAR
         B     SPA030                  GOTO INCREMENT ADDR
SPA020   MVI   0(6),X'40'              MOVE SPACE TO WA
SPA030   A     6,=F'1'                 INCREMENT WA-ADDR
         A     5,=F'1'                 INCREMENT DATAADDR
         BCT   4,SPA010                DECREMENT LEN UNTIL 0
         L     4,LEN                   LOAD LEN TO REG4
         CLI   TYPE,C'A'               STRING AS IS ?
         BE    MOVEOUT                 ...YES, MOVEOUT
         LR    5,3                     LOAD DATAADDR TO REG5
SPA040   MVI   0(5),X'40'              MOVE SPACES TO DATAAREA
         A     5,=F'1'                 INCREMENT TO NEXT DATAADDR
         BCT   4,SPA040                DECREMENT LEN REG UNTIL 0
         CLI   SWSIGNI,X'00'           SIGNI SW OFF ?
         BE    BACK                    ...YES, RETURN
         SR    8,7                     GET LENGTH OF SIGNI CHARS
         A     8,=F'1'                 .
*
* AT THIS POINT . . .
* REG 7 CONTAINS THE START OF THE FIRST SIGNI CHAR IN WORKAREA
* REG 8 CONTAINS THE LENGTH OR THE NO. OF SIGNI CHAR/S
*
         CLI   TYPE,C'<'               STRING COMPRESSED & LEFTJUST ?
         BE    CLEFT
         CLI   TYPE,C'C'               STRING COMPRESSED & CENTERED ?
         BE    CCENTR
         CLI   TYPE,C'>'               STRING COMPRESSED & RIGHTJUST ?
         BE    CRIGHT
         CLI   TYPE,C'L'               STRING TO BE LEFT JUSTIFIED ?
         BE    LEFT
         CLI   TYPE,C'M'               STRING TO BE CENTERED ?
         BE    MIDDLE
         CLI   TYPE,C'R'               STRING TO BE RIGHT JUSTIFIED ?
         BE    RIGHT
         B     ERROR1
*
CLEFT    BAL   11,COMPRES              LINK TO COMPRES W/ RETURN ADDR
         B     LEFT                    GOTO LEFT-RTN
*
CCENTR   BAL   11,COMPRES              LINK TO COMPRES W/ RETURN ADDR
         B     MIDDLE                  GOTO MIDDLE-RTN
*
CRIGHT   BAL   11,COMPRES              LINK TO COMPRES W/ RETURN ADDR
         B     RIGHT                   GOTO RIGHT-RTN
*
COMPRES  LA    5,WORKARE2              LOAD ADDR OF WRKARE2 TO REG5
         MVI   SWBLA,X'00'             INIT BLANK SW TO OFF
COM010   CLI   0(7),X'40'              CHAR = SPACE ?
         BNE   COM020                  ...NO, GOTO TRANSFER CHAR
         CLI   SWBLA,X'01'             BLANK SW ON ?
         BE    COM050                  ...YES, SKIP CHAR
         MVI   SWBLA,X'01'             SET BLANK SW TO ON
         B     COM030                  GOTO TRANSFER CHAR
COM020   LR    6,5                    COPY LAST SIGNI CHAR ADDR TO REG6
         MVI   SWBLA,X'00'             SET BLANK SW TO OFF
COM030   MVC   0(1,5),0(7)             MOVE WORKAREA TO WORKARE2
COM040   A     5,=F'1'                 INCREMENT FOR NEXT WORKARE2 ADDR
COM050   A     7,=F'1'                 INCREMENT FOR NEXT WORKAREA ADDR
         BCT   8,COM010                DECREMENT LEN UNTIL 0
         LA    7,WORKARE2              LOAD ADDR OF WRKARE2 TO REG7
         SR    6,7                     GET LENGTH OR NO. OF SIGNI CHAR
         A     6,=F'1'                 .
         LR    8,6                     .
         BR    11                      RETURN
*
MIDDLE   L     4,LEN                   LOAD LEN TO REG4
         SR    4,8                     GET DIFF OF DATALEN & NO. CHAR/S
         CVD   4,DWD                   SETUP START OF STRING IN CENTER
         MVC   FULL(4),DWD+4           .
         DP    FULL(4),=P'2'           .
         ZAP   DWD(8),FULL(3)          .
         CVB   4,DWD                   .
         AR    3,4                     SETUP START OF DATAADDR
*
LEFT     MVC   0(1,3),0(7)             MOVE WA TO DATAAREA
         A     3,=F'1'                 INCREMENT FOR NEXT DATAADDR
         A     7,=F'1'                 INCREMENT FOR NEXT WA-ADDR
         BCT   8,LEFT                  DECREMENT WA-LEN UNTIL 0
         B     BACK                    RETURN
*
RIGHT    A     3,LEN                   POS TO LAST BYTE OF DATAAREA
         BCTR  3,0                     .
         AR    7,8                     POS TO LAST SIGNI CHAR IN WA
         BCTR  7,0                     .
RIG010   MVC   0(1,3),0(7)             MOVE WA TO DATAAREA
         BCTR  3,0                     DECREMENT FOR NEXT DATAADDR
         BCTR  7,0                     DECREMENT FOR NEXT WA-ADDR
         BCT   8,RIG010                DECREMENT WA-LEN UNTIL 0
         B     BACK                    RETURN
*
ZEROFILL MVI   WORKAREA,X'F0'          INITIALIZE WORKAREA TO X'F0'
         MVC   WORKAREA+1(255),WORKAREA .
         MVI   SWNUM,X'00'             INITIALIZE SWITCHES TO X'00'
         MVI   SWSIGNI,X'00'           .
         MVI   SWPERIOD,X'00'          .
         MVI   SWCOMMA,X'00'           .
         MVI   SWPLUS,X'00'            .
         MVI   SWDOLLAR,X'00'          .
         MVI   SWMINUS,X'00'           .
         SR    7,7                     INIT REG7 FOR DECPL CTR
         SR    8,8                     INIT REG8 FOR COMMA CTR
         LR    5,3                     LOAD REG5 START BYTE DATA ADDR
         AR    5,4                     SET REG5 THE LAST BYTE DATA ADDR
         LA    6,WORKAREA         LOAD START BYTE WORKAREA ADDR TO REG6
         AR    6,4                     SET REG6 THE LAST BYTE WRKA ADDR
         BCTR  6,0                     CORRESPONDING TO DATA ADDR
ZER010   BCTR  5,0                     POS TO NEXT LOWER ADDR OF DATA
         CLI   0(5),X'F0'              CHAR < '0' ?
         BL    ZER015                  ...YES, CONTINUE NUMERIC CHECK
         CLI   0(5),X'F9'              CHAR > '9' ?
         BH    ZER015                  ...YES, CONTINUE NUMERIC CHECK
         CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BNE   ZERNUM                  ...NO, THEN OK
         CLI   SWNUM,X'01'             PREVIOUS CHAR NUMERIC ?
         BE    ZERNUM                  ...YES, GOTO NUMERIC RTN
         B     ERROR4                  ELSE  NON-NUM STRING; GOBACK
ZER015   CLI   0(5),X'C0'              CHAR < +0 ?
         BL    ZERNN                   ...YES, CONTINUE CHECKING
         CLI   0(5),X'C9'              CHAR < = +9 ?
         BNH   ZERSIGN                 ...YES, GOTO SIGN RTN
         CLI   0(5),X'D0'              CHAR < -0 ?
         BL    ZERNN                   ...YES, CONTINUE CHECKING
         CLI   0(5),X'D9'              CHAR > -9 ?
         BH    ZERNN                   ...YES, CONTINUE CHECKING
         OI    SWMINUS,X'10'           SET HIGH ORDER MINUS SW BITS ON
ZERSIGN  CLI   TYPE,C'+'               STRING TYPE IS SIGNED NUMERIC ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BE    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         MVC   0(1,6),0(5)             MOVE SIGNIFICANT CHAR
         OI    0(6),X'F0'              SET CHAR TO NUMERIC VALUE
         B     ZERNU1                  GOTO CONTINUE NUMERIC RTN
ZERNUM   MVC   0(1,6),0(5)             MOVE SIGNIFICANT CHAR
ZERNU1   MVI   SWSIGNI,X'01'           SET SIGNIFICANT SWITCH TO ON
         MVI   SWNUM,X'01'             SET NUMERIC SW TO ON
         AH    8,=H'1'                 INCREMENT COMMA CTR
         BCTR  6,0                     POS TO NEXT LOWER ADDR OF WRKARE
         CLI   SWPERIOD,X'01'          PERIOD PREVIOUSLY FOUND ?
         BE    ZER090                  ...YES, CHECK FOR ANOTHER LOOP
         A     7,=F'1'                 ...NO, INCREMENT DECPL CTR
         B     ZER090                  GO CHECK FOR ANOTHER LOOP
ZERNN    CLI   0(5),X'4B'              CHAR = PERIOD ?
         BNE   ZER020                  ...NO, CONTINUE CHECKING
         CLI   SWPERIOD,X'01'          PERIOD PREVIOUSLY FOUND ?
         BE    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         C     4,=F'2'                 DATALEN < 2 ?
         BL    ERROR4                  ...YES, GOTO ERROR4
         C     4,DECPL                 DATALEN < DEC PLACES ?
         BL    ERROR3                  ...YES, GOTO ERROR
         MVI   SWPERIOD,X'01'          ...NO, SET PERIOD SW TO ON
         SR    8,8                     RESET COMMA CTR TO 0
         B     ZER090                  CHECK FOR ANOTHER LOOP
ZER020   CLI   0(5),X'6B'              CHAR = COMMA ?
         BNE   ZER030                  ...NO, THEN CONTINUE CHECK
         CH    8,=H'3'                 FOUND 3 PREV CONSEC SIGNI DIGTS?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         BCTR  5,0                     ADVANCE TO NEXT LOWER ADDRESS
         CLI   0(5),X'F0'              CHAR < '0' ?
         BL    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         CLI   0(5),X'F9'              CHAR > '9' ?
         BH    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         MVI   SWCOMMA,X'01'           SET COMMA SWITCH TO ON
         SR    8,8                     RESET COMMA CTR
         A     5,=F'1'                 RESTORE REG5
         B     ZER090                  CHECK FOR ANOTHER LOOP
ZER030   MVI   SWNUM,X'00'             SET NUMERIC SWITCH TO OFF
         CLI   0(5),X'4E'              CHAR = PLUS ?
         BNE   ZER040                  ...NO, THEN CONTINUE CHECK
         CLI   TYPE,C'+'               STRING TYPE IS SIGNED NUMERIC ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         CLI   SWPLUS,X'01'            PREVIOUS PLUS SIGN FOUND ?
         BE    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         MVI   SWPLUS,X'01'            SET PLUS SWITCH TO ON
         B     ZER090                  CHECK FOR ANOTHER LOOP
ZER040   CLI   0(5),X'5B'              CHAR = DOLLAR SIGN ?
         BNE   ZER050                  ...NO, THEN CONTINUE CHECK
         CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         CLI   SWDOLLAR,X'01'          PREVIOUS DOLLAR SIGN FOUND ?
         BE    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         MVI   SWDOLLAR,X'01'          SET DOLLAR SWITCH TO ON
         B     ZER090                  CHECK FOR ANOTHER LOOP
ZER050   CLI   0(5),X'60'              CHAR = MINUS SIGN ?
         BNE   ZER060                  ...NO, THEN CONTINUE CHECK
         CLI   TYPE,C'+'               STRING TYPE IS SIGNED NUMERIC ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
         TM    SWMINUS,X'01'           PREVIOUS MINUS SIGN FOUND ?
         BO    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         OI    SWMINUS,X'01'           SET LOW ORDER MINUS SW BITS ON
         B     ZER090                  CHECK FOR ANOTHER LOOP
ZER060   CLI   0(5),X'00'              CHAR = LOW VALUE ?
         BE    ZER080                  ...YES, CHECK IF PERIOD FOUND
         CLI   0(5),X'40'              CHAR = SPACE ?
         BE    ZER080                  ...YES, CHECK IF PERIOD FOUND
         CLI   0(5),X'6D'              CHAR = UNDERSCORE ?
         BNE   ERROR4                  ...NO, NON-NUM STRING; GOBACK
ZER080   CLI   SWSIGNI,X'01'           PREVIOUS SIGNI DIGIT FOUND ?
         BE    ZER090                  ...YES, CHECK FOR ANOTHER LOOP
         A     5,=F'1'                 RECALL PREVIOUS HIGHER ADDRESS
         CLI   0(5),X'4B'              CHAR = PERIOD ?
         BE    ERROR4                  ...YES, NON-NUM STRING; GOBACK
         BCTR  5,0                     RESTORE REG5
ZER090   CR    5,3                     REG5 = REG3 ?
         BNE   ZER010                  LOOP AGAIN
ZERMVO   CLI   SWSIGNI,X'01'           ANY SIGNI CHAR FOUND ?
         BNE   MOVEOUT                 ...NO, THEN MOVEOUT
         CLI   SWPERIOD,X'01'          PERIOD PREVIOUSLY FOUND ?
         BE    ZERDEC                  ...YES, THEN ADJUST FOR DECPL
         CLI   SWCOMMA,X'01'           COMMA PREVIOUSLY FOUND ?
         BNE   SIGNCHK                 ...NO, THEN CONTINUE MOVEOUT
         SR    7,7                     RESET DEC.PL FOUND TO 0
ZERDEC   C     7,DECPL                 DEC.PL FOUND = DECPL ?
         BE    SIGNCHK                 ...YES, CONTINUE MOVEOUT
         MVC   WORKARE2(256),WORKAREA  MOVE CONTENTS OF WRKARE TO WRKA2
         MVI   WORKAREA,X'F0'          INITIALIZE WORKAREA TO X'F0'
         MVC   WORKAREA+1(255),WORKAREA .
         S     7,DECPL                 GET NO OF BYTES FOR ADJUSTMENT
         LA    8,WORKARE2
         LR    6,8
         AR    6,4
         SR    6,7
         LA    7,WORKARE2+255          SAVE LAST BYTE ADDR OF WRKARE2
         CR    6,7                     AND COMPARE IF EXCEEDED
         BH    ERROR5                  ...EXCEEDED, THEN ERROR
         LA    7,WORKAREA
         AR    7,4
ZERLP    BCTR  7,0
         BCTR  6,0
         MVC   0(1,7),0(6)             MOVE CHARACTERS UNTIL ...
         CR    6,8                     START OF WORKAREA IS REACHED
         BNE   ZERLP                   ...NO, LOOP AGAIN
SIGNCHK  CLI   TYPE,C'+'               STRING TYPE IS SIGNED NUMERIC ?
         BNE   MOVEOUT                 ...NO, THEN MOVEOUT
         LA    7,WORKAREA
         AR    7,4
         BCTR  7,0
         CLI   SWMINUS,X'00'           MINUS SW IS OFF ?
         BE    SIGNPOS                 ...YES, GOTO POSITIVE
         NI    0(7),X'DF'              SET NEGATIVE SIGN
         B     MOVEOUT                 CONTINUE MOVEOUT
SIGNPOS  NI    0(7),X'CF'              SET POSITIVE SIGN
MOVEOUT  BCTR  4,0                     DECREMENT LEN BY 1
         EX    4,VARMVC                MOVE DATA BACK INTO FIELD
         B     BACK
ERROR1   MVI   0(2),X'F1'              ERROR - INVALID FORMAT
         B     BACK
ERROR2   MVI   0(2),X'F2'              ERROR - INVALID LENGTH
         B     BACK
ERROR3   MVI   0(2),X'F3'              ERROR - DECPL > LEN
         B     BACK
ERROR4   MVI   0(2),X'F4'              ERROR - NON-NUMERIC STRING
         B     BACK
ERROR5   MVI   0(2),X'F5'              ERROR - WORKAREA EXCEEDED
BACK     L     13,SAVEREG+4
         LM    14,12,12(13)
         BR    14
*
SAVEREG  DS    18F
VARMVC   MVC   0(1,3),WORKAREA
STAT     DS    C
TYPE     DS    C
LEN      DS    F
DECPL    DS    F
WORKAREA DS    CL256
WORKARE2 DS    CL256
DWD      DS    D
FULL     DS    F
         ORG   DWD
SWNUM    DS    X
SWSIGNI  DS    X
SWPERIOD DS    X
SWCOMMA  DS    X
SWPLUS   DS    X
SWDOLLAR DS    X
SWMINUS  DS    X
SWBLA    DS    X
         DS    XL4
         ORG
*        DC    X'40'                   SPACE
*        DC    X'4B'                   . (PERIOD)
*        DC    X'4E'                   + (PLUS)
*        DC    X'5B'                   $ (DOLLAR)
*        DC    X'60'                   - (MINUS)
*        DC    X'6B'                   , (COMMA)
*        DC    X'6D'                   _ (UNDERSCORE)
         LTORG
         END   START