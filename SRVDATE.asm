* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* VIRGILIO CALIMLIM HTTP://WWW.LINKEDIN.COM/IN/VIRGILIOCALIMLIM       *
* ------------------------------------------------------------------- *
* DATE CONVERSION UTILITY AS CALLED FROM A COBOL PROGRAM              *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* MODULE   : SRVDATE                                                  *
* FUNCTIONS: 1. VALIDATES THE DATE PASSED. IF VALID, WILL RETURN THE  *
*               MONTH IN (MM) & (MMMMMMMMM), THE LITERAL DAY AND THE  *
*               JULIAN DATE.                                          *
*            2. COMPUTES THE NO. OF DAYS BETWEEN DATES.               *
*            3. FINDS THE DATE AFTER/BEFORE A NO. OF DAYS FROM FDATE. *
* OPTIONS  : INFMT = N - NUMERIC MONTH FORMAT (NN)               - F1 *
*                  = L - LITERAL MONTH FORMAT (MMMMMMMMM OR MMM) - F1 *
*                  = J - JULIAN FORMAT                           - F1 *
*                  = C - COMPUTES NO. OF DAYS BETWEEN DATES      - F2 *
*                  = F - FINDS THE DATE AFTER NDAYS FROM F-DATE  - F3 *
*            NOTE: F1, F2 & F3 REFERS TO FUNCTIONS 1, 2 & 3 RESPCTVLY *
* RESTRCTN : - YEAR IS RANGED FROM 1 TO 9999                          *
*            - NDAYS IS RANGED FROM 1 TO 3,652,059 WHICH IS THE NO.   *
*              OF DAYS FROM 01JAN0001 THRU 31DEC9999                  *
*            FOR INFMT N, L & J - STAT1, STAT2 & STAT3 CORRESPONDS TO *
*            THE EVALUATION RESULT OF YR, MM & DD OR WHICHEVER        *
*            REPRESENTS THEM DEPENDING ON THE INFMT.                  *
*            FOR INFMT C & F - STAT1, STAT2 & STAT3 CORRESPONDS TO    *
*            THE EVALUATION RESULT OF FROM-DATE, TO-DATE & NDAYS.     *
*            A VALUE OF 1 IN ANY OF THE STATKEYS WOULD MEAN AN ERROR  *
*            IN THE CORRESPONDING FIELD. A VALUE OF 0 WOULD MEAN AN   *
*            ACCEPTABLE VALUE.                                        *
*                                                         VBC-19APR89 *
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *
* COBOL FORMAT:                                                       *
*   CALL 'SRVDATE' USING SRVDATE-PARAMETERS.                          *
* PARAMETERS:                                                         *
*   01  SRVDATE-PARAMETERS.                                           *
*       03  STATKEYS.                                                 *
*           05  STAT1              PIC 9.                             *
*           05  STAT2              PIC 9.                             *
*           05  STAT3              PIC 9.                             *
*       03  INFMT                  PIC X.                             *
*           88  INFMT-VAL                    VALUE 'N' 'L' 'J'        *
*                                                  'C' 'F'.           *
*       03  FROM-DATE.                                                *
*           05  YR                 PIC 9(4).                          *
*           05  MM                 PIC 9(2).                          *
*           05  DD                 PIC 9(2).                          *
*       03  LITERAL.                                                  *
*           05  MONTH              PIC X(9).                          *
*           05  DAY                PIC X(9).                          *
*           05  JULIAN             PIC 9(3).                          *
*       03  CONVERTION REDEFINES LITERAL.                             *
*           05  TO-DATE.                                              *
*               08  TYR            PIC 9(4).                          *
*               08  TMM            PIC 9(2).                          *
*               08  TDD            PIC 9(2).                          *
*           05  NDAYS              PIC S9(8) COMP.                    *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
* EXAMPLES :
* ---------------------------------------------------------------------
*      : FROM DATE  :  LITERAL  :  LITERAL  :     :  TO DATE   :
*  FMT : YYYY MM DD :   MONTH   :    DAY    : JUL : YYYY MM DD : NDAYS
* ---------------------------------------------------------------------
*   N    1989 01 01
* =>N    1989 01 01  JANUARY      SUNDAY      001
*   L    1989    01  JAN
* =>L    1989 01 01  JANUARY      SUNDAY      001
*   L    1989    01  JANICE
* =>L    1989 01 01  JANUARY      SUNDAY      001
*   J    1989                                 001
* =>J    1989 01 01  JANUARY      SUNDAY      001
*   C    1989 01 01                                 1990 01 01
* =>C    1989 01 01                                 1990 01 01  +366
*   C    1990 01 01                                 1989 01 01
* =>C    1990 01 01                                 1989 01 01  -366
*   F    1989 01 01                                             +366
* =>F    1989 01 01                                 1990 01 01  +366
*   F    1990 01 01                                             -366
* =>F    1990 01 01                                 1989 01 01  -366
* ---------------------------------------------------------------------
*  NOTE: LINE WITH '=>' CONTAINS THE RESULTING VALUES WITH THE PREVIOUS
*        LINE CONTAINING THE INPUT PARAMETERS.
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         GBLC  &LB
*
SRVDATE  CSECT ,
R0       EQU   0
R1       EQU   1
R2       EQU   2
R3       EQU   3
R4       EQU   4
R5       EQU   5
R6       EQU   6
R7       EQU   7
R8       EQU   8
R9       EQU   9
R10      EQU   10
R11      EQU   11
R12      EQU   12
R13      EQU   13
R14      EQU   14
R15      EQU   15
*
BEGIN    STM   R14,R12,12(R13)
         BALR  R10,R0
         USING *,R10,R11
BASEADDR L     R11,NEXTBASE
         ST    R13,SAVEREG+4
         L     R2,0(R1)
         MVC   PARMS(33),0(R2)
         MVC   STATYR(3),=C'000'       INITIALIZE RETURN CODES TO 0
         XR    R12,R12                 .
*
CHKCNV   CLC   INFMT(1),=C'C'          IN-FORMAT IS CONVERTION ?
         BNE   CHKFND                  ...NO, THEN GOTO CHKFND
*
* = = =  CONVERTION STARTS HERE  = = = = = = = = = = = = = = = = = = =
*
CNVFMT   EQU   *                       CONVERT TO-DATE TO NDAYS
CNVFDT   MVC   DYR(8),NUMYR
         BAL   R9,VALDATE
         LTR   R15,R15
         BZ    CNVTDT
         MVC   STATYR(1),=C'1'
CNVTDT   MVC   DYR(8),TYR
         BAL   R9,VALDATE
         LTR   R15,R15
         BZ    CNVCON
         MVC   STATMM(1),=C'1'
         B     DATRET
CNVCON   CLC   STATYR(1),=C'1'
         BE    DATRET
* BINARY TO PACK DECIMAL  = = = = = *
*        L     R3,NDAYS             *
*        CVD   R3,DWD               *
*        MVC   COMP3(4),DWD+4       *
* PACK DECIMAL TO BINARY  = = = = = *
*        ZAP   DWD(8),COMP3         *
*        CVB   R3,DWD               *
*        ST    R3,NDAYS             *
* = = = = = = = = = = = = = = = = = *
CNVCKYR  CLC   NUMYR(8),TYR
         BH    CNVNEG
         MVC   SFYR(8),NUMYR
         MVC   STYR(8),TYR
         B     CNVCHK1
CNVNEG   MVC   SFYR(8),TYR
         MVC   STYR(8),NUMYR
CNVCHK1  PACK  SPK1(4),SFYR
         PACK  SPK2(4),STYR
         SP    SPK2(4),SPK1
         CP    SPK2(4),=P'0'           CHECK IF DATES ARE OF SAME YEAR
         BH    CNVNSYR                 ...NO, GOTO NOT SAME YEAR
CNVSMYR  MVC   DYR(8),SFYR
         BAL   R9,CHKLEAP
         BAL   R9,JULRTN
         LTR   R12,R12
         BNZ   CNVERR3
         ZAP   SPK1(4),PKDEC+2(2)
         MVC   DYR(8),STYR
         BAL   R9,JULRTN
         LTR   R12,R12
         BNZ   CNVERR3
         ZAP   NDAYS(4),PKDEC+2(2)
         SP    NDAYS(4),SPK1
         B     CNVNDYS
CNVNSYR  ZAP   DWD(8),SPK2             DATES ARE NOT OF THE SAME YEAR
         CVB   R3,DWD                  R3 = DIFF. IN YEARS IN BIN
         MVC   DYR(8),SFYR
         BAL   R9,CHKLEAP             ESTABLISH IF FEBRUARY HAS 28 DAYS
         BAL   R9,JULRTN               GET JULIAN DATE OF LOWER DATE
         LTR   R12,R12
         BNZ   CNVERR3
         ZAP   SPK1(4),PKDEC+2(2)      STORE IN SPK1 THE JULIAN DATE
         CP    MMDD2(2),=P'28'         CHECK IF LEAP YEAR
         BE    CNVNLP                  ...NO, THEN ORDINARY YEAR
         ZAP   NDAYS(4),=P'366'        ...YES, YEAR HAS 366
         B     CNVFNDY
CNVNLP   ZAP   NDAYS(4),=P'365'        ...ORDINARY, YEAR HAS 365 DAYS
CNVFNDY  SP    NDAYS(4),SPK1         SUBTRACT JULIAN FROM DAYS IN YEAR
         MVC   DYR(8),STYR
         BAL   R9,CHKLEAP             ESTABLISH IF FEBRUARY HAS 28 DAYS
         BAL   R9,JULRTN
         LTR   R12,R12
         BNZ   CNVERR3
         AP    NDAYS(4),PKDEC+2(2)
         PACK  SPK1(3),SFYR            PACK LOWER YEAR TO SPK1
CNVLOOP  BCTR  R3,0                    SUBTRACT 1 FROM DIFF. IN YEARS
         LTR   R3,R3                   ZERO YEARS LEFT ?
         BZ    CNVNDYS                 ...YES, GOTO CONVERT NDAYS
         CP    SPK1(3),=P'9999'        PACK YEAR < 9999 ?
         BNL   CNVERR3                 ...NO, THEN GOTO ERR3
         AP    SPK1(3),=P'1'           INCREMENT YEAR BY 1
         UNPK  DYR(4),SPK1(3)          UNPACK TO DYR .
         OI    DYR+3,X'F0'            . AS PARAMETER FOR SUBRTN-CHKLEAP
         BAL   R9,CHKLEAP              CALL CHKLEAP
         CP    MMDD2(2),=P'28'         CHECK IF LEAP YEAR
         BE    CNVLPN                  ...NO, THEN ORDINARY YEAR
         AP    NDAYS(4),=P'366'        ...YES, ADD 366 TO NDAYS
         B     CNVLOOP                 LOOP
CNVLPN   AP    NDAYS(4),=P'365'        ...NO, ADD 365 TO NDAYS
         B     CNVLOOP                 LOOP
CNVNDYS  AP    NDAYS(4),=P'1'          ADD 1 TO NDAYS
         CLC   NUMYR(8),TYR        CHECK IF FDATE IS BEFORE/AFTER TDATE
         BNH   CNVNOR                  ...BEFORE, GOTO NORMAL
         OI    NDAYS+3,X'0D'           ...AFTER, MAKE NDAYS NEGATIVE
CNVNOR   ZAP   DWD(8),NDAYS            CONVERT NDAYS TO BINARY
         CVB   R3,DWD                  .
         ST    R3,NDAYS                .
         B     DATRET
CNVERR3  MVC   STATDD(1),=C'1'         ERROR FROM JULRTN OR TYR > 9999
         B     DATRET
*
CHKFND   CLC   INFMT(1),=C'F'          IN-FORMAT IS FIND ?
         BNE   VALYR                   ...NO, THEN GOTO VALYR
*
FNDFMT   EQU   *                       FIND DATE AFTER/BEFORE NDAYS
FNDFDT   MVC   DYR(8),NUMYR
         BAL   R9,VALDATE
         LTR   R15,R15                 R15 = 0 ?
         BZ    FNDVNDY                 ...YES, GOTO VALIDATE NDAYS
         MVC   STATYR(1),=C'1'
* FNDVNDY  LA    R3,NDAYS                VALIDATE NDAYS
*        ICM   R3,15,0(R3)             HIGH ORDER BIT = 1 ?
*        BNP   FNDNEG                  ...YES, GOTO NEG. NDAYS RTN
FNDVNDY  CLI   NDAYS,X'7F'             HIGH ORDER BIT = 1 ?
         BH    FNDNEG                  ...YES, GOTO NEG. NDAYS RTN
* FNDPOS   CLC   NDAYS(4),=X'00FFFFFF'   NDAYS > +16777215 ?
FNDPOS   CLC   NDAYS(4),=X'0037B9DB'   NDAYS > +3652059 ?
         BH    FNDERR3                 ...YES, GOTO ERR3
         CLC   NDAYS(4),=X'00000001'   NDAYS < +1 ?
         BL    FNDERR3                 ...YES, GOTO ERR3
         CLC   STATYR(1),=C'1'         ERRONEOUS FDATE ?
         BE    DATRET                  ...YES, RETURN
         L     R3,NDAYS                LOAD NDAYS TO R3 AS WORKAREA
         MVC   DYR(8),NUMYR            MOVE FDATE TO CHKLEAP PARAMETER
         BAL   R9,CHKLEAP             ESTABLISH NO. OF DAYS IN FEBRUARY
         BAL   R9,JULRTN               GET JULIAN DATE OF FDATE
         LTR   R12,R12                 R12 NE 0 ?
         BNZ   FNDERR2                 ...YES, GOTO ERR2
         ZAP   DWD(8),PKDEC+2(2)       STORE IN DWD THE JULIAN DATE
         CVB   R4,DWD                  STORE IN R4 THE BIN JULIAN DATE
         ST    R4,SPK1                 STORE BIN JULIAN TO TMP-STORAGE
         CP    MMDD2(2),=P'28'         MONTH2 = 28 DAYS ?
         BE    FNDPOY                  ...YES, THEN ORDINARY YEAR
         LA    R4,366                  SET UP MAX. DAYS
         B     FNDPO1                  .
FNDPOY   LA    R4,365                  SET UP MAX. DAYS
FNDPO1   S     R4,SPK1                 SUBTRACT JULIAN FROM MAX. DAYS
         A     R4,=F'1'                ADD 1 TO RESULT
         CR    R3,R4                   NDAYS NOT WITHIN THIS YEAR ?
         BH    FNDPHI                  ...YES, GOTO FUTURE YEAR
FNDPLO   A     R3,SPK1            ADD JULIAN TO NDAYS GIVING NEW JULIAN
         MVC   TYR(4),NUMYR            SAME YEAR
         B     FNDPDT                  GOTO GETDATE
FNDPHI   PACK  SPK1(3),NUMYR
FNDPLP   SR    R3,R4
         CP    SPK1(3),=P'9999'        PACK YEAR < 9999 ?
         BNL   FNDERR2                 ...NO, THEN GOTO ERR2
         AP    SPK1(3),=P'1'           UPGRADE YEAR BY 1
         UNPK  DYR(4),SPK1(3)          SET UP PARAMETER FOR CHKLEAP
         OI    DYR+3,X'F0'
         BAL   R9,CHKLEAP              SET UP NO. OF DAYS IN FEBRUARY
         CP    MMDD2(2),=P'28'         MONTH2 = 28 DAYS ?
         BE    FNDPLPO                 ...YES, THEN ORDINARY YEAR
         LA    R4,366                  SET UP MAX. DAYS
         B     FNDPLP1                 .
FNDPLPO  LA    R4,365                  SET UP MAX. DAYS
FNDPLP1  CR    R3,R4                   NDAYS LEFT > MAXDAYS ?
         BH    FNDPLP                  ...YES, THEN LOOP AGAIN
         MVC   TYR(4),DYR              ...NO, THEN SET UP TO-YEAR
         B     FNDDT                   AND RETURN
* FNDNEG   CLC   NDAYS(4),=X'FF000001'   NDAYS < -16777215 ?
FNDNEG   CLC   NDAYS(4),=X'FFC84625'   NDAYS < -3652059 ?
         BL    FNDERR3                 ...YES, GOTO ERR3
         CLC   STATYR(1),=C'1'         ERRONEOUS FDATE ?
         BE    DATRET                  ...YES, RETURN
         L     R3,NDAYS                LOAD NDAYS TO R3 AS WORKAREA
         MVC   DYR(8),NUMYR            MOVE FDATE TO CHKLEAP PARAMETER
         BAL   R9,CHKLEAP             ESTABLISH NO. OF DAYS IN FEBRUARY
         BAL   R9,JULRTN               GET JULIAN DATE OF FDATE
         LTR   R12,R12                 R12 NE 0 ?
         BNZ   FNDERR2                 ...YES, GOTO ERR2
         ZAP   DWD(8),PKDEC+2(2)       STORE IN DWD THE JULIAN DATE
         CVB   R4,DWD                  STORE IN R4 THE BIN JULIAN DATE
         AR    R3,R4                   ADD BIN-JULIAN TO NDAYS
         BM    FNDNLO                  RESULT IS NEGATIVE ?
FNDNHI   MVC   TYR(4),NUMYR            ...NO, THEN SAME YEAR
         B     FNDNDT                  GOTO GETDATE
FNDNLO   PACK  SPK1(3),NUMYR
FNDNLP   CP    SPK1(3),=P'1'           PACK YEAR > 1 ?
         BNH   FNDERR2                 ...NO, GOTO ERR2
         SP    SPK1(3),=P'1'           DECREMENT YEAR BY 1
         UNPK  DYR(4),SPK1(3)          SET UP PARAMETER FOR CHKLEAP
         OI    DYR+3,X'F0'             .
         BAL   R9,CHKLEAP              SET UP NO. OF DAYS IN FEBRUARY
         CP    MMDD2(2),=P'28'         MONTH2 = 28 DAYS ?
         BE    FNDNLPO                 ...YES, THEN ORDINARY YEAR
         LA    R4,366                  SET UP MAX. DAYS
         B     FNDNLP1                 .
FNDNLPO  LA    R4,365                  SET UP MAX. DAYS
FNDNLP1  AR    R3,R4                   ADD MAX. DAYS TO NDAYS LEFT
         BM    FNDNLP
         MVC   TYR(4),DYR
FNDNDT   A     R3,=F'1'                ADD 1 TO NEW JULIAN
         B     FNDDT
FNDPDT   BCTR  R3,0                    SUBTRACT 1 FROM NEW JULIAN
FNDDT    CVD   R3,DWD
         UNPK  DJUL(3),DWD+6(2)
         OI    DJUL+2,X'F0'
         BAL   R9,GETDATE
         MVC   TMM(4),DMM
         B     DATRET
FNDERR2  MVC   STATMM(1),=C'1'      ERROR FROM JULRTN OR TYR < 1 > 9999
         B     DATRET
FNDERR3  MVC   STATDD(1),=C'1'         ERROR - NDAYS NOT WITHIN RANGE
         B     DATRET
*
* = = =  LITERALS START HERE  = = = = = = = = = = = = = = = = = = = = =
*
VALYR    MNUMVAL NUMYR,4,=P'1',=P'9999' VALIDATE YEAR
         LTR   R15,R15
         BNZ   YRERR
         MVC   DYR(4),NUMYR
         BAL   R9,CHKLEAP
         BAL   R9,FNDAY
         B     CHKJUL
YRERR    MVC   STATYR(1),=C'1'
         LA    R12,9
*
CHKJUL   CLC   INFMT(1),=C'J'          IN-FORMAT IS JULIAN ?
         BNE   VALDD                   ...NO, THEN GOTO VALIDATE DD
         CP    MMDD2(2),=P'28'
         BE    J365
J366     MNUMVAL JULIAN,3,=P'1',=P'366' VALIDATE JULIAN
         B     JULR15
J365     MNUMVAL JULIAN,3,=P'1',=P'365' VALIDATE JULIAN
JULR15   LTR   R15,R15
         BZ    JULR12
         MVC   STATDD(1),=C'1'
         B     DATRET
JULR12   LTR   R12,R12
         BNZ   DATRET
         MVC   DJUL(3),JULIAN
         BAL   R9,GETDATE
         MVC   NUMMM(4),DMM
         BAL   R9,LMONRTN
         BAL   R9,LDAYRTN
         B     DATRET
*
VALDD    MNUMVAL NUMDD,2,=P'1',=P'31'   VALIDATE NUMERIC DAY
         LTR   R15,R15
         BZ    NMONCHK
         MVC   STATDD(1),=C'1'
         LA    R12,9
*
NMONCHK  CLC   INFMT(1),=C'N'          IN-FORMAT IS NUMERIC MONTH ?
         BNE   LMONCHK                 ...NO, THEN GOTO CHECK IF LITRL
NMVALMM  MNUMVAL NUMMM,2,=P'1',=P'12'   VALIDATE NUMERIC MONTH
         LTR   R15,R15
         BZ    NMJUL
         MVC   STATMM(1),=C'1'
         B     DATRET
NMJUL    LTR   R12,R12
         BNZ   DATRET
         MVC   DMM(4),NUMMM
         BAL   R9,JULRTN
         LTR   R12,R12
         BZ    NMOK
         MVC   STATDD(1),=C'1'
         B     DATRET
NMOK     MVC   JULIAN(3),DJUL
         BAL   R9,LMONRTN
         BAL   R9,LDAYRTN
         B     DATRET
*
LMONCHK  CLC   INFMT(1),=C'L'          IN-FORMAT IS LITERAL MONTH ?
         BNE   INFEROR                 ...NO, THEN GOTO ERROR ROUTINE
LMC1     CLC   LMON(3),LMON1
         BNE   LMC2
         MVC   NUMMM(2),=C'01'
         B     LMCJUL
LMC2     CLC   LMON(3),LMON2
         BNE   LMC3
         MVC   NUMMM(2),=C'02'
         B     LMCJUL
LMC3     CLC   LMON(3),LMON3
         BNE   LMC4
         MVC   NUMMM(2),=C'03'
         B     LMCJUL
LMC4     CLC   LMON(3),LMON4
         BNE   LMC5
         MVC   NUMMM(2),=C'04'
         B     LMCJUL
LMC5     CLC   LMON(3),LMON5
         BNE   LMC6
         MVC   NUMMM(2),=C'05'
         B     LMCJUL
LMC6     CLC   LMON(3),LMON6
         BNE   LMC7
         MVC   NUMMM(2),=C'06'
         B     LMCJUL
LMC7     CLC   LMON(3),LMON7
         BNE   LMC8
         MVC   NUMMM(2),=C'07'
         B     LMCJUL
LMC8     CLC   LMON(3),LMON8
         BNE   LMC9
         MVC   NUMMM(2),=C'08'
         B     LMCJUL
LMC9     CLC   LMON(3),LMON9
         BNE   LMC10
         MVC   NUMMM(2),=C'09'
         B     LMCJUL
LMC10    CLC   LMON(3),LMON10
         BNE   LMC11
         MVC   NUMMM(2),=C'10'
         B     LMCJUL
LMC11    CLC   LMON(3),LMON11
         BNE   LMC12
         MVC   NUMMM(2),=C'11'
         B     LMCJUL
LMC12    CLC   LMON(3),LMON12
         BNE   LMCERR
         MVC   NUMMM(2),=C'12'
LMCJUL   MVC   DMM(4),NUMMM
         BAL   R9,JULRTN
         LTR   R12,R12
         BZ    LMCOK
         MVC   STATDD(1),=C'1'
         B     DATRET
LMCOK    MVC   JULIAN(3),DJUL
         BAL   R9,LDAYRTN
         B     DATRET
*
INFEROR  MVC   STATYR(1),=C'1'
         MVC   STATDD(1),=C'1'
LMCERR   MVC   STATMM(1),=C'1'
*
DATRET   MVC   0(33,R2),PARMS
         XR    R15,R15
         CLC   PARMS(3),=C'000'
         BE    RETURN
         LA    R15,16
RETURN   L     R13,SAVEREG+4      BACK CHAIN RETAINING R15
         L     R14,12(R13)        .
         LM    R0,R12,20(R13)     .
         BR    R14                .
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   GLOBAL DATA USED BY ALL INTERNAL ROUTINES                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
NEXTBASE DC    A(BASEADDR+4096)
SAVEREG  DC    18F'0'
PARMS    EQU   *
STATYR   DS    CL1
STATMM   DS    CL1
STATDD   DS    CL1
INFMT    DS    CL1
NUMYR    DS    CL4
NUMMM    DS    CL2
NUMDD    DS    CL2
LMON     DS    CL9
LDAY     DS    CL9
JULIAN   DS    CL3
         ORG   LMON
TYR      DS    CL4
TMM      DS    CL2
TDD      DS    CL2
NDAYS    DS    F
         DS    CL9
*
         ORG
DWD      DS    D
SPK1     DS    F
SPK2     DS    F
DYR      DS    CL4
DMM      DS    CL2
DDD      DS    CL2
DJUL     DS    CL3
SFYR     DS    CL4
SFMM     DS    CL2
SFDD     DS    CL2
STYR     DS    CL4
STMM     DS    CL2
STDD     DS    CL2
FDAY     DS    PL1
PKDEC    DS    PL4
DIV4     DS    PL4
DIV1C    DS    PL5
DIV4C    DS    PL5
*
LMONTAB  DS    0CL108
LMON1    DC    CL9'JANUARY'
LMON2    DC    CL9'FEBRUARY'
LMON3    DC    CL9'MARCH'
LMON4    DC    CL9'APRIL'
LMON5    DC    CL9'MAY'
LMON6    DC    CL9'JUNE'
LMON7    DC    CL9'JULY'
LMON8    DC    CL9'AUGUST'
LMON9    DC    CL9'SEPTEMBER'
LMON10   DC    CL9'OCTOBER'
LMON11   DC    CL9'NOVEMBER'
LMON12   DC    CL9'DECEMBER'
*
MMDDTAB  DS    0CL24
MMDD1    DC    PL2'31'
MMDD2    DC    PL2'28'
MMDD3    DC    PL2'31'
MMDD4    DC    PL2'30'
MMDD5    DC    PL2'31'
MMDD6    DC    PL2'30'
MMDD7    DC    PL2'31'
MMDD8    DC    PL2'31'
MMDD9    DC    PL2'30'
MMDD10   DC    PL2'31'
MMDD11   DC    PL2'30'
MMDD12   DC    PL2'31'
*
LDAYTAB  DS    0CL63
LDAY1    DC    CL9'SUNDAY'
LDAY2    DC    CL9'MONDAY'
LDAY3    DC    CL9'TUESDAY'
LDAY4    DC    CL9'WEDNESDAY'
LDAY5    DC    CL9'THURSDAY'
LDAY6    DC    CL9'FRIDAY'
LDAY7    DC    CL9'SATURDAY'
*
         LTORG
*
         CNOP  0,4
CHKLEAP  EQU   *                        CHECK YEAR IF LEAP-YEAR
         PACK  DIV4(4),DYR
         PACK  DIV1C(5),DYR
         PACK  DIV4C(5),DYR
         DP    DIV4(4),=P'4'
         DP    DIV1C(5),=P'100'
         DP    DIV4C(5),=P'400'
         MVC   MMDD2(2),=P'28'         INITIALIZE FEB TO 28 DAYS
         CP    DIV4+3(1),=P'0'         YEAR MULTIPLE OF 4 ?
         BNE   CHK400                  ...NO, GOTO CHECK IF MUL 400
         CP    DIV1C+3(2),=P'0'        YEAR MULTIPLE OF 100 ?
         BNE   LEAPYR                  ...NO, GOTO LEAP YEAR
CHK400   CP    DIV4C+3(2),=P'0'        YEAR MULTIPLE OF 400 ?
         BNE   LEAPRET                 ...NO, RETURN
LEAPYR   MVC   MMDD2(2),=P'29'         SET FEB TO 29 DAYS (LEAP YEAR)
LEAPRET  BR    R9                      RETURN TO CALLER
*
JULRTN   EQU   *                       FIND THE JULIAN DATE
         PACK  PKDEC(2),DDD
         PACK  PKDEC+2(2),DDD
JR1      CLC   DMM(2),=C'01'
         BNE   JR2
         CP    PKDEC(2),MMDD1
         BH    JRERR
         B     JREND
JR2      AP    PKDEC+2(2),MMDD1
         CLC   DMM(2),=C'02'
         BNE   JR3
         CP    PKDEC(2),MMDD2
         BH    JRERR
         B     JREND
JR3      AP    PKDEC+2(2),MMDD2
         CLC   DMM(2),=C'03'
         BNE   JR4
         CP    PKDEC(2),MMDD3
         BH    JRERR
         B     JREND
JR4      AP    PKDEC+2(2),MMDD3
         CLC   DMM(2),=C'04'
         BNE   JR5
         CP    PKDEC(2),MMDD4
         BH    JRERR
         B     JREND
JR5      AP    PKDEC+2(2),MMDD4
         CLC   DMM(2),=C'05'
         BNE   JR6
         CP    PKDEC(2),MMDD5
         BH    JRERR
         B     JREND
JR6      AP    PKDEC+2(2),MMDD5
         CLC   DMM(2),=C'06'
         BNE   JR7
         CP    PKDEC(2),MMDD6
         BH    JRERR
         B     JREND
JR7      AP    PKDEC+2(2),MMDD6
         CLC   DMM(2),=C'07'
         BNE   JR8
         CP    PKDEC(2),MMDD7
         BH    JRERR
         B     JREND
JR8      AP    PKDEC+2(2),MMDD7
         CLC   DMM(2),=C'08'
         BNE   JR9
         CP    PKDEC(2),MMDD8
         BH    JRERR
         B     JREND
JR9      AP    PKDEC+2(2),MMDD8
         CLC   DMM(2),=C'09'
         BNE   JR10
         CP    PKDEC(2),MMDD9
         BH    JRERR
         B     JREND
JR10     AP    PKDEC+2(2),MMDD9
         CLC   DMM(2),=C'10'
         BNE   JR11
         CP    PKDEC(2),MMDD10
         BH    JRERR
         B     JREND
JR11     AP    PKDEC+2(2),MMDD10
         CLC   DMM(2),=C'11'
         BNE   JR12
         CP    PKDEC(2),MMDD11
         BH    JRERR
         B     JREND
JR12     AP    PKDEC+2(2),MMDD11
         CP    PKDEC(2),MMDD12
         BH    JRERR
         B     JREND
JRERR    LA    R12,9
         BR    R9
JREND    UNPK  DJUL(3),PKDEC+2(2)
         OI    DJUL+2,X'F0'
         BR    R9                      RETURN TO CALLER
*
GETDATE  EQU   *                       FIND THE NUMERIC DAY
         PACK  PKDEC(2),DJUL
MD1      CP    PKDEC(2),MMDD1
         BH    MD2
         MVC   DMM(2),=C'01'
         B     GETDAY
MD2      SP    PKDEC(2),MMDD1
         CP    PKDEC(2),MMDD2
         BH    MD3
         MVC   DMM(2),=C'02'
         B     GETDAY
MD3      SP    PKDEC(2),MMDD2
         CP    PKDEC(2),MMDD3
         BH    MD4
         MVC   DMM(2),=C'03'
         B     GETDAY
MD4      SP    PKDEC(2),MMDD3
         CP    PKDEC(2),MMDD4
         BH    MD5
         MVC   DMM(2),=C'04'
         B     GETDAY
MD5      SP    PKDEC(2),MMDD4
         CP    PKDEC(2),MMDD5
         BH    MD6
         MVC   DMM(2),=C'05'
         B     GETDAY
MD6      SP    PKDEC(2),MMDD5
         CP    PKDEC(2),MMDD6
         BH    MD7
         MVC   DMM(2),=C'06'
         B     GETDAY
MD7      SP    PKDEC(2),MMDD6
         CP    PKDEC(2),MMDD7
         BH    MD8
         MVC   DMM(2),=C'07'
         B     GETDAY
MD8      SP    PKDEC(2),MMDD7
         CP    PKDEC(2),MMDD8
         BH    MD9
         MVC   DMM(2),=C'08'
         B     GETDAY
MD9      SP    PKDEC(2),MMDD8
         CP    PKDEC(2),MMDD9
         BH    MD10
         MVC   DMM(2),=C'09'
         B     GETDAY
MD10     SP    PKDEC(2),MMDD9
         CP    PKDEC(2),MMDD10
         BH    MD11
         MVC   DMM(2),=C'10'
         B     GETDAY
MD11     SP    PKDEC(2),MMDD10
         CP    PKDEC(2),MMDD11
         BH    MD12
         MVC   DMM(2),=C'11'
         B     GETDAY
MD12     SP    PKDEC(2),MMDD11
         MVC   DMM(2),=C'12'
GETDAY   UNPK  DDD(2),PKDEC(2)
         OI    DDD+1,X'F0'
         BR    R9                      RETURN TO CALLER
*
VALDATE  EQU   *                       DATE VALIDATION
VALDYR   MNUMVAL DYR,4,=P'1',=P'9999'
         LTR   R15,R15
         BNZ   VALDRET
VALDMM   MNUMVAL DMM,2,=P'1',=P'12'
         LTR   R15,R15
         BNZ   VALDRET
         PACK  PKDEC(2),DDD
VD1      CLC   DMM(2),=C'01'
         BNE   VD2
         CP    PKDEC(2),MMDD1
         BH    VALDERR
         B     VALDRET
VD2      CLC   DMM(2),=C'02'
         BNE   VD3
         MSTR  VD,R9
         BAL   R9,CHKLEAP
         L     R9,&LB
         CP    PKDEC(2),MMDD2
         BH    VALDERR
         B     VALDRET
VD3      CLC   DMM(2),=C'03'
         BNE   VD4
         CP    PKDEC(2),MMDD3
         BH    VALDERR
         B     VALDRET
VD4      CLC   DMM(2),=C'04'
         BNE   VD5
         CP    PKDEC(2),MMDD4
         BH    VALDERR
         B     VALDRET
VD5      CLC   DMM(2),=C'05'
         BNE   VD6
         CP    PKDEC(2),MMDD5
         BH    VALDERR
         B     VALDRET
VD6      CLC   DMM(2),=C'06'
         BNE   VD7
         CP    PKDEC(2),MMDD6
         BH    VALDERR
         B     VALDRET
VD7      CLC   DMM(2),=C'07'
         BNE   VD8
         CP    PKDEC(2),MMDD7
         BH    VALDERR
         B     VALDRET
VD8      CLC   DMM(2),=C'08'
         BNE   VD9
         CP    PKDEC(2),MMDD8
         BH    VALDERR
         B     VALDRET
VD9      CLC   DMM(2),=C'09'
         BNE   VD10
         CP    PKDEC(2),MMDD9
         BH    VALDERR
         B     VALDRET
VD10     CLC   DMM(2),=C'10'
         BNE   VD11
         CP    PKDEC(2),MMDD10
         BH    VALDERR
         B     VALDRET
VD11     CLC   DMM(2),=C'11'
         BNE   VD12
         CP    PKDEC(2),MMDD11
         BH    VALDERR
         B     VALDRET
VD12     CLC   DMM(2),=C'12'
         BNE   VALDERR
         CP    PKDEC(2),MMDD12
         BH    VALDERR
         B     VALDRET
VALDERR  LA    R15,9
VALDRET  BR    R9                      RETURN TO CALLER
*
FNDAY    EQU   *                       FIND FIRST DAY OF THE YEAR
         PACK  DIV4(4),NUMYR
         PACK  DIV1C(5),NUMYR
         PACK  DIV4C(5),NUMYR
         SP    DIV4(4),=P'1'
         SP    DIV1C(5),=P'1'
         SP    DIV4C(5),=P'1'
         DP    DIV4(4),=P'4'
         DP    DIV1C(5),=P'100'
         DP    DIV4C(5),=P'400'
         PACK  PKDEC(4),NUMYR
         AP    PKDEC(4),=P'1'
         AP    PKDEC(4),DIV4(3)
         AP    PKDEC(4),DIV4C(3)
         SP    PKDEC(4),DIV1C(3)
         DP    PKDEC(4),=P'7'
         MVC   FDAY(1),PKDEC+3
         BR    R9                      RETURN TO CALLER
*
LDAYRTN  EQU   *                       FIND THE LITERAL DAY
         PACK  PKDEC(3),JULIAN
         AP    PKDEC(3),FDAY
         SP    PKDEC(3),=P'1'
         DP    PKDEC(3),=P'7'
         CP    PKDEC+2(1),=P'0'
         BE    LD7
LD1      CP    PKDEC+2(1),=P'1'
         BNE   LD2
         MVC   LDAY(9),LDAY1
         B     LDRET
LD2      CP    PKDEC+2(1),=P'2'
         BNE   LD3
         MVC   LDAY(9),LDAY2
         B     LDRET
LD3      CP    PKDEC+2(1),=P'3'
         BNE   LD4
         MVC   LDAY(9),LDAY3
         B     LDRET
LD4      CP    PKDEC+2(1),=P'4'
         BNE   LD5
         MVC   LDAY(9),LDAY4
         B     LDRET
LD5      CP    PKDEC+2(1),=P'5'
         BNE   LD6
         MVC   LDAY(9),LDAY5
         B     LDRET
LD6      CP    PKDEC+2(1),=P'6'
         BNE   LD7
         MVC   LDAY(9),LDAY6
         B     LDRET
LD7      MVC   LDAY(9),LDAY7
LDRET    BR    R9                      RETURN TO CALLER
*
LMONRTN  EQU   *                       FIND THE LITERAL MONTH
LM1      CLC   NUMMM(2),=C'01'
         BNE   LM2
         MVC   LMON(9),LMON1
         B     LMRET
LM2      CLC   NUMMM(2),=C'02'
         BNE   LM3
         MVC   LMON(9),LMON2
         B     LMRET
LM3      CLC   NUMMM(2),=C'03'
         BNE   LM4
         MVC   LMON(9),LMON3
         B     LMRET
LM4      CLC   NUMMM(2),=C'04'
         BNE   LM5
         MVC   LMON(9),LMON4
         B     LMRET
LM5      CLC   NUMMM(2),=C'05'
         BNE   LM6
         MVC   LMON(9),LMON5
         B     LMRET
LM6      CLC   NUMMM(2),=C'06'
         BNE   LM7
         MVC   LMON(9),LMON6
         B     LMRET
LM7      CLC   NUMMM(2),=C'07'
         BNE   LM8
         MVC   LMON(9),LMON7
         B     LMRET
LM8      CLC   NUMMM(2),=C'08'
         BNE   LM9
         MVC   LMON(9),LMON8
         B     LMRET
LM9      CLC   NUMMM(2),=C'09'
         BNE   LM10
         MVC   LMON(9),LMON9
         B     LMRET
LM10     CLC   NUMMM(2),=C'10'
         BNE   LM11
         MVC   LMON(9),LMON10
         B     LMRET
LM11     CLC   NUMMM(2),=C'11'
         BNE   LM12
         MVC   LMON(9),LMON11
         B     LMRET
LM12     MVC   LMON(9),LMON12
LMRET    BR    R9                      RETURN TO CALLER
*
         LTORG
         END   BEGIN
