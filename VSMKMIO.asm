* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
* STANDARD IO ROUTINE FOR A VSAM KSDS FILE CALLED BY A COBOL PROGRAM  *
* = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = *
* COBOL FORMAT:                                                       *
*   CALL 'VSMKMIO' USING REQRD-PARMS RECORD                           *
* PARAMETERS:                                                         *
*   01  REQRD-PARMS.                                                  *
*       03  STATUS-KEY.                                               *
*           05  OPENTYP            PIC X.                             *
*               88  OPENTYP-OUTPUT           VALUE 'O'.               *
*           05  FILLER             PIC X.                             *
*       03  XSTAT-RC               PIC 9(4)  COMP.                    *
*       03  XSTAT-15               PIC 9(4)  COMP.                    *
*       03  REQUEST                PIC X(4).                          *
*           88  REQUEST-VALID                VALUE 'OPEN' 'CLOS'      *
*                                                  'READ' 'RDNX'      *
*                                                  'WRIT' 'UPDT'      *
*                                                  'DELT' 'RDPV'.     *
*       03  DDNAME                 PIC X(8).                          *
*   01  RECORD                     UPTO 4096 BYTES ONLY.              *
*   01  FILE-ATTRIBUTES REDEFINES RECORD.                             *
*       03  KEYLEN                 PIC 9(4)  COMP.                    *
*       03  KEYOFSET               PIC 9(4)  COMP.                    *
*       03  RECLEN                 PIC 9(4)  COMP.                    *
* RESTRICTIONS:                                                       *
*   1. MAXIMUM LOGICAL RECORD LENGTH IS 4096.                         *
*   2. MAXIMUM KEY LENGTH IS 40.                                      *
*   3. MAXIMUM NO. OF FILES THAT CAN BE PROCESSED IS 5.               *
*   4. REQRD-PARMS ARE ALWAYS REQUIRED.                               *
*   5. FILE-ATTRIBUTES ARE REQUIRED DURING OPEN ONLY.                 *
*   6. RECORD IS REQUIRED DURING I-O REQUESTS.                        *
*   7. DEFAULT OPENTYPE IS INPUT.                                     *
*   8. FILE/S SHOULD BE PRE-INITIALIZED.                              *
*                                                         VBC-01JAN87 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*
         MACRO
&LBL     MMVC  &RECV,&LEN,&OFSET,&OFSET1,&SEND
&LBL     MVC   LEN(4),&LEN
         MVC   LEN+4(4),&OFSET
         MVC   LEN+8(4),&OFSET1
         LA    R5,LEN
         LA    R6,&RECV
         LA    R7,&SEND
         L     R15,=A(MVCRTN)
         BALR  R14,R15
         MEND
*
         MACRO
&LBL     MCLC  &RECV,&LEN,&OFSET,&OFSET1,&SEND
&LBL     MVC   LEN(4),&LEN
         MVC   LEN+4(4),&OFSET
         MVC   LEN+8(4),&OFSET1
         LA    R5,LEN
         LA    R6,&RECV
         LA    R7,&SEND
         L     R15,=A(CLCRTN)
         BALR  R14,R15
         MEND
*
         MACRO
         MDS   &N
DDNAME&N DC    8C' '
KEYLEN&N DC    F'0'
KYOFST&N DC    F'0'
RECLEN&N DC    F'0'
ACB&N    DS    F
RPL&N    DS    F
PRVREQ&N DS    CL4
FNDKEY&N DS    CL40
         MEND
*
         MACRO
         MDD   &N
         CLC   DDNAME&N.(8),CL8BLA
         BE    CKFVAC
         MEND
*
         MACRO
         MCKF  &N,&M
CKF&N    CLC   DDNAME&N.(8),CL8BLA
         BE    &M
         CLC   DDNAME&N.(8),DDNAME
         BNE   &M
         MVC   KEYLEN(4),KEYLEN&N
         MVC   KEYOFSET(4),KYOFST&N
         MVC   RECLEN(4),RECLEN&N
         L     R5,ACB&N
         ST    R5,ACB
         L     R5,RPL&N
         ST    R5,RPL
         MVC   PREVREQ(4),PRVREQ&N
         MMVC  FOUNDKEY,KEYLEN,FW0,FW0,FNDKEY&N
         B     CKFIN
         MEND
*
         MACRO
         MSAV  &N,&M
MEM&N    CLC   DDNAME&N.(8),DDNAME
         BNE   &M
         MVC   PRVREQ&N.(4),REQUEST
         MMVC  FNDKEY&N,KEYLEN,FW0,FW0,FOUNDKEY
         B     MEMRET
         MEND
*
         MACRO
         MGEN  &N,&M
GENFL&N  CLC   DDNAME&N.(8),CL8BLA
         BNE   &M
         ST    R5,ACB&N
         ST    R1,RPL&N
         MVC   DDNAME&N.(8),DDNAME
         MVC   KEYLEN&N.(4),KEYLEN
         MVC   KYOFST&N.(4),KEYOFSET
         MVC   RECLEN&N.(4),RECLEN
         B     GENRET
         MEND
*
         MACRO
         MBLA  &N,&M
BLAF&N   CLC   DDNAME&N.(8),DDNAME
         BNE   &M
         MVC   DDNAME&N.(8),CL8BLA
         B     BLASTAT
         MEND
*
VSMKMIO  CSECT ,
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
         USING BASEADDR,R10,R11,R12
BASEADDR LM    R11,R12,NEXTBASE
         ST    R13,SAVEVSM+4
         LR    R8,R13
         LA    R13,SAVEVSM
         ST    R13,8(,R8)
         LM    R2,R3,0(R1)
*
         MVC   REQUEST(12),6(R2)
         CLC   PREVDD(8),DDNAME
         BE    REQOPEN
         L     R15,=A(CHKFILE)
         BALR  R14,R15
*
REQOPEN  CLC   OPEN(4),REQUEST
         BNE   CHKOPEN
         L     R15,=A(OPENRTN)
         BALR  R14,R15
         B     ENDCASE
*
CHKOPEN  TM    FILESTAT,INFLAG
         BNO   ERRFLNO
*
REQCLOS  CLC   CLOS(4),REQUEST
         BNE   REQREAD
         L     R15,=A(CLOSRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQREAD  CLC   READ(4),REQUEST
         BNE   REQRDNX
         L     R15,=A(READRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQRDNX  CLC   RDNX(4),REQUEST
         BNE   REQWRIT
         L     R15,=A(RDNXRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQWRIT  CLC   WRIT(4),REQUEST
         BNE   REQUPDT
         L     R15,=A(WRITRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQUPDT  CLC   UPDT(4),REQUEST
         BNE   REQDELT
         L     R15,=A(UPDTRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQDELT  CLC   DELT(4),REQUEST
         BNE   REQRDPV
         L     R15,=A(DELTRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQRDPV  CLC   RDPV(4),REQUEST
         BNE   REQEROR
         L     R15,=A(RDPVRTN)
         BALR  R14,R15
         B     ENDCASE
*
REQEROR  EQU   *                        ERROR - INVALID REQUEST
         LA    R15,X'04'
         STH   R15,RPLRC
         LA    R15,X'62'                DECIMAL 98
         ST    R15,FDBK
         B     ENDCASE
*
ERRFLNO  EQU   *                        ERROR - REQUESTED FILE NOT OPEN
         LA    R15,X'04'
         STH   R15,RPLRC
         LA    R15,X'05'
         ST    R15,FDBK
         MVI   REQUEST,X'40'
*
ENDCASE  EQU   *
         L     R15,=A(CVTCHAR)
         BALR  R14,R15
         L     R15,=A(SAVMEMO)
         BALR  R14,R15
*
         L     R13,SAVEVSM+4
         LM    R14,R12,12(R13)
         SR    R15,R15
         BR    14
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   GLOBAL DATA USED BY ALL INTERNAL ROUTINES                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
NEXTBASE DC    A(BASEADDR+4096,BASEADDR+2*4096)
SAVEVSM  DC    18F'0'
OPENRC   DS    H
CLOSRC   DS    H
RPLRC    DS    H
SHOWEROR DS    0F
ERROR    DS    F
SHOWFDBK DS    0F
FDBK     DS    F
PAKNBR   DS    D
FWRESULT DS    CL15
*
OPEN     DC    CL4'OPEN'
CLOS     DC    CL4'CLOS'
READ     DC    CL4'READ'
RDNX     DC    CL4'RDNX'
WRIT     DC    CL4'WRIT'
UPDT     DC    CL4'UPDT'
DELT     DC    CL4'DELT'
RDPV     DC    CL4'RDPV'
*
FILESTAT DC    B'00000000'              FILE OPEN/CLOSE SWITCH
FILEFULL DC    B'00000000'              FILE TABLE FULL SWITCH
INFLAG   EQU   B'10111111'              ON SWITCH
OUTFLAG  EQU   B'01000000'              OFF SWITCH
*
         DS    0F
REQUEST  DS    CL4
DDNAME   DS    CL8
KEYLEN   DS    F
KEYOFSET DS    F
RECLEN   DS    F
ACB      DS    F
RPL      DS    F
FILEKEY  DS    CL40
FOUNDKEY DS    CL40
SERCHKEY DS    CL40
IOBUFFER DS    CL4096
LEN      DS    F
OFSET    DS    F
OFSET1   DS    F
PREVREQ  DS    CL4
PREVDD   DC    8C' '
FW0      DC    F'0'
CL8BLA   DC    8C' '
*
         MDS   1
         MDS   2
         MDS   3
         MDS   4
         MDS   5
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   CHECK-FILE  ROUTINE                                               *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CHKFILE  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R8,R0
         USING *,R8
*
         ST    R13,SAVECHK+4
         LR    R7,R13
         LA    R13,SAVECHK
         ST    R13,8(,R7)
*
         MVI   FILESTAT,OUTFLAG
         MVI   FILEFULL,INFLAG
*
         MDD   1
         MDD   2
         MDD   3
         MDD   4
         MDD   5
         BNE   CKF1
CKFVAC   MVI   FILEFULL,OUTFLAG
         MCKF  1,CKF2
         MCKF  2,CKF3
         MCKF  3,CKF4
         MCKF  4,CKF5
         MCKF  5,CKFRET
CKFIN    MVI   FILESTAT,INFLAG
CKFRET   EQU   *
         L     R13,SAVECHK+4
         LM    R14,R12,12(R13)
         BR    R14
*
SAVECHK  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   SAVE-MEMORY  ROUTINE                                              *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
SAVMEMO  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R8,R0
         USING *,R8
*
         ST    R13,SAVEMEM+4
         LR    R7,R13
         LA    R13,SAVEMEM
         ST    R13,8(,R7)
*
         MSAV  1,MEM2
         MSAV  2,MEM3
         MSAV  3,MEM4
         MSAV  4,MEM5
         MSAV  5,MEMRET
MEMRET   EQU   *
         MVC   PREVDD(8),DDNAME
         MVC   PREVREQ(4),REQUEST
         L     R13,SAVEMEM+4
         LM    R14,R12,12(R13)
         BR    R14
*
SAVEMEM  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   OPEN  ROUTINE                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
OPENRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVEOPE+4
         LR    R8,R13
         LA    R13,SAVEOPE
         ST    R13,8(,R8)
*
         TM    FILESTAT,INFLAG
         BO    OPNOPEN
         TM    FILEFULL,INFLAG
         BO    OPNFULL
         MVC   KEYLEN(4),FW0
         MVC   KEYLEN+2(2),0(R3)
         MVC   KEYOFSET(4),FW0
         MVC   KEYOFSET+2(2),2(R3)
         MVC   RECLEN(4),FW0
         MVC   RECLEN+2(2),4(R3)
*
         CLC   KEYLEN(4),=F'40'
         BH    OPNVLNE
         CLC   KEYLEN(4),=F'1'
         BL    OPNVLNE
*
         CLC   KEYOFSET(4),=F'4095'
         BH    OPNVLNE
*
         CLC   RECLEN(4),=F'4096'
         BH    OPNVLNE
         CLC   RECLEN(4),=F'1'
         BL    OPNVLNE
*
         L     R15,=A(GENFILE)
         BALR  R14,R15
         LTR   R15,R15
         BNZ   OPNRET
         L     R5,ACB
         CLC   0(1,R2),=C'O'
         BNE   OPENIN
         MODCB ACB=(R5),MACRF=OUT
         B     OPNCKRC
OPENIN   MODCB ACB=(R5),MACRF=IN
OPNCKRC  LTR   R15,R15
         BNZ   OPNERR
         OPEN  (R5)
         LTR   R15,R15
         BNZ   OPNERR
         OI    FILESTAT,INFLAG
         B     OPNSHOW
OPNERR   L     R7,=A(BLANKDD)
         BALR  R14,R7
OPNSHOW  EQU   *
         STH   R15,OPENRC
         SHOWCB ACB=(R5),AREA=SHOWEROR,LENGTH=4,FIELDS=(ERROR)
         B     OPNRET
OPNOPEN  EQU   *                        ERROR - FILE ALREADY OPEN
         LA    R15,X'08'
         STH   R15,OPENRC
         LA    R15,X'04'
         ST    R15,ERROR
         B     OPNRET
OPNVLNE  EQU   *                        ERROR - INVALID NUMERIC PARAMTR
         LA    R15,X'08'
         STH   R15,RPLRC
         LA    R15,X'62'                DECIMAL 98
         ST    R15,FDBK
         MVI   REQUEST,X'40'
         B     OPNRET
OPNFULL  EQU   *                        ERROR - FILE ALLOCATION FULL
         LA    R15,X'0C'
         STH   R15,RPLRC
         LA    R15,X'62'                DECIMAL 98
         ST    R15,FDBK
         MVI   REQUEST,X'40'
OPNRET   EQU   *
         L     R13,SAVEOPE+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVEOPE  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   GENERATE-CONTROL-BLOCKS-OF FILE  ROUTINE                          *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
GENFILE  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R8,R0
         USING *,R8
*
         ST    R13,SAVEGEN+4
         LR    R7,R13
         LA    R13,SAVEGEN
         ST    R13,8(,R7)
*
         GENCB BLK=ACB,AM=VSAM,MACRF=(KEY,NRS),DDNAME=(*,DDNAME)
         LTR   R15,R15
         BNZ   GENFACB
         LR    R5,R1
         GENCB BLK=RPL,ACB=(R5),OPTCD=MVE,ARG=FILEKEY,AREA=IOBUFFER,   X
               KEYLEN=(*,KEYLEN),RECLEN=(*,RECLEN),AREALEN=(*,RECLEN)
         LTR   R15,R15
         BNZ   GENFRPL
         ST    R1,RPL
         ST    R5,ACB
*
         MGEN  1,GENFL2
         MGEN  2,GENFL3
         MGEN  3,GENFL4
         MGEN  4,GENFL5
         MGEN  5,GENERR
GENFACB  EQU   *
         STH   R15,OPENRC
         SHOWCB ACB=(R1),AREA=SHOWEROR,LENGTH=4,FIELDS=(ERROR)
         B     GENRET
GENFRPL  EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R1),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
         MVI   REQUEST,X'40'
         B     GENRET
GENERR   EQU   *                        ERROR - LOGIC PROGRAM ERROR
         LA    R15,X'04'
         STH   R15,RPLRC
         LA    R15,X'63'                DECIMAL 99
         ST    R15,FDBK
         MVI   REQUEST,X'40'
GENRET   EQU   *
         L     R13,SAVEGEN+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVEGEN  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   CLOSE  ROUTINE                                                    *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CLOSRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVECLO+4
         LR    R8,R13
         LA    R13,SAVECLO
         ST    R13,8(,R8)
*
         L     R5,ACB
         CLOSE (R5)
         LTR   R15,R15
         BNZ   CLSSHOW
*        NI    FILESTAT,OUTFLAG
         L     R7,=A(BLANKDD)
         BALR  R14,R7
CLSSHOW  EQU   *
         STH   R15,CLOSRC
         SHOWCB ACB=(R5),AREA=SHOWEROR,LENGTH=4,FIELDS=(ERROR)
CLSRET   EQU   *
         L     R13,SAVECLO+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVECLO  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   READ  ROUTINE                                                     *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
READRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVERED+4
         LR    R8,R13
         LA    R13,SAVERED
         ST    R13,8(,R8)
*
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(DIR,FWD,ARD,NUP,FKS,KEQ)
         LTR   R15,R15
         BNZ   READFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,KEYOFSET,0(R3)
*
         L     R5,RPL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   READFAIL
*
         MMVC  0(R3),RECLEN,FW0,FW0,IOBUFFER
*
         SR    R15,R15
         L     R5,RPL
READFAIL EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVERED+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVERED  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   READNEXT  ROUTINE                                                 *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
RDNXRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVERNX+4
         LR    R8,R13
         LA    R13,SAVERNX
         ST    R13,8(,R8)
*
         MMVC  SERCHKEY,KEYLEN,FW0,KEYOFSET,0(R3)
*
         MCLC  SERCHKEY,KEYLEN,FW0,FW0,FOUNDKEY
         LTR   R15,R15
         BNZ   RDNXPT
*
         CLC   PREVREQ(4),RDNX
         BE    RDNXGET
RDNXPT   EQU   *
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(SEQ,FWD,ARD,NSP,GEN,KGE)
         LTR   R15,R15
         BNZ   RDNXFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,FW0,SERCHKEY
*
         L     R5,RPL
         POINT RPL=(R5)
         LTR   R15,R15
         BNZ   RDNXFAIL
RDNXGET  EQU   *
         L     R5,RPL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   RDNXFAIL
*
         MMVC  0(R3),RECLEN,FW0,FW0,IOBUFFER
*
         MMVC  FOUNDKEY,KEYLEN,FW0,KEYOFSET,IOBUFFER
*
         SR    R15,R15
         L     R5,RPL
RDNXFAIL EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVERNX+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVERNX  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   WRITE  ROUTINE                                                    *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
WRITRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVEWRT+4
         LR    R8,R13
         LA    R13,SAVEWRT
         ST    R13,8(,R8)
*
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(DIR,FWD,ARD,NUP,FKS,KEQ)
         LTR   R15,R15
         BNZ   WRITFAIL
*
         MMVC  IOBUFFER,RECLEN,FW0,FW0,0(R3)
*
         L     R5,RPL
         PUT   RPL=(R5)
WRITFAIL EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVEWRT+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVEWRT  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   UPDATE  ROUTINE                                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
UPDTRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVEUPD+4
         LR    R8,R13
         LA    R13,SAVEUPD
         ST    R13,8(,R8)
*
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(DIR,FWD,ARD,UPD,FKS,KEQ)
         LTR   R15,R15
         BNZ   UPDTFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,KEYOFSET,0(R3)
*
         L     R5,RPL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   UPDTFAIL
*
         MMVC  IOBUFFER,RECLEN,FW0,FW0,0(R3)
*
         L     R5,RPL
         PUT   RPL=(R5)
UPDTFAIL EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVEUPD+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVEUPD  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   DELETE  ROUTINE                                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
DELTRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVEDEL+4
         LR    R8,R13
         LA    R13,SAVEDEL
         ST    R13,8(,R8)
*
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(DIR,FWD,ARD,UPD,FKS,KEQ)
         LTR   R15,R15
         BNZ   DELTFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,KEYOFSET,0(R3)
*
         L     R5,RPL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   DELTFAIL
         ERASE RPL=(R5)
DELTFAIL EQU   *
         STH   R15,RPLRC
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVEDEL+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVEDEL  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   READ-PREVIOUS  ROUTINE                                            *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
RDPVRTN  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
*
         ST    R13,SAVERPV+4
         LR    R8,R13
         LA    R13,SAVERPV
         ST    R13,8(,R8)
*
         MMVC  SERCHKEY,KEYLEN,FW0,KEYOFSET,0(R3)
*
         MCLC  SERCHKEY,KEYLEN,FW0,FW0,FOUNDKEY
         LTR   R15,R15
         BNZ   RDPVPOS
*
         CLC   PREVREQ(4),RDPV
         BE    RDPVGET
RDPVPOS  EQU   *
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(SEQ,FWD,ARD,NSP,GEN,KGE)
         LTR   R15,R15
         BNZ   RDPVFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,FW0,SERCHKEY
*
         L     R5,RPL
         POINT RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVLRD
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVFAIL
*
         MMVC  FILEKEY,KEYLEN,FW0,KEYOFSET,IOBUFFER
*
RDPVARD  EQU   *
         L     R5,RPL
         MODCB RPL=(R5),OPTCD=(SEQ,BWD,ARD,NSP,FKS,KEQ)
         LTR   R15,R15
         BNZ   RDPVFAIL
         POINT RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVFAIL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVFAIL
*
         MCLC  FILEKEY,KEYLEN,FW0,FW0,SERCHKEY
         LTR   R15,R15
         BZ    RDPVOK
         B     RDPVGET
*
RDPVLRD  EQU   *
         MODCB RPL=(R5),OPTCD=(SEQ,BWD,LRD,NSP)
         LTR   R15,R15
         BNZ   RDPVFAIL
         POINT RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVFAIL
RDPVGET  EQU   *
         L     R5,RPL
         GET   RPL=(R5)
         LTR   R15,R15
         BNZ   RDPVFAIL
RDPVOK   EQU   *
*
         MMVC  0(R3),RECLEN,FW0,FW0,IOBUFFER
*
         MMVC  FOUNDKEY,KEYLEN,FW0,KEYOFSET,IOBUFFER
*
         SR    R15,R15
RDPVFAIL EQU   *
         STH   R15,RPLRC
         L     R5,RPL
         SHOWCB RPL=(R5),AREA=SHOWFDBK,LENGTH=4,FIELDS=(FDBK)
*
         L     R13,SAVERPV+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVERPV  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   BLANK-OUT DDNAME ROUTINE                                          *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
BLANKDD  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R8,R0
         USING *,R8
*
         ST    R13,SAVEBLA+4
         LR    R7,R13
         LA    R13,SAVEBLA
         ST    R13,8(,R7)
*
         MBLA  1,BLAF2
         MBLA  2,BLAF3
         MBLA  3,BLAF4
         MBLA  4,BLAF5
         MBLA  5,BLAERR
BLAERR   EQU   *                        ERROR - LOGIC PROGRAM ERROR
         LA    R15,X'08'
         STH   R15,RPLRC
         LA    R15,X'63'                DECIMAL 99
         ST    R15,FDBK
         MVI   REQUEST,X'40'
         B     BLARET
BLASTAT  MVI   FILESTAT,OUTFLAG
BLARET   EQU   *
         L     R13,SAVEBLA+4
         LM    R14,R12,12(R13)
         BR    R14
*
SAVEBLA  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   MOVE-CHARACTER  ROUTINE                                           *
*   R5     - NO. OF BYTES TO BE MOVED          (BINARY FW)            *
*   R5 + 4 - OFFSET OF 1ST OPERAND             (BINARY FW)            *
*   R5 + 8 - OFFSET OF 2ND OPERAND             (BINARY FW)            *
*   R6     - 1ST OPERAND                                              *
*   R7     - 2ND OPERAND                                              *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
MVCRTN   CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
         ST    R13,SAVEMVC+4
*
MVC1ST   L     R5,LEN+4
MVCO1    EQU   *                      POS 1ST OPERAND AFTER IT'S OFFSET
         C     R5,=F'0'
         BE    MVC2ND
         LA    R6,1(0,R6)
         S     R5,=F'1'
         B     MVCO1
MVC2ND   L     R5,LEN+8
MVCO2    EQU   *                      POS 2ND OPERAND AFTER IT'S OFFSET
         C     R5,=F'0'
         BE    MVCLEN
         LA    R7,1(0,R7)
         S     R5,=F'1'
         B     MVCO2
MVCLEN   L     R5,LEN
MVCMVC   EQU   *                       MOVE CONTENTS OF 2ND TO 1ST
         C     R5,=F'0'
         BE    MVCRET
         MVC   0(1,R6),0(R7)
         LA    R6,1(0,R6)
         LA    R7,1(0,R7)
         S     R5,=F'1'
         B     MVCMVC
MVCRET   EQU   *
         L     R13,SAVEMVC+4
         LM    R14,R12,12(R13)
         BR    R14
*
SAVEMVC  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   COMPARE-LOGICAL-CHARACTER  ROUTINE                                *
*   R5     - NO. OF BYTES TO BE COMPARED       (BINARY FW)            *
*   R5 + 4 - OFFSET OF 1ST OPERAND             (BINARY FW)            *
*   R5 + 8 - OFFSET OF 2ND OPERAND             (BINARY FW)            *
*   R6     - 1ST OPERAND                                              *
*   R7     - 2ND OPERAND                                              *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CLCRTN   CSECT ,
         STM   R14,R12,12(R13)
         BALR  R9,R0
         USING *,R9
         ST    R13,SAVECLC+4
*
         SR    R15,R15
CLC1ST   L     R5,LEN+4
CLCO1    EQU   *                      POS 1ST OPERAND AFTER IT'S OFFSET
         C     R5,=F'0'
         BE    CLC2ND
         LA    R6,1(0,R6)
         S     R5,=F'1'
         B     CLCO1
CLC2ND   L     R5,LEN+8
CLCO2    EQU   *                      POS 2ND OPERAND AFTER IT'S OFFSET
         C     R5,=F'0'
         BE    CLCLEN
         LA    R7,1(0,R7)
         S     R5,=F'1'
         B     CLCO2
CLCLEN   L     R5,LEN
CLCCLC   EQU   *                       COMPARE CONTENTS OF 1ST VS 2ND
         C     R5,=F'0'
         BE    CLCRET
         CLC   0(1,R6),0(R7)
         BNE   CLCNE
         LA    R6,1(0,R6)
         LA    R7,1(0,R7)
         S     R5,=F'1'
         B     CLCCLC
CLCNE    EQU   *
         LA    R15,9
CLCRET   EQU   *
         L     R13,SAVECLC+4
         L     R14,12(R13)
         LM    R0,R12,20(R13)
         BR    R14
*
SAVECLC  DC    18F'0'
         LTORG
*
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*   CONVERT-ASMCD-TO-COBCD  ROUTINE                                   *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
CVTCHAR  CSECT ,
         STM   R14,R12,12(R13)
         BALR  R8,R0
         USING *,R8
*
         ST    R13,SAVECHR+4
         LR    R7,R13
         LA    R13,SAVECHR
         ST    R13,8(,R7)
*
TSTOPEN  CLC   OPEN(4),REQUEST
         BNE   TSTCLOS
*        L     R7,ERROR
*        CVD   R7,PAKNBR
*        MVC   2(2,R2),PAKNBR+6
         MVC   2(2,R2),ERROR+2
         TR    ERROR+3(1),OPENCODE
         L     R7,ERROR
*        LH    R6,OPENRC
         MVC   4(2,R2),OPENRC
         B     ENDIF
TSTCLOS  CLC   CLOS(4),REQUEST
         BNE   TSTRPL
*        L     R7,ERROR
*        CVD   R7,PAKNBR
*        MVC   2(2,R2),PAKNBR+6
         MVC   2(2,R2),ERROR+2
         TR    ERROR+3(1),CLOSCODE
         L     R7,ERROR
*        LH    R6,CLOSRC
         MVC   4(2,R2),CLOSRC
         B     ENDIF
TSTRPL   EQU   *
*        L     R7,FDBK
*        CVD   R7,PAKNBR
*        MVC   2(2,R2),PAKNBR+6
         MVC   2(2,R2),FDBK+2
         TR    FDBK+3(1),RPLCODE
         L     R7,FDBK
*        LH    R6,RPLRC
         MVC   4(2,R2),RPLRC
ENDIF    EQU   *
*        CVD   R6,PAKNBR
*        MVC   4(2,R2),PAKNBR+6
         CVD   R7,PAKNBR
         UNPK  FWRESULT(15),PAKNBR
         OI    FWRESULT+14,X'F0'
         MVC   0(2,R2),FWRESULT+13
         L     R13,SAVECHR+4
         LM    R14,R12,12(R13)
         BR    R14
*
SAVECHR  DC    18F'0'
         LTORG ,
*
*   RPL ERROR CODE TRANSALATION TABLE
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
RPLCODE  DC    X'006363630A6363631663636315636363'    0
         DC    X'176363635D6363635D63636318636363'    1
         DC    X'146363635C6363635D6363635C636363'    2
         DC    X'5D6363635A6363635F63636363636363'    3
         DC    X'5A6363635C6363635C6363635C636363'    4
         DC    X'5C6363635C6363635E6363635C636363'    5
         DC    X'156363635C6363635C6363635C636363'    6
         DC    X'5C6363635F6363636363636363636363'    7
         DC    X'5F6363635C6363635C6363631E636363'    8
         DC    X'5A636363186363635D6363635A636363'    9
         DC    X'63636363636363636363636363636363'    A
         DC    X'63636363636363636363636363636363'    B
         DC    X'176363635C6363635C6363635C636363'    C
         DC    X'5D6363635D6363635F63636363636363'    D
         DC    X'63636363636363636363636363636363'    E
         DC    X'63636363636363636363636363636363'    F
*
*   OPEN ERROR CODE TRANSALATION TABLE
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
OPENCODE DC    X'00635F635C6363636363636363635F5D'    0
         DC    X'635F5F5F636363636363636363636363'    1
         DC    X'5F635F63636363635D63636363636363'    2
         DC    X'5C635D63636363636363636363636363'    3
         DC    X'5C5D5F5F5F5F5F5F5F63636363635F5F'    4
         DC    X'5F63636363636363636363635F636363'    5
         DC    X'5F6363635F6363635F6363635F635F63'    6
         DC    X'636363635A5F63636363636363636363'    7
         DC    X'606363631E6363635D63636363636363'    8
         DC    X'1E6363635F6363635F63636363636363'    9
         DC    X'5F5F63631E1E5D5D5D63636363636363'    A
         DC    X'1E6363635F6363631E6363635D636363'    B
         DC    X'5F6363635A6363635A6363635A636363'    C
         DC    X'5A6363635A6363635A6363635A636363'    D
         DC    X'5A6363635A6363635A6363635A636363'    E
         DC    X'5A6363635A6363635A63636363635A5F'    F
*
*   CLOSE ERROR CODE TRANSALATION TABLE
*                 0 1 2 3 4 5 6 7 8 9 A B C D E F
CLOSCODE DC    X'00635F635C6363636363636363636363'    0
         DC    X'63636363636363636363636363636363'    1
         DC    X'63636363636363636363636363636363'    2
         DC    X'63636363636363636363636363636363'    3
         DC    X'6363636363636363636363635A636363'    4
         DC    X'63636363636363636363636363636363'    5
         DC    X'63636363636363636363636363636363'    6
         DC    X'63636363636363636363636363636363'    7
         DC    X'636363631E6363635D63636363636363'    8
         DC    X'1E636363636363636363636363636363'    9
         DC    X'636363631E1E5D5D6363636363636363'    A
         DC    X'1E636363636363631E6363635D636363'    B
         DC    X'63636363636363636363636363636363'    C
         DC    X'63636363636363636363636363636363'    D
         DC    X'636363635A6363636363636363636363'    E
         DC    X'6363636363636363636363635A635A63'    F
*
         END   BEGIN
