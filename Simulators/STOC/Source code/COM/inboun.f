      SUBROUTINE INBOUN
C======================================================================
C     境界条件を読み込む
C     入力データの追加方法は README_INPUT を参照
C======================================================================
      use mod_fault,only: set_utm,icoord,isystem,jsystem
      IMPLICIT NONE
C
      INCLUDE 'AREA.h'
      INCLUDE 'BOUNDI.h'
      INCLUDE 'BOUNDR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEI.h'
      INCLUDE 'FILEC.h'
      INCLUDE 'TABLER.h'
      INCLUDE 'TABLEI.h'
      INCLUDE 'MODELI.h'
      INCLUDE 'CONNEC.h'
C
C      COMMON  / ADD0116 / AMP,TTT,ALL,HHH,AXX
      INCLUDE 'RGWAVE.h'
C
      CHARACTER(23)::CTMP
      CHARACTER(132)::CLINED
C
C      DIMENSION RTMP(200)
C
      REAL(8)::RTMP(100000)
      REAL(8)::LC_DEG=-999.9D0
      INTEGER::LL=-1
      INTEGER::ITMP(20),IHA(9),INSIDE,IDUM,JDUM
C
      DATA IHA/ 9*0 /
C
C ... プリセット用ローカル変数
C
      INTEGER::IU1=0,IV1=0,IW1=0,IH1=0,IT1=0,
     $         IC1=0,IK1=0,IE1=0,IVTYP1=0,ITTYP1=0
      REAL(8)::RU1=0.0D0,RV1=0.0D0,RW1=0.0D0,RH1=0.0D0,
     $         RT1=0.0D0,RC1=0.0D0,RK1=0.0D0,RE1=0.0D0
      REAL(8)::AM2=0.0D0,AS2=0.0D0,AN2=0.0D0,AK2=0.0D0,
     $         AK1=0.0D0,AO1=0.0D0,AP1=0.0D0,AQ1=0.0D0
      REAL(8)::BM2=1.570796326794697D0,BS2=1.570796326794697D0,
     $         BN2=1.570796326794697D0,BK2=1.570796326794697D0,
     $         BK1=1.570796326794697D0,BO1=1.570796326794697D0,
     $         BP1=1.570796326794697D0,BQ1=1.570796326794697D0
      REAL(8)::PAI=3.14159265358979D0,H0=0.0D0
C
      INTEGER::I,I1OO,ICTYP1,IE,IERR,IH2,IHTYP1,INPD,IS,ITFILE
      INTEGER::KOUNT,M,N,NDAT1,NSR
C
C
      ITFILE = 0
      ICTYP1 = 0
      IHTYP1 = 0
      DO 100 N=1,100000
         CALL GET1(IS,IE,IERR)
         IF( IERR.GT.0 ) GO TO 900
         CLINED = CLINE
C
         IF( CLINE(IS:IE) .EQ. '%END' ) THEN
            GO TO 300
C ...................  < 開境界条件-2D >
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OPEN-2D' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               IPFLG = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               IPFLG = 1
            ELSE
               CALL ERRMSG('INBOUN',6430)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=OPEN-2D'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
C ...................  < 開境界条件-ゾンマー >
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OPEN-SOMMER' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF(NDAT1.NE.4) THEN
               CALL ERRMSG('INBOUN',6431)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 4'
               WRITE(LP,*) 'VARIABLE=OPEN-SOMMER'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NSOMER(1) = ITMP(1)
            NSOMER(2) = ITMP(2)
            NSOMER(3) = ITMP(3)
            NSOMER(4) = ITMP(4)
            IF( IPECON(4,NRANK+1).GE.0 ) NSOMER(1)=0
            IF( IPECON(5,NRANK+1).GE.0 ) NSOMER(2)=0
            IF( IPECON(6,NRANK+1).GE.0 ) NSOMER(3)=0
            IF( IPECON(7,NRANK+1).GE.0 ) NSOMER(4)=0
C
CDEBUGc           0でないときは同じ値にするように制限
CDEBUG            NSR=MAXVAL(NSOMER(1:4))
CDEBUG            IF((NSOMER(1).NE.0.AND.NSOMER(1).NE.NSR).OR.
CDEBUG     $         (NSOMER(2).NE.0.AND.NSOMER(2).NE.NSR).OR.
CDEBUG     $         (NSOMER(3).NE.0.AND.NSOMER(3).NE.NSR).OR.
CDEBUG     $         (NSOMER(4).NE.0.AND.NSOMER(4).NE.NSR)) THEN
CDEBUG               CALL ERRMSG('INBOUN',6432)
CDEBUG               WRITE(LP,*) 'SEVERAL TYPES OF OPEN-SOMMER',
CDEBUG     $                     ' CONDITION IS SET'
CDEBUG               WRITE(LP,*) 'VARIABLE=OPEN-SOMMER'
CDEBUG               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
CDEBUG               WRITE(LP,*) 'LINE=',CLINE
CDEBUG               CALL ABORT1('')
CDEBUG            ENDIF
C
C ...................  < オーバーラップ範囲 >
C
         ELSE IF( CLINE(IS:IE) .EQ. 'OVERLAP' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF(NDAT1.NE.4) THEN
               CALL ERRMSG('INBOUN',6433)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 4'
               WRITE(LP,*) 'VARIABLE=OVERLAP'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NOVRLP(1) = ITMP(1)
            NOVRLP(2) = ITMP(2)
            NOVRLP(3) = ITMP(3)
            NOVRLP(4) = ITMP(4)
            IF( IPECON(4,NRANK+1).GE.0 ) NOVRLP(1)=0
            IF( IPECON(5,NRANK+1).GE.0 ) NOVRLP(2)=0
            IF( IPECON(6,NRANK+1).GE.0 ) NOVRLP(3)=0
            IF( IPECON(7,NRANK+1).GE.0 ) NOVRLP(4)=0
C
         ELSE IF( CLINE(IS:IE) .EQ. 'MODIFY-DEPTH' ) THEN
            CALL GETC(CTMP,3)
            IF( CTMP.EQ.'OFF' ) THEN
               LMODDEP = 0
            ELSE IF( CTMP.EQ.'ON ' ) THEN
               LMODDEP = 1
            ELSE
               CALL ERRMSG('INBOUN',6434)
               WRITE(LP,*) 'VALUE MUST BE ON OR OFF'
               WRITE(LP,*) 'VARIABLE=MODIFY-DEPTH'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'VERTICAL-PROFILE' ) THEN
            CALL GETC(CTMP,6)
            IF( CTMP.EQ.'FLAT  ' ) THEN
               LVPNAB=0
            ELSE IF( CTMP.EQ.'NABOR ' ) THEN
               LVPNAB=1
            ELSE
               CALL ERRMSG('INBOUN',6435)
               WRITE(LP,*) 'VALUE MUST BE FLAT OR NABOR'
               WRITE(LP,*) 'VARIABLE=VERTICAL-PROFILE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'NABOR' ) THEN
            CALL GETI(IVPNAB)
            IF( IVPNAB.LE.0 ) THEN
               CALL ERRMSG('INBOUN',6436)
               WRITE(LP,*) 'VALUE MUST BE >= 1'
               WRITE(LP,*) 'VARIABLE=NABOR'
               WRITE(LP,*) 'VALUE=',IVPNAB
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'NEST-TANGENTIAL-VELOCITY' ) THEN
            CALL GETC(CTMP,6)
            IF( CTMP.EQ.'PARENT' ) THEN
               LNTANG=0
            ELSE IF( CTMP.EQ.'FREE  ' ) THEN
               LNTANG=1
            ELSE IF( CTMP.EQ.'HYBRID' ) THEN
               LNTANG=2
            ELSE
               CALL ERRMSG('INBOUN',6437)
               WRITE(LP,*) 'VALUE MUST BE PARENT OR FREE OR HYBRID'
               WRITE(LP,*) 'VARIABLE=NEST-TANG-VELOCITY'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
C ...................  < 海面(自由表面)条件 >
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SURFACE-TYPE' ) THEN
            CALL GETC(CTMP,13)
            IF( CTMP.EQ.'CONSTANT     ' ) THEN
C              ISURF(1) = 0
C              ISURF(2) = 0
C              ISURF(3) = 0
            ELSE IF( CTMP.EQ.'FUNCTION      ' ) THEN
               ISURF(1) = -1
               ISURF(2) = -1
               ISURF(3) = -1
            ELSE IF( CTMP.EQ.'FUNCTION2     ' ) THEN
               ISURF(1) = -2
               ISURF(2) = -2
               ISURF(3) = -2
            ELSE
               CALL ERRMSG('INBOUN',6438)
               WRITE(LP,*) 'VALUE MUST BE CONSTANT OR FUNCTION[2]'
               WRITE(LP,*) 'VARIABLE=SURFACE-TYPE'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C ..................... 海面気圧条件
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SURFACE-PRESSURE' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
            IF( ISURF(1).GE.0 ) THEN
C ............ (一定値)
               IF( NDAT1.EQ.1 ) THEN
                  ISURF(1) = 0
                  RSURF(1) = RTMP(1)
C
C ............ (時系列値)
               ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
                  NTABLE = NTABLE + 1
                  ISURF(1) = NTABLE
                  IF( NTABLE.GT.NTBLSZ ) THEN
                     CALL ERRMSG('INBOUN',6439)
                     WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                           'OVER NTBLSZ IN WHOLE INPUT DATA'
                     WRITE(LP,*) 'VARIABLE=SURFACE-PRESSURE'
                     WRITE(LP,*) 'LINE=',CLINE
                     CALL ABORT1('')
                  END IF
                  ITABLE(NTABLE) = NDAT1/2
                  DO 110 M=1,NDAT1/2
                     TTABLE(M,NTABLE) = RTMP(M*2-1)
                     VTABLE(M,NTABLE) = RTMP(M*2)
  110             CONTINUE
               ELSE
                  CALL ERRMSG('INBOUN',6440)
                  WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                        'OR 2*N'
                  WRITE(LP,*) 'VARIABLE=SURFACE-PRESSURE'
                  WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
            END IF
C ..................... 海面風速U条件
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SURFACE-WIND-U' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
            IF( ISURF(2).GE.0 ) THEN
C ............ (一定値)
               IF( NDAT1.EQ.1 ) THEN
                  ISURF(2) = 0
                  RSURF(2) = RTMP(1)
C
C ............ (時系列値)
               ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
                  NTABLE = NTABLE + 1
                  ISURF(2) = NTABLE
                  IF( NTABLE.GT.NTBLSZ ) THEN
                     CALL ERRMSG('INBOUN',6441)
                     WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                           'OVER NTBLSZ IN WHOLE INPUT DATA'
                     WRITE(LP,*) 'VARIABLE=SURFACE-WIND-U'
                     WRITE(LP,*) 'LINE=',CLINE
                     CALL ABORT1('')
                  END IF
                  ITABLE(NTABLE) = NDAT1/2
                  DO 120 M=1,NDAT1/2
                     TTABLE(M,NTABLE) = RTMP(M*2-1)
                     VTABLE(M,NTABLE) = RTMP(M*2)
  120             CONTINUE
               ELSE
                  CALL ERRMSG('INBOUN',6442)
                  WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                        'OR 2*N'
                  WRITE(LP,*) 'VARIABLE=SURFACE-WIND-U'
                  WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
            END IF
C ..................... 海面風速V条件
C
         ELSE IF( CLINE(IS:IE) .EQ. 'SURFACE-WIND-V' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
            IF( ISURF(3).GE.0 ) THEN
C ............ (一定値)
               IF( NDAT1.EQ.1 ) THEN
                  ISURF(3) = 0
                  RSURF(3) = RTMP(1)
C
C ............ (時系列値)
               ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
                  NTABLE = NTABLE + 1
                  ISURF(3) = NTABLE
                  IF( NTABLE.GT.NTBLSZ ) THEN
                     CALL ERRMSG('INBOUN',6443)
                     WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                           'OVER NTBLSZ IN WHOLE INPUT DATA'
                     WRITE(LP,*) 'VARIABLE=SURFACE-WIND-V'
                     WRITE(LP,*) 'LINE=',CLINE
                     CALL ABORT1('')
                  END IF
                  ITABLE(NTABLE) = NDAT1/2
                  DO 130 M=1,NDAT1/2
                     TTABLE(M,NTABLE) = RTMP(M*2-1)
                     VTABLE(M,NTABLE) = RTMP(M*2)
  130             CONTINUE
               ELSE
                  CALL ERRMSG('INBOUN',6444)
                  WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                        'OR 2*N'
                  WRITE(LP,*) 'VARIABLE=SURFACE-WIND-V'
                  WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
            END IF
C .................. 流速Uに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'U' ) THEN
            I1OO = I1DD
            I1DD = - I1DD
            CALL GETC(CTMP,4)
            IF(CTMP.EQ.'FILE') THEN
              I1OO = I1DD
              I1DD = - I1DD
C             (時系列入力用ファイルを開く)
              IF(ITFILE.EQ.0) THEN
                CFLNM(IFLNM-3:IFLNM) = '.tim'
                write(lp,*) CFLNM
                OPEN(IFLTM,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $               FORM='FORMATTED',ERR=901)
                ITFILE = 1
              END IF
              INPD = INP
              INP = IFLTM
              I1DD = 0
            CALL GETD(IS,IE,IERR)
              IF( CLINE(IS:IE) .EQ. 'U' ) THEN
                CALL MGETD(RTMP,NDAT1,2*NTIMSZ)
                IF( MOD(NDAT1,2).EQ.0 ) THEN
                  NTABLE = NTABLE + 1
                  IU1 = NTABLE
                  IF( NTABLE.GT.NTBLSZ ) THEN
                    CALL ERRMSG('INBOUN',6445)
                    WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                          'OVER NTBLSZ IN WHOLE INPUT DATA'
                    WRITE(LP,*) 'VARIABLE=U'
                    WRITE(LP,*) 'LINE=',CLINE
                    CALL ABORT1('')
                  END IF
                  ITABLE(NTABLE) = NDAT1/2
                  DO 145 M=1,NDAT1/2
                    TTABLE(M,NTABLE) = RTMP(M*2-1)
                    VTABLE(M,NTABLE) = RTMP(M*2)
  145             CONTINUE
                ELSE
                  CALL ERRMSG('INBOUN',6446)
                  WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2*N'
                  WRITE(LP,*) 'VARIABLE=U'
                  WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
                END IF
                I1DD = I1OO
              ELSE
                CALL ERRMSG('INBOUN',6447)
                WRITE(LP,*) 'FILE DATA DATA MUST BE U ='
                WRITE(LP,*) 'VARIABLE=U'
                CALL ABORT1('')
              END IF
              INP = INPD
              CLINE = CLINED
              I1DD = IABS(I1DD)
            ELSE
C
              IF(CLINED.NE.CLINE) THEN
                KOUNT = 0
    1           CONTINUE
                KOUNT = KOUNT+1
                IF(KOUNT.GT.5) THEN
                   CALL ERRMSG('INBOUN',6448)
                   WRITE(LP,*) 'KOUNT=',KOUNT
                   CALL ABORT1('')
                ENDIF
                BACKSPACE INP
                I1DD = 0
                CALL GET1(IS,IE,IERR)
                IF(CLINE.NE.CLINED) THEN
                  BACKSPACE INP
                  GO TO 1
                END IF
              END IF
              I1DD = I1OO
              CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
              IF( NDAT1.EQ.1 ) THEN
                IU1 = 0
                RU1 = RTMP(1)
C
C ......... (時系列値)
              ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
                NTABLE = NTABLE + 1
                IU1 = NTABLE
                IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6449)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=U'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
                END IF
                ITABLE(NTABLE) = NDAT1/2
                DO 140 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  140          CONTINUE
             ELSE
               CALL ERRMSG('INBOUN',6450)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=U'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
             END IF
           END IF
C .................. 流速Vに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'V' ) THEN
           I1OO = I1DD
           I1DD = - I1DD
           CALL GETC(CTMP,4)
           IF(CTMP.EQ.'FILE') THEN
             I1OO = I1DD
             I1DD = - I1DD
C            (時系列入力用ファイルを開く)
             IF(ITFILE.EQ.0) THEN
               CFLNM(IFLNM-3:IFLNM) = '.tim'
               write(lp,*) CFLNM
               OPEN(IFLTM,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $              FORM='FORMATTED',ERR=901)
               ITFILE = 1
             END IF
             INPD = INP
             INP = IFLTM
             I1DD = 0
             CALL GET1(IS,IE,IERR)
             IF( CLINE(IS:IE) .EQ. 'V' ) THEN
               CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
               IF( MOD(NDAT1,2).EQ.0 ) THEN
                 NTABLE = NTABLE + 1
                 IV1 = NTABLE
                 IF( NTABLE.GT.NTBLSZ ) THEN
                   CALL ERRMSG('INBOUN',6451)
                   WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                   WRITE(LP,*) 'VARIABLE=V'
                   WRITE(LP,*) 'LINE=',CLINE
                   CALL ABORT1('')
                 END IF
                 ITABLE(NTABLE) = NDAT1/2
                 DO 155 M=1,NDAT1/2
                   TTABLE(M,NTABLE) = RTMP(M*2-1)
                   VTABLE(M,NTABLE) = RTMP(M*2)
  155            CONTINUE
               ELSE
                 CALL ERRMSG('INBOUN',6452)
                 WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2*N'
                 WRITE(LP,*) 'VARIABLE=V'
                 WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               I1DD = I1OO
             ELSE
               CALL ERRMSG('INBOUN',6453)
               WRITE(LP,*) 'FILE DATA DATA MUST BE U ='
               WRITE(LP,*) 'VARIABLE=V'
               CALL ABORT1('')
             END IF
             INP = INPD
             CLINE = CLINED
             I1DD = IABS(I1DD)
           ELSE
C
             IF(CLINED.NE.CLINE) THEN
               KOUNT = 0
    2          CONTINUE
               KOUNT = KOUNT+1
               IF(KOUNT.GT.5) THEN
                  CALL ERRMSG('INBOUN',6454)
                  WRITE(LP,*) 'KOUNT=',KOUNT
                  CALL ABORT1('')
               ENDIF
               BACKSPACE INP
               I1DD = 0
               CALL GET1(IS,IE,IERR)
               IF(CLINE.NE.CLINED) THEN
                 BACKSPACE INP
                 GO TO 2
               END IF
             END IF
             I1DD = I1OO
             CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
             IF( NDAT1.EQ.1 ) THEN
               IV1 = 0
               RV1 = RTMP(1)
C
C ......... (時系列値)
             ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IV1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                 CALL ERRMSG('INBOUN',6455)
                 WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                        'OVER NTBLSZ IN WHOLE INPUT DATA'
                 WRITE(LP,*) 'VARIABLE=V'
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 150 M=1,NDAT1/2
                 TTABLE(M,NTABLE) = RTMP(M*2-1)
                 VTABLE(M,NTABLE) = RTMP(M*2)
  150          CONTINUE
             ELSE
               CALL ERRMSG('INBOUN',6456)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=V'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
             END IF
           END IF
C .................. 流速Wに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'W' ) THEN
           I1OO = I1DD
           I1DD = - I1DD
           CALL GETC(CTMP,4)
           IF(CTMP.EQ.'FILE') THEN
             I1OO = I1DD
             I1DD = - I1DD
C            (時系列入力用ファイルを開く)
             IF(ITFILE.EQ.0) THEN
               CFLNM(IFLNM-3:IFLNM) = '.tim'
               write(lp,*) CFLNM
               OPEN(IFLTM,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $              FORM='FORMATTED',ERR=901)
               ITFILE = 1
             END IF
             INPD = INP
             INP = IFLTM
             I1DD = 0
             CALL GET1(IS,IE,IERR)
             IF( CLINE(IS:IE) .EQ. 'W' ) THEN
               CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
               IF( MOD(NDAT1,2).EQ.0 ) THEN
                 NTABLE = NTABLE + 1
                 IW1 = NTABLE
                 IF( NTABLE.GT.NTBLSZ ) THEN
                   CALL ERRMSG('INBOUN',6457)
                   WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                   WRITE(LP,*) 'VARIABLE=W'
                   WRITE(LP,*) 'LINE=',CLINE
                   CALL ABORT1('')
                 END IF
                 ITABLE(NTABLE) = NDAT1/2
                 DO 165 M=1,NDAT1/2
                   TTABLE(M,NTABLE) = RTMP(M*2-1)
                   VTABLE(M,NTABLE) = RTMP(M*2)
  165            CONTINUE
               ELSE
                 CALL ERRMSG('INBOUN',6458)
                 WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2*N'
                 WRITE(LP,*) 'VARIABLE=W'
                 WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               I1DD = I1OO
             ELSE
               CALL ERRMSG('INBOUN',6459)
               WRITE(LP,*) 'FILE DATA DATA MUST BE W ='
               WRITE(LP,*) 'VARIABLE=W'
               CALL ABORT1('')
             END IF
             INP = INPD
             CLINE = CLINED
             I1DD = IABS(I1DD)
           ELSE
C
             IF(CLINED.NE.CLINE) THEN
               KOUNT = 0
    3          CONTINUE
               KOUNT = KOUNT+1
               IF(KOUNT.GT.5) THEN
                  CALL ERRMSG('INBOUN',6460)
                  WRITE(LP,*) 'KOUNT=',KOUNT
                  CALL ABORT1('')
               ENDIF
               BACKSPACE INP
               I1DD = 0
               CALL GET1(IS,IE,IERR)
               IF(CLINE.NE.CLINED) THEN
                 BACKSPACE INP
                 GO TO 3
               END IF
             END IF
             I1DD = I1OO
             CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
             IF( NDAT1.EQ.1 ) THEN
               IW1 = 0
               RW1 = RTMP(1)
C
C ......... (時系列値)
             ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IW1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                 CALL ERRMSG('INBOUN',6461)
                 WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                        'OVER NTBLSZ IN WHOLE INPUT DATA'
                 WRITE(LP,*) 'VARIABLE=W'
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 160 M=1,NDAT1/2
                 TTABLE(M,NTABLE) = RTMP(M*2-1)
                 VTABLE(M,NTABLE) = RTMP(M*2)
  160          CONTINUE
             ELSE
               CALL ERRMSG('INBOUN',6462)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=W'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
             END IF
           END IF
C .................. 海面高さHに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'H' ) THEN
           I1OO = I1DD
           I1DD = - I1DD
           CALL GETC(CTMP,4)
           IF(CTMP.EQ.'FILE') THEN
             I1OO = I1DD
             I1DD = - I1DD
C            (時系列入力用ファイルを開く)
             IF(ITFILE.EQ.0) THEN
               CFLNM(IFLNM-3:IFLNM) = '.tim'
               write(lp,*) CFLNM
               OPEN(IFLTM,FILE=CFLNM(1:IFLNM),STATUS='OLD',
     $              FORM='FORMATTED',ERR=901)
               ITFILE = 1
             END IF
             INPD = INP
             INP = IFLTM
             I1DD = 0
             CALL GET1(IS,IE,IERR)
             IF( CLINE(IS:IE) .EQ. 'H' ) THEN
               CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
               IF( MOD(NDAT1,2).EQ.0 ) THEN
                 NTABLE = NTABLE + 1
                 IH1 = NTABLE
                 IF( NTABLE.GT.NTBLSZ ) THEN
                   CALL ERRMSG('INBOUN',6463)
                   WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                   WRITE(LP,*) 'VARIABLE=H'
                   WRITE(LP,*) 'LINE=',CLINE
                   CALL ABORT1('')
                 END IF
                 ITABLE(NTABLE) = NDAT1/2
                 DO 175 M=1,NDAT1/2
                   TTABLE(M,NTABLE) = RTMP(M*2-1)
                   VTABLE(M,NTABLE) = RTMP(M*2)
  175            CONTINUE
               ELSE
                 CALL ERRMSG('INBOUN',6464)
                 WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2*N'
                 WRITE(LP,*) 'VARIABLE=H'
                 WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               I1DD = I1OO
             ELSE
               CALL ERRMSG('INBOUN',6465)
               WRITE(LP,*) 'FILE DATA DATA MUST BE H ='
               WRITE(LP,*) 'VARIABLE=H'
               CALL ABORT1('')
             END IF
             INP = INPD
             CLINE = CLINED
             I1DD = IABS(I1DD)
           ELSE
C
             IF(CLINED.NE.CLINE) THEN
               KOUNT = 0
    4          CONTINUE
               KOUNT = KOUNT+1
               IF(KOUNT.GT.5) THEN
                  CALL ERRMSG('INBOUN',6466)
                  WRITE(LP,*) 'KOUNT=',KOUNT
                  CALL ABORT1('')
               ENDIF
               BACKSPACE INP
               I1DD = 0
               CALL GET1(IS,IE,IERR)
               IF(CLINE.NE.CLINED) THEN
                 BACKSPACE INP
                 GO TO 4
               END IF
             END IF
             I1DD = I1OO
             CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
             IF( NDAT1.EQ.1 ) THEN
               IH1 = 0
               RH1 = RTMP(1)
C
C ......... (時系列値)
             ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IH1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                 CALL ERRMSG('INBOUN',6467)
                 WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                        'OVER NTBLSZ IN WHOLE INPUT DATA'
                 WRITE(LP,*) 'VARIABLE=H'
                 WRITE(LP,*) 'LINE=',CLINE
                 CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 170 M=1,NDAT1/2
                 TTABLE(M,NTABLE) = RTMP(M*2-1)
                 VTABLE(M,NTABLE) = RTMP(M*2)
  170          CONTINUE
             ELSE
               CALL ERRMSG('INBOUN',6468)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=H'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
             END IF
           END IF
C .................. 温度Tに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'T' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IT1 = 0
               RT1 = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IT1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6469)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=T'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 180 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  180          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6470)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=T'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 濃度Cに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'C' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IC1 = 0
               RC1 = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IC1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6471)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=C'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 190 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  190          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6472)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=C'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 乱流エネルギーkに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'K' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IK1 = 0
               RK1 = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IK1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6473)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=K'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 200 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  200          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6474)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=K'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. エネルギー散逸Eに関する固定条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'E' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IE1 = 0
               RE1 = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IE1 = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6475)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=E'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 210 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  210          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6476)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $                     'OR 2*N'
               WRITE(LP,*) 'VARIABLE=E'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 流速に関する一般的条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE-V' ) THEN
            CALL GETC(CTMP,13)
C ......... (スリップ)
            IF( CTMP.EQ.'SLIP         ' ) THEN
               IVTYP1 = 0
C ......... (ノースリップ)
            ELSE IF( CTMP.EQ.'NO-SLIP      ' ) THEN
               IVTYP1 = 1
C ......... (壁関数)
            ELSE IF( CTMP.EQ.'WALL-FUNCTION' ) THEN
               IVTYP1 = 2
C ......... (固定条件:IU1,IV1,IW1)
            ELSE IF( CTMP.EQ.'CONSTANT     ' ) THEN
               IVTYP1 = 3
            ELSE
               CALL ERRMSG('INBOUN',6477)
               WRITE(LP,*) 'VALUE MUST BE SLIP OR NO-SLIP ',
     $                     'OR WALL-FUNCTION OR CONSTANT'
               WRITE(LP,*) 'VARIABLE=TYPE-V'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 温度に関する一般的条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE-T' ) THEN
            CALL GETC(CTMP,13)
C ......... (断熱条件)
            IF( CTMP.EQ.'ADIABATIC    ' ) THEN
               ITTYP1 = 0
C ......... (固定条件:IT1)
            ELSE IF( CTMP.EQ.'CONSTANT     ' ) THEN
               ITTYP1 = 1
            ELSE
               CALL ERRMSG('INBOUN',6478)
               WRITE(LP,*) 'VALUE MUST BE ADIABATIC OR ',
     $                     'CONSTANT'
               WRITE(LP,*) 'VARIABLE=TYPE-T'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 濃度に関する一般的条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE-C' ) THEN
            CALL GETC(CTMP,13)
C ......... (勾配ゼロ条件)
            IF( CTMP.EQ.'FREE         ' ) THEN
               ICTYP1 = 0
               ITTYP1 = 0
C ......... (固定条件:IT1)
            ELSE IF( CTMP.EQ.'CONSTANT     ' ) THEN
               ICTYP1 = 1
               ITTYP1 = 1
            ELSE
               CALL ERRMSG('INBOUN',6479)
               WRITE(LP,*) 'VALUE MUST BE FREE OR ',
     $                     'CONSTANT'
               WRITE(LP,*) 'VARIABLE=TYPE-C'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. X方向流入境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'INLET-I' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6480)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=INLET-I'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IDUM=ITMP(1)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),ITMP(3),2,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NINLT = NINLT + 1
            IF( NINLT.GT.NINLSZ ) THEN
               CALL ERRMSG('INBOUN',6481)
               WRITE(LP,*) 'THE NUMBER OF INLET BOUNDARY MAY ',
     $                     'NOT BE OVER NINLSZ'
               WRITE(LP,*) 'VARIABLE=INLET-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6482)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=INLET-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(1)
            IAREA(2,NAREA) = ITMP(1)
            IAREA(3,NAREA) = ITMP(2)
            IAREA(4,NAREA) = ITMP(3)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 1
            MINLT(NINLT)   = NAREA
            IINLT(1,NINLT) = IU1
            IINLT(2,NINLT) = IV1
            IINLT(3,NINLT) = IW1
            IINLT(4,NINLT) = IH1
            IINLT(5,NINLT) = IT1
            IINLT(6,NINLT) = IC1
            IINLT(7,NINLT) = IK1
            IINLT(8,NINLT) = IE1
            IF( IU1.EQ.0 ) RINLT(1,NINLT) = RU1
            IF( IV1.EQ.0 ) RINLT(2,NINLT) = RV1
            IF( IW1.EQ.0 ) RINLT(3,NINLT) = RW1
            IF( IH1.EQ.0 ) RINLT(4,NINLT) = RH1
            IF( IT1.EQ.0 ) RINLT(5,NINLT) = RT1
            IF( IC1.EQ.0 ) RINLT(6,NINLT) = RC1
            IF( IK1.EQ.0 ) RINLT(7,NINLT) = RK1
            IF( IE1.EQ.0 ) RINLT(8,NINLT) = RE1
C
            IF(IHTYP1.EQ.2) THEN
              IH2 = 0
              DO 240 M=1,9
                IH2 = MAX(IH2,IHA(M))
  240         CONTINUE
              IF(IH2.GT.NOTFSZ) THEN
                CALL ERRMSG('INBOUN',6483)
                WRITE(LP,*) 'THE NUMBER OF TIDAL DATA MAY ',
     $                      'NOT BE OVER NOTFSZ'
                WRITE(LP,*) 'VARIABLE=INLET-I'
                WRITE(LP,*) 'LINE=',CLINE
                CALL ABORT1('')
              ELSE IF(IH2.GT.0) THEN
                IINLT(4,NINLT) = -IH2
                RTIDE(1,1,IH2) = AS2
                RTIDE(2,1,IH2) = BS2
                RTIDE(1,2,IH2) = AM2
                RTIDE(2,2,IH2) = BM2
                RTIDE(1,3,IH2) = AK1
                RTIDE(2,3,IH2) = BK1
                RTIDE(1,4,IH2) = AO1
                RTIDE(2,4,IH2) = BO1
                RTIDE(1,5,IH2) = AN2
                RTIDE(2,5,IH2) = BN2
                RTIDE(1,6,IH2) = AK2
                RTIDE(2,6,IH2) = BK2
                RTIDE(1,7,IH2) = AP1
                RTIDE(2,7,IH2) = BP1
                RTIDE(1,8,IH2) = AQ1
                RTIDE(2,8,IH2) = BQ1
                HTIDE(1,IH2) = H0
              ELSE
                CALL ERRMSG('INBOUN',6484)
                WRITE(LP,*) 'TIDAL DATA IS NONE'
                CALL ABORT1('')
              END IF
            END IF
C
C .................. Y方向流入境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'INLET-J' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6485)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=INLET-J'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            JDUM=ITMP(1)
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(1),JDUM,3,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NINLT = NINLT + 1
            IF( NINLT.GT.NINLSZ ) THEN
               CALL ERRMSG('INBOUN',6486)
               WRITE(LP,*) 'THE NUMBER OF INLET BOUNDARY MAY ',
     $                     'NOT BE OVER NINLSZ'
               WRITE(LP,*) 'VARIABLE=INLET-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6487)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=INLET-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(1)
            IAREA(4,NAREA) = ITMP(1)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 2
            MINLT(NINLT)   = NAREA
            IINLT(1,NINLT) = IU1
            IINLT(2,NINLT) = IV1
            IINLT(3,NINLT) = IW1
            IINLT(4,NINLT) = IH1
            IINLT(5,NINLT) = IT1
            IINLT(6,NINLT) = IC1
            IINLT(7,NINLT) = IK1
            IINLT(8,NINLT) = IE1
            IF( IU1.EQ.0 ) RINLT(1,NINLT) = RU1
            IF( IV1.EQ.0 ) RINLT(2,NINLT) = RV1
            IF( IW1.EQ.0 ) RINLT(3,NINLT) = RW1
            IF( IH1.EQ.0 ) RINLT(4,NINLT) = RH1
            IF( IT1.EQ.0 ) RINLT(5,NINLT) = RT1
            IF( IC1.EQ.0 ) RINLT(6,NINLT) = RC1
            IF( IK1.EQ.0 ) RINLT(7,NINLT) = RK1
            IF( IE1.EQ.0 ) RINLT(8,NINLT) = RE1
C
            IF(IHTYP1.EQ.2) THEN
              IH2 = 0
              DO 255 M=1,9
                IH2 = MAX(IH2,IHA(M))
  255         CONTINUE
              IF(IH2.GT.NOTFSZ) THEN
                CALL ERRMSG('INBOUN',6488)
                WRITE(LP,*) 'THE NUMBER OF TIDAL DATA MAY ',
     $                      'NOT BE OVER NOTFSZ'
                WRITE(LP,*) 'VARIABLE=INLET-J'
                WRITE(LP,*) 'LINE=',CLINE
                CALL ABORT1('')
              ELSE IF(IH2.GT.0) THEN
                IINLT(4,NINLT) = -IH2
                RTIDE(1,1,IH2) = AS2
                RTIDE(2,1,IH2) = BS2
                RTIDE(1,2,IH2) = AM2
                RTIDE(2,2,IH2) = BM2
                RTIDE(1,3,IH2) = AK1
                RTIDE(2,3,IH2) = BK1
                RTIDE(1,4,IH2) = AO1
                RTIDE(2,4,IH2) = BO1
                RTIDE(1,5,IH2) = AN2
                RTIDE(2,5,IH2) = BN2
                RTIDE(1,6,IH2) = AK2
                RTIDE(2,6,IH2) = BK2
                RTIDE(1,7,IH2) = AP1
                RTIDE(2,7,IH2) = BP1
                RTIDE(1,8,IH2) = AQ1
                RTIDE(2,8,IH2) = BQ1
                HTIDE(1,IH2) = H0
              ELSE
                CALL ERRMSG('INBOUN',6489)
                WRITE(LP,*) 'TIDAL DATA IS NONE'
                CALL ABORT1('')
              END IF
            END IF
C
C .................. Z方向流入境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'INLET-K' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6490)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=INLET-K'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(4),ITMP(5),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NINLT = NINLT + 1
            IF( NINLT.GT.NINLSZ ) THEN
               CALL ERRMSG('INBOUN',6491)
               WRITE(LP,*) 'THE NUMBER OF INLET BOUNDARY MAY ',
     $                     'NOT BE OVER NINLSZ'
               WRITE(LP,*) 'VARIABLE=INLET-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6492)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=INLET-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(4)
            IAREA(4,NAREA) = ITMP(5)
            IAREA(5,NAREA) = ITMP(1)
            IAREA(6,NAREA) = ITMP(1)
            IAREA(7,NAREA) = 3
            MINLT(NINLT)   = NAREA
            IINLT(1,NINLT) = IU1
            IINLT(2,NINLT) = IV1
            IINLT(3,NINLT) = IW1
            IINLT(4,NINLT) = IH1
            IINLT(5,NINLT) = IT1
            IINLT(6,NINLT) = IC1
            IINLT(7,NINLT) = IK1
            IINLT(8,NINLT) = IE1
            IF( IU1.EQ.0 ) RINLT(1,NINLT) = RU1
            IF( IV1.EQ.0 ) RINLT(2,NINLT) = RV1
            IF( IW1.EQ.0 ) RINLT(3,NINLT) = RW1
            IF( IH1.EQ.0 ) RINLT(4,NINLT) = RH1
            IF( IT1.EQ.0 ) RINLT(5,NINLT) = RT1
            IF( IC1.EQ.0 ) RINLT(6,NINLT) = RC1
            IF( IK1.EQ.0 ) RINLT(7,NINLT) = RK1
            IF( IE1.EQ.0 ) RINLT(8,NINLT) = RE1
C
C .................. X方向自由流入出境界に関する条件(最新の条件を使用)
C
C .................. 開境界に関する一般的条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'TYPE-H' ) THEN
            CALL GETC(CTMP,13)
C ......... (造波水位：勾配ゼロ)
            IF( CTMP.EQ.'FREE         ' ) THEN
               IHTYP1 = 0
C ......... (造波水位:時間テーブル)
            ELSE IF( CTMP.EQ.'TABLE        ' .OR.
     &               CTMP.EQ.'FIX          ' ) THEN
               IHTYP1 = 1
C ......... (造波水位:天文潮)
            ELSE IF( CTMP.EQ.'TIDE         ' ) THEN
               IHTYP1 = 2
            ELSE
               CALL ERRMSG('INBOUN',6493)
               WRITE(LP,*) 'VALUE MUST BE FREE OR ',
     $                     'FIX'
               WRITE(LP,*) 'VARIABLE=TYPE-H'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FREE-I' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6494)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=FREE-I'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IDUM=ITMP(1)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),ITMP(3),2,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOUTLT = NOUTLT + 1
            IF( NOUTLT.GT.NOTFSZ ) THEN
               CALL ERRMSG('INBOUN',6495)
               WRITE(LP,*) 'THE NUMBER OF FREE BOUNDARY MAY ',
     $                     'NOT BE OVER NOTFSZ'
               WRITE(LP,*) 'VARIABLE=FREE-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6496)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=FREE-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(1)
            IAREA(2,NAREA) = ITMP(1)
            IAREA(3,NAREA) = ITMP(2)
            IAREA(4,NAREA) = ITMP(3)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 1
            MOUTLT(NOUTLT) = NAREA
            IOUTLT(1,NOUTLT) = -1
            IOUTLT(2,NOUTLT) = -1
            IOUTLT(3,NOUTLT) = 0
            IF(ITTYP1.EQ.1) THEN
              IOUTLT(1,NOUTLT) = IT1
              IF( IT1.EQ.0 ) ROUTLT(1,NOUTLT) = RT1
            END IF
            IF(ICTYP1.EQ.1) THEN
              IOUTLT(2,NOUTLT) = IC1
              IF( IC1.EQ.0 ) ROUTLT(2,NOUTLT) = RC1
            END IF
            IF(IHTYP1.EQ.1) THEN
              IF(IH1.GT.0) THEN
                IOUTLT(3,NOUTLT) = IH1
              ELSE
                CALL ERRMSG('INBOUN',6497)
                WRITE(LP,*) 'H-TABLE IS NONE'
                CALL ABORT1('')
              END IF
            ELSE IF(IHTYP1.EQ.2) THEN
              IH2 = 0
              DO 245 M=1,9
                IH2 = MAX(IH2,IHA(M))
  245         CONTINUE
              IF(IH2.GT.0) THEN
                IOUTLT(3,NOUTLT) = -IH2
                RTIDE(1,1,IH2) = AS2
                RTIDE(2,1,IH2) = BS2
                RTIDE(1,2,IH2) = AM2
                RTIDE(2,2,IH2) = BM2
                RTIDE(1,3,IH2) = AK1
                RTIDE(2,3,IH2) = BK1
                RTIDE(1,4,IH2) = AO1
                RTIDE(2,4,IH2) = BO1
                RTIDE(1,5,IH2) = AN2
                RTIDE(2,5,IH2) = BN2
                RTIDE(1,6,IH2) = AK2
                RTIDE(2,6,IH2) = BK2
                RTIDE(1,7,IH2) = AP1
                RTIDE(2,7,IH2) = BP1
                RTIDE(1,8,IH2) = AQ1
                RTIDE(2,8,IH2) = BQ1
                HTIDE(1,IH2) = H0
              ELSE
                CALL ERRMSG('INBOUN',6498)
                WRITE(LP,*) 'TIDAL DATA IS NONE'
                CALL ABORT1('')
              END IF
            END IF
C
C .................. Y方向自由流入出境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FREE-J' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6499)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=FREE-J'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            JDUM=ITMP(1)
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(1),JDUM,3,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOUTLT = NOUTLT + 1
            IF( NOUTLT.GT.NOTFSZ ) THEN
               CALL ERRMSG('INBOUN',6500)
               WRITE(LP,*) 'THE NUMBER OF FREE BOUNDARY MAY ',
     $                     'NOT BE OVER NOTFSZ'
               WRITE(LP,*) 'VARIABLE=FREE-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6501)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=FREE-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(1)
            IAREA(4,NAREA) = ITMP(1)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 2
            MOUTLT(NOUTLT) = NAREA
            IOUTLT(1,NOUTLT) = -1
            IOUTLT(2,NOUTLT) = -1
            IOUTLT(3,NOUTLT) = 0
            IF(ITTYP1.EQ.1) THEN
              IOUTLT(1,NOUTLT) = IT1
              IF( IT1.EQ.0 ) ROUTLT(1,NOUTLT) = RT1
            END IF
            IF(ICTYP1.EQ.1) THEN
              IOUTLT(2,NOUTLT) = IC1
              IF( IC1.EQ.0 ) ROUTLT(2,NOUTLT) = RC1
            END IF
            IF(IHTYP1.EQ.1) THEN
              IF(IH1.GT.0) THEN
                IOUTLT(3,NOUTLT) = IH1
              ELSE
                CALL ERRMSG('INBOUN',6502)
                WRITE(LP,*) 'H-TABLE IS NONE'
                CALL ABORT1('')
              END IF
            ELSE IF(IHTYP1.EQ.2) THEN
              IH2 = 0
              DO 250 M=1,9
                IH2 = MAX(IH2,IHA(M))
  250         CONTINUE
              IF(IH2.GT.0) THEN
                IOUTLT(3,NOUTLT) = -IH2
                RTIDE(1,1,IH2) = AS2
                RTIDE(2,1,IH2) = BS2
                RTIDE(1,2,IH2) = AM2
                RTIDE(2,2,IH2) = BM2
                RTIDE(1,3,IH2) = AK1
                RTIDE(2,3,IH2) = BK1
                RTIDE(1,4,IH2) = AO1
                RTIDE(2,4,IH2) = BO1
                RTIDE(1,5,IH2) = AN2
                RTIDE(2,5,IH2) = BN2
                RTIDE(1,6,IH2) = AK2
                RTIDE(2,6,IH2) = BK2
                RTIDE(1,7,IH2) = AP1
                RTIDE(2,7,IH2) = BP1
                RTIDE(1,8,IH2) = AQ1
                RTIDE(2,8,IH2) = BQ1
                HTIDE(1,IH2) = H0
              ELSE
                CALL ERRMSG('INBOUN',6503)
                WRITE(LP,*) 'TIDAL DATA IS NONE'
                CALL ABORT1('')
              END IF
            END IF
C
C .................. Z方向自由流入出境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'FREE-K' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6504)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=FREE-K'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(4),ITMP(5),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NOUTLT = NOUTLT + 1
            IF( NOUTLT.GT.NOTFSZ ) THEN
               CALL ERRMSG('INBOUN',6505)
               WRITE(LP,*) 'THE NUMBER OF FREE BOUNDARY MAY ',
     $                     'NOT BE OVER NOTFSZ'
               WRITE(LP,*) 'VARIABLE=FREE-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6506)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=FREE-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(4)
            IAREA(4,NAREA) = ITMP(5)
            IAREA(5,NAREA) = ITMP(1)
            IAREA(6,NAREA) = ITMP(1)
            IAREA(7,NAREA) = 3
            MOUTLT(NOUTLT) = NAREA
            IOUTLT(1,NOUTLT) = -1
            IOUTLT(2,NOUTLT) = -1
            IOUTLT(3,NOUTLT) = -1
            IF(ITTYP1.EQ.1) THEN
              IOUTLT(1,NOUTLT) = IT1
              IF( IT1.EQ.0 ) ROUTLT(1,NOUTLT) = RT1
            END IF
            IF(ICTYP1.EQ.1) THEN
              IOUTLT(2,NOUTLT) = IC1
              IF( IC1.EQ.0 ) ROUTLT(2,NOUTLT) = RC1
            END IF
C
C .................. X方向壁面境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'WALL-I' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6507)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=WALL-I'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            IDUM=ITMP(1)
            CALL MODIJ(ITMP(1),IDUM,ITMP(2),ITMP(3),2,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NWALL = NWALL + 1
            IF( NWALL.GT.NWLLSZ ) THEN
               CALL ERRMSG('INBOUN',6508)
               WRITE(LP,*) 'THE NUMBER OF WALL BOUNDARY MAY ',
     $                     'NOT BE OVER NWLLSZ'
               WRITE(LP,*) 'VARIABLE=WALL-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6509)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=WALL-I'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(1)
            IAREA(2,NAREA) = ITMP(1)
            IAREA(3,NAREA) = ITMP(2)
            IAREA(4,NAREA) = ITMP(3)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 1
            MWALL(1,NWALL) = NAREA
            IF(MLNS.EQ.0.AND.IVTYP1.EQ.2) THEN
              CALL ERRMSG('INBOUN',6510)
              WRITE(LP,*) 'WALL FUNCTION B.C.  ONLY STOC-IC'
              CALL ABORT1('')
            END IF
            MWALL(2,NWALL) = IVTYP1
            MWALL(3,NWALL) = ITTYP1
            IF( IVTYP1.EQ.3 ) THEN
               IWALL(1,NWALL) = IU1
               IWALL(2,NWALL) = IV1
               IWALL(3,NWALL) = IW1
               IF( IU1.EQ.0 ) RWALL(1,NWALL) = RU1
               IF( IV1.EQ.0 ) RWALL(2,NWALL) = RV1
               IF( IW1.EQ.0 ) RWALL(3,NWALL) = RW1
            END IF
            IF( ITTYP1.EQ.1 ) THEN
               IWALL(4,NWALL) = IT1
               IF( IT1.EQ.0 ) RWALL(4,NWALL) = RT1
               IF( ICTYP1.EQ.-1 ) THEN
                 IWALL(5,NWALL) = IC1
                 IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
               END IF
            END IF
            IF( ICTYP1.EQ.1 ) THEN
               IWALL(5,NWALL) = IC1
               IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
            END IF
C .................. Y方向壁面境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'WALL-J' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6511)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=WALL-J'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            JDUM=ITMP(1)
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(1),JDUM,3,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NWALL = NWALL + 1
            IF( NWALL.GT.NWLLSZ ) THEN
               CALL ERRMSG('INBOUN',6512)
               WRITE(LP,*) 'THE NUMBER OF WALL BOUNDARY MAY ',
     $                     'NOT BE OVER NWLLSZ'
               WRITE(LP,*) 'VARIABLE=WALL-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6513)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=WALL-J'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(1)
            IAREA(4,NAREA) = ITMP(1)
            IAREA(5,NAREA) = ITMP(4)
            IAREA(6,NAREA) = ITMP(5)
            IAREA(7,NAREA) = 2
            MWALL(1,NWALL) = NAREA
            IF(MLNS.EQ.0.AND.IVTYP1.EQ.2) THEN
              CALL ERRMSG('INBOUN',6514)
              WRITE(LP,*) 'WALL FUNCTION B.C.  ONLY STOC-IC'
              CALL ABORT1('')
            END IF
            MWALL(2,NWALL) = IVTYP1
            MWALL(3,NWALL) = ITTYP1
            IF( IVTYP1.EQ.3 ) THEN
               IWALL(1,NWALL) = IU1
               IWALL(2,NWALL) = IV1
               IWALL(3,NWALL) = IW1
               IF( IU1.EQ.0 ) RWALL(1,NWALL) = RU1
               IF( IV1.EQ.0 ) RWALL(2,NWALL) = RV1
               IF( IW1.EQ.0 ) RWALL(3,NWALL) = RW1
            END IF
            IF( ITTYP1.EQ.1 ) THEN
               IWALL(4,NWALL) = IT1
               IF( IT1.EQ.0 ) RWALL(4,NWALL) = RT1
               IF( ICTYP1.EQ.-1 ) THEN
                 IWALL(5,NWALL) = IC1
                 IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
               END IF
            END IF
            IF( ICTYP1.EQ.1 ) THEN
               IWALL(5,NWALL) = IC1
               IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
            END IF
C .................. Z方向壁面境界に関する条件(最新の条件を使用)
C
         ELSE IF( CLINE(IS:IE) .EQ. 'WALL-K' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
            IF( NDAT1 .NE. 5 ) THEN
               CALL ERRMSG('INBOUN',6515)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=WALL-K'
               WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
            CALL MODIJ(ITMP(2),ITMP(3),ITMP(4),ITMP(5),1,INSIDE)
            IF( INSIDE.EQ.0 ) GOTO 100
C
            NWALL = NWALL + 1
            IF( NWALL.GT.NWLLSZ ) THEN
               CALL ERRMSG('INBOUN',6516)
               WRITE(LP,*) 'THE NUMBER OF WALL BOUNDARY MAY ',
     $                     'NOT BE OVER NWLLSZ'
               WRITE(LP,*) 'VARIABLE=WALL-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            NAREA = NAREA + 1
            IF( NAREA.GT.NARASZ ) THEN
               CALL ERRMSG('INBOUN',6517)
               WRITE(LP,*) 'THE NUMBER OF AREA MAY NOT BE ',
     $                     'OVER NARASZ IN WHOLE INPUT DATA'
               WRITE(LP,*) 'VARIABLE=WALL-K'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
            IAREA(1,NAREA) = ITMP(2)
            IAREA(2,NAREA) = ITMP(3)
            IAREA(3,NAREA) = ITMP(4)
            IAREA(4,NAREA) = ITMP(5)
            IAREA(5,NAREA) = ITMP(1)
            IAREA(6,NAREA) = ITMP(1)
            IAREA(7,NAREA) = 3
            MWALL(1,NWALL) = NAREA
            IF(MLNS.EQ.0.AND.IVTYP1.EQ.2) THEN
              CALL ERRMSG('INBOUN',6518)
              WRITE(LP,*) 'WALL FUNCTION B.C.  ONLY STOC-IC'
              CALL ABORT1('')
            END IF
            MWALL(2,NWALL) = IVTYP1
            MWALL(3,NWALL) = ITTYP1
            IF( IVTYP1.EQ.3 ) THEN
               IWALL(1,NWALL) = IU1
               IWALL(2,NWALL) = IV1
               IWALL(3,NWALL) = IW1
               IF( IU1.EQ.0 ) RWALL(1,NWALL) = RU1
               IF( IV1.EQ.0 ) RWALL(2,NWALL) = RV1
               IF( IW1.EQ.0 ) RWALL(3,NWALL) = RW1
            END IF
            IF( ITTYP1.EQ.1 ) THEN
               IWALL(4,NWALL) = IT1
               IF( IT1.EQ.0 ) RWALL(4,NWALL) = RT1
               IF( ICTYP1.EQ.-1 ) THEN
                 IWALL(5,NWALL) = IC1
                 IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
               END IF
            END IF
            IF( ICTYP1.EQ.1 ) THEN
               IWALL(5,NWALL) = IC1
               IF( IC1.EQ.0 ) RWALL(5,NWALL) = RC1
            END IF
C .................. 流速に関するデフォルト条件
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DEFAULT-TYPE-V' ) THEN
            CALL GETC(CTMP,13)
            IF( CTMP.EQ.'SLIP         ' ) THEN
               MDWALV = 0
            ELSE IF( CTMP.EQ.'NO-SLIP      ' ) THEN
               MDWALV = 1
            ELSE IF( CTMP.EQ.'WALL-FUNCTION' ) THEN
               MDWALV = 2
            ELSE IF( CTMP.EQ.'SLIP-XY      ' ) THEN
               MDWALV = 3
            ELSE
               CALL ERRMSG('INBOUN',6519)
               WRITE(LP,*) 'VALUE MUST BE SLIP OR NO-SLIP ',
     $                     'OR WALL-FUNCTION'
               WRITE(LP,*) 'VARIABLE=DEFAULT-TYPE-V'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 温度と濃度に関するデフォルト条件
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DEFAULT-TYPE-T' .OR.
     1            CLINE(IS:IE) .EQ. 'DEFAULT-TYPE-C' ) THEN
            CALL GETC(CTMP,13)
            IF( CTMP.EQ.'ADIABATIC    ' ) THEN
               MDWALT = 0
            ELSE IF( CTMP.EQ.'FREE         ' ) THEN
               MDWALT = 0
            ELSE IF( CTMP.EQ.'CONSTANT     ' ) THEN
               MDWALT = 1
            ELSE
               CALL ERRMSG('INBOUN',6520)
               WRITE(LP,*) 'VALUE MUST BE ADIABATIC OR CONSTANT'
               WRITE(LP,*) 'VARIABLE=DEFAULT-TYPE-T'
               WRITE(LP,*) 'VALUE=',CTMP
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 温度に関するデフォルト条件が固定の場合
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DEFAULT-T' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IDWALT = 0
               RDWALT = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IDWALT = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6521)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=DEFAULT-T'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 220 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  220          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6522)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $            'OR 2*N'
               WRITE(LP,*) 'VARIABLE=DEFAULT-T'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C .................. 濃度に関するデフォルト条件が固定の場合
C
         ELSE IF( CLINE(IS:IE) .EQ. 'DEFAULT-C' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (一定値)
            IF( NDAT1.EQ.1 ) THEN
               IDWALC = 0
               RDWALC = RTMP(1)
C
C ......... (時系列値)
            ELSE IF( MOD(NDAT1,2).EQ.0 ) THEN
               NTABLE = NTABLE + 1
               IDWALC = NTABLE
               IF( NTABLE.GT.NTBLSZ ) THEN
                  CALL ERRMSG('INBOUN',6523)
                  WRITE(LP,*) 'THE NUMBER OF TABLE MAY NOT BE ',
     $                         'OVER NTBLSZ IN WHOLE INPUT DATA'
                  WRITE(LP,*) 'VARIABLE=DEFAULT-T'
                  WRITE(LP,*) 'LINE=',CLINE
                  CALL ABORT1('')
               END IF
               ITABLE(NTABLE) = NDAT1/2
               DO 230 M=1,NDAT1/2
                  TTABLE(M,NTABLE) = RTMP(M*2-1)
                  VTABLE(M,NTABLE) = RTMP(M*2)
  230          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6524)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 ',
     $            'OR 2*N'
               WRITE(LP,*) 'VARIABLE=DEFAULT-T'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
C .......開境界における潮汐条件
C
C ......... 主要8分潮
         ELSE IF( CLINE(IS:IE) .EQ. 'M2' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (M2:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AM2 = RTMP(1)
               BM2 = RTMP(2)*PAI/180.0D0
               IHA(2) = IHA(2)+1
            ELSE
               CALL ERRMSG('INBOUN',6525)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=M2'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'S2' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (S2:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AS2 = RTMP(1)
               BS2 = RTMP(2)*PAI/180.0D0
               IHA(1) = IHA(1)+1
            ELSE
               CALL ERRMSG('INBOUN',6526)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=S2'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'N2' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (N2:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AN2 = RTMP(1)
               BN2 = RTMP(2)*PAI/180.0D0
               IHA(5) = IHA(5)+1
            ELSE
               CALL ERRMSG('INBOUN',6527)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=N2'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'K2' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (K2:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AK2 = RTMP(1)
               BK2 = RTMP(2)*PAI/180.0D0
               IHA(6) = IHA(6)+1
            ELSE
               CALL ERRMSG('INBOUN',6528)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=K2'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'K1' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (K1:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AK1 = RTMP(1)
               BK1 = RTMP(2)*PAI/180.0D0
               IHA(3) = IHA(3)+1
            ELSE
               CALL ERRMSG('INBOUN',6529)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=K1'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'O1' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (O1:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AO1 = RTMP(1)
               BO1 = RTMP(2)*PAI/180.0D0
               IHA(4) = IHA(4)+1
            ELSE
               CALL ERRMSG('INBOUN',6530)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=O1'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'P1' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (P1:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AP1 = RTMP(1)
               BP1 = RTMP(2)*PAI/180.0D0
               IHA(7) = IHA(7)+1
            ELSE
               CALL ERRMSG('INBOUN',6531)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=P1'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'Q1' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (Q1:振幅,遅角)
            IF( NDAT1.EQ.2 ) THEN
               AQ1 = RTMP(1)
               BQ1 = RTMP(2)*PAI/180.0D0
               IHA(8) = IHA(8)+1
            ELSE
               CALL ERRMSG('INBOUN',6532)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2 '
               WRITE(LP,*) 'VARIABLE=Q1'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'H0' ) THEN
            CALL MGETR(RTMP,NDAT1,2*NTIMSZ)
C ......... (H0:振幅基準水位)
            IF( NDAT1.EQ.1 ) THEN
               H0 = RTMP(1)
               HTIDE0=RTMP(1)
               IHA(9) = IHA(9)+1
            ELSE
               CALL ERRMSG('INBOUN',6533)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 '
               WRITE(LP,*) 'VARIABLE=H0'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'TIDE' ) THEN
            CALL MGETI(ITMP,NDAT1,20)
C ......... (計算分潮数)
            IF( NDAT1.EQ.1 ) THEN
               NTIDE = ITMP(1)
            ELSE
               CALL ERRMSG('INBOUN',6534)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 1 '
               WRITE(LP,*) 'VARIABLE=TIDE'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'OMEGA' ) THEN
            CALL MGETR(RTMP,NDAT1,20)
C ......... (OMEGA:周期入力、デフォルト周期の変更)
            IF( NDAT1.EQ.8 ) THEN
               DO 260 M=1,8
                 ROMEG(M)=RTMP(M)
                 IF(ROMEG(M).GT.0.0D0) ROMEG(M)=2.0D0*PAI/ROMEG(M)
  260          CONTINUE
            ELSE
               CALL ERRMSG('INBOUN',6535)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 8 '
               WRITE(LP,*) 'VARIABLE=OMEGA'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
         ELSE IF( CLINE(IS:IE) .EQ. 'WAVE' ) THEN
            CALL MGETR(RTMP,NDAT1,20)
C ......... (WAVE:微小振幅波境界条件)
            IF( NDAT1.EQ.5 ) THEN
               AMP = RTMP(1)
               TTT = RTMP(2)
               IF(TTT.LE.0.0D0) TTT=40.0D0
               ALL = RTMP(3)
               IF(ALL.LE.0.0D0) ALL=125.2D0
               HHH = RTMP(4)
               IF(HHH.LE.0.0D0) HHH=1.0D0
               AXX = RTMP(5)
            ELSE
               CALL ERRMSG('INBOUN',6536)
               WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 5'
               WRITE(LP,*) 'VARIABLE=OMEGA'
               WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
C ..... (時間発展海底変化条件)
         ELSE IF( CLINE(IS:IE) .EQ. 'SEA-BOTTOM' ) THEN
            CALL GETC(CTMP,4)
            IF( CTMP.EQ.'OFF ' ) THEN
               NBOT = 0
            ELSE IF( CTMP.EQ.'ON  ' ) THEN
               NBOT = 1
cadd 20130703(s)
            ELSE IF( CTMP.EQ.'CALC' ) THEN
               NBOT = -1
cadd 20130703(e)
            END IF

               WRITE(LP,*) 'SEA-BOTTOM FLAG ON MLNS =',MLNS

C
         ELSE IF( CLINE(IS:IE) .EQ. 'FAULT-SYSTEM' ) THEN
            CALL GETC(CTMP,7)
            IF( CTMP.EQ.'TOKYO  ' ) THEN
               JSYSTEM = 1
            ELSE IF( CTMP.EQ.'JGD2000' ) THEN
               JSYSTEM = 2
            ELSE IF( CTMP.EQ.'WGS84  ' ) THEN
               JSYSTEM = 3
            ELSE
               CALL ERRMSG('INBOUN',6537)
               WRITE(LP,*)'VALUE MUST BE TOKYO OR JGD2000 OR WGS84'
               WRITE(LP,*) 'VARIABLE=FAULT-SYSTEM'
               WRITE(LP,*) 'VALUE=',trim(CTMP)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'GRID-SYSTEM' ) THEN
            CALL GETC(CTMP,7)
            IF( CTMP.EQ.'TOKYO  ' ) THEN
               ISYSTEM = 1
            ELSE IF( CTMP.EQ.'JGD2000' ) THEN
               ISYSTEM = 2
            ELSE IF( CTMP.EQ.'WGS84  ' ) THEN
               ISYSTEM = 3
            ELSE
               CALL ERRMSG('INBOUN',6538)
               WRITE(LP,*)'VALUE MUST BE TOKYO OR JGD2000 OR WGS84'
               WRITE(LP,*) 'VARIABLE=GRID-SYSTEM'
               WRITE(LP,*) 'VALUE=',trim(CTMP)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'COORDINATE' ) THEN
            CALL GETC(CTMP,23)
            IF( CTMP.EQ.'JAPAN-PLANE-RECTANGULAR' ) THEN
               LL = 1
            ELSE IF( CTMP.EQ.'UTM                    ' ) THEN
               LL = 0
            ELSE IF( CTMP.EQ.'LONGITUDE-LATITUDE     ' ) THEN
               LL = 2
            ELSE
               CALL ERRMSG('INBOUN',6539)
               WRITE(LP,*) 'VALUE MUST BE JAPAN-PLANE-RECTANGULAR OR'
               WRITE(LP,*) '   UTM OR LONGITUDE-LATITUDE'
               WRITE(LP,*) 'VARIABLE=COORDINATE'
               WRITE(LP,*) 'VALUE=',trim(CTMP)
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'RECTANGULAR-ZONE' ) THEN
            CALL GETI(ICOORD)
            IF( ICOORD.LT.1.OR.ICOORD.GT.19 ) THEN
               CALL ERRMSG('INBOUN',6540)
               WRITE(LP,*)'VALUE MUST BE 1-19'
               WRITE(LP,*) 'VARIABLE=RECTANGULAR-ZONE'
               WRITE(LP,*) 'VALUE=',ICOORD
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            ENDIF
C
         ELSE IF( CLINE(IS:IE) .EQ. 'UTM-CENTER' ) THEN
            CALL GETR(LC_DEG)
C
C .....  水上10m風速の係数
         ELSE IF( CLINE(IS:IE) .EQ. 'COEF-WIND10M' ) THEN
            CALL GETR(AWIND10)
C
C .....  アルベド
         ELSE IF( CLINE(IS:IE) .EQ. 'ALBEDO' ) THEN
            CALL GETR(ALBEDO)
C
         ELSE
            CALL ERRMSG('INBOUN',6541)
            WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
            CALL ABORT1('')
         END IF
  100 CONTINUE
  300 CONTINUE
C
C
C ... 断層パラメータから水位変動量を計算する場合
      IF( NBOT.EQ.-1 ) THEN
C
C ...... (a) UTMの場合
         IF( LL.EQ.0 ) THEN
            ICOORD=0
C
            IF(LC_DEG.LT.-999.D0) THEN
               CALL ERRMSG('INBOUN',6542)
               WRITE(LP,*) 'UTM-CENTER IS NOT DEFINED'
               CALL ABORT1('')
            ENDIF
            CALL SET_UTM(LC_DEG)
C
C ...... (b) 19座標系の場合
         ELSEIF( LL.EQ.1 ) THEN
            IF( ICOORD.LE.0.OR.ICOORD.GT.19 ) THEN
               CALL ERRMSG('INBOUN',6543)
               WRITE(LP,*) 'RCTANGULAR-ZONE(1-19) IS NOT DEFINED'
               CALL ABORT1('')
            ENDIF
C
C
C ...... (c) 緯度経度座標系の場合
         ELSEIF( LL.EQ.2 ) THEN
            ICOORD=-1
C
C ...... (d) その他
         ELSE
            CALL ERRMSG('INBOUN',6544)
            WRITE(LP,*) 'IF SEA-BOTTOM=CALC, COORDINATE MUST BE SET.'
            CALL ABORT1('')
         ENDIF
      ENDIF
C
      RETURN
C
C ... 読み込みエラー
  900 CONTINUE
      CALL ERRMSG('INBOUN',6545)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
C
  901 CONTINUE
      CALL ERRMSG('INBOUN',6546)
      WRITE(LP,*) 'FILE OPEN ERROR: TIME TABLE FILE'
      WRITE(LP,*) 'FILE NUMBER=',IFLTM
      CALL ABORT1('')
C
      END
