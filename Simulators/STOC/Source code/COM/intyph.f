C===========================================================
C     オリジナル
C     GLONG SUB PROGRAM ( STORM SURGE )   Ver.2 (1994/07/25)
C     SUBROUTINE TYHRD
C     STOC用に変更
C===========================================================
C-----------------------------------------------------------
      SUBROUTINE INTYPH
C-----------------------------------------------------------
      IMPLICIT NONE
C
      INTEGER,PARAMETER::NTYHIN=100
C
      INCLUDE 'TYPHOI.h'
      INCLUDE 'TYPHOR.h'
      INCLUDE 'FILE.h'
      INCLUDE 'FILEC.h'
C
      REAL(8)::TTX(NTYHIN),TTY(NTYHIN),TTP(NTYHIN),TTR(NTYHIN)
      REAL(8)::TTU(NTYHIN),TTV(NTYHIN)
      INTEGER::ITM(NTYHIN)
      REAL(8)::RTMP(10)
      INTEGER::ITMP(10)
C
      REAL(8)::ER,PP,PPP,RR,RX,RY,TDUR,TL,TSP,TSP1,TSP2
      REAL(8)::TT,TTPD,TTRD,TTXD,TTYD
      INTEGER::I,IE,IERR,IS,L,M,N,N1,N2,N3,N4,NDAT1,NN
C
C-----------------------------------------------------------
      ER = 6377397.15D0
      PP = 3.14159265358979D0
      PPP=PP/180.D0
C-----------------------------------------------------------
CC      READ(03,100)
CC      READ(03,100)
CC      READ(03,100)
C
      N=0
      DO 100 L=1,100000
        CALL GET1(IS,IE,IERR)
        IF( IERR.GT.0 ) GO TO 900
c        write(*,*) 'debug:intyph:variable=',cline(is:ie)
C
        IF( CLINE(IS:IE) .EQ. '%END' ) THEN
          GO TO 200
C
        ELSE IF( CLINE(IS:IE) .EQ. 'MODEL') THEN
          CALL MGETI(ITMP,NDAT1,3)
          IF(NDAT1.NE.3) THEN
            CALL ERRMSG('INTYPH',6730)
            WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 3'
            WRITE(LP,*) 'VARIABLE=MODEL'
            WRITE(LP,*) 'VALUE=',(ITMP(I),' ',I=1,NDAT1)
            WRITE(LP,*) 'LINE=',CLINE
            CALL ABORT1('')
          END IF
          IHNC1 = ITMP(1)
          IHNCM = ITMP(2)
          IHNDS = ITMP(3)
C
        ELSE IF( CLINE(IS:IE) .EQ. 'PARAM-C') THEN
          CALL MGETR(RTMP,NDAT1,10)
          IF(NDAT1.NE.2) THEN
            CALL ERRMSG('INTYPH',6731)
            WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2'
            WRITE(LP,*) 'VARIABLE=PARAM-C'
            WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
            WRITE(LP,*) 'LINE=',CLINE
            CALL ABORT1('')
          END IF
          C1 = RTMP(1)
          C2 = RTMP(2)
C
        ELSE IF( CLINE(IS:IE) .EQ. 'GLOBAL-ORG') THEN
          CALL MGETR(RTMP,NDAT1,10)
          IF(NDAT1.NE.2) THEN
            CALL ERRMSG('INTYPH',6732)
            WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 2'
            WRITE(LP,*) 'VARIABLE=GLOBAL-ORG'
            WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
            WRITE(LP,*) 'LINE=',CLINE
            CALL ABORT1('')
          END IF
          XLN = RTMP(1)
          YLT = RTMP(2)
C
        ELSE IF( CLINE(IS:IE) .EQ. 'COURSE') THEN
          CALL MGETR(RTMP,NDAT1,10)
          IF(NDAT1.NE.10) THEN
            CALL ERRMSG('INTYPH',6733)
            WRITE(LP,*) 'THE NUMBER OF DATA MUST BE 10'
            WRITE(LP,*) 'VARIABLE=COURSE'
            WRITE(LP,*) 'VALUE=',(RTMP(I),' ',I=1,NDAT1)
            WRITE(LP,*) 'LINE=',CLINE
            CALL ABORT1('')
          END IF
          N1 = NINT(RTMP(1))
          N2 = NINT(RTMP(2))
          N3 = NINT(RTMP(3))
          N4 = NINT(RTMP(4))
          TTXD = RTMP(6)
          TTYD = RTMP(7)
          TTPD = RTMP(8)
          TTRD = RTMP(9)
          TSP  = RTMP(10)
C
          N=N+1
          IF( N.GT.NTYHIN ) THEN
               CALL ERRMSG('INTYPH',6734)
               WRITE(LP,*) 'THE NUMBER OF COURSE AREA MAY ',
     $                     'NOT BE OVER NTYHIN'
               WRITE(LP,*) 'VARIABLE=COURSE'
               WRITE(LP,*) 'LINE=',CLINE
               CALL ABORT1('')
            END IF
C
          IF(N.EQ.1)THEN
            NS1=N1
            NS2=N2
            NS3=N3
            NS4=N4
            NS5=0
          END IF
C
          TTX(N) = TTXD
          TTY(N) = TTYD
          TTP(N) = TTPD
          TTR(N) = TTRD
C
          CALL CTIME1(N1,N2,N3,N4,NS1,NS2,NS3,NS4,ITM(N))
C
          IF(N.NE.1) THEN
            TL=0.5D0*(TTY(N)+TTY(N-1))*PPP
            RX=ER*(TTX(N)-TTX(N-1))*PPP*COS(TL)
            RY=ER*(TTY(N)-TTY(N-1))*PPP
            RR=SQRT(RX**2+RY**2)
            IF(RR.GT.1.0D-10)THEN
              TTU(N)=TSP*(RX/RR)
              TTV(N)=TSP*(RY/RR)
            ELSE
              TTU(N)=0.0D0
              TTV(N)=0.0D0
            ENDIF
          ENDIF
C
          IF(N.EQ.1) TSP1=TSP
          IF(N.EQ.2) TSP2=TSP
C
        ELSE
          CALL ERRMSG('INTYPH',6735)
          WRITE(LP,*) 'UNKNOWN VARIABLE NAME: ',CLINE(IS:IE)
          CALL ABORT1('')
        END IF
 100  CONTINUE
C
C
  200 CONTINUE
      NTYH=ITM(N)+1
C
      IF( NTYH.GT.NTYHSZ ) THEN
        CALL ERRMSG('INTYPH',6736)
        WRITE(LP,*) 'THE NUMBER OF COURSE AREA MAY ',
     $              'NOT BE OVER NTYHSZ'
        WRITE(LP,*) 'VARIABLE=COURSE'
        CALL ABORT1('')
      END IF
C
      NE1=N1
      NE2=N2
      NE3=N3
      NE4=N4
      NE5=0
      IF(TSP2.GT.1.0D-10) THEN
        TTU(1)=TTU(2)*(TSP1/TSP2)
        TTV(1)=TTV(2)*(TSP1/TSP2)
      ELSE
        TTU(1)=0.0D0
        TTV(1)=0.0D0
      ENDIF
C
      DO 310 N=1,NTYH
        NN=N-1
        M=0
  311   CONTINUE
        M=M+1
        IF(NN.EQ.ITM(M))GOTO 312
        IF(NN.LT.ITM(M))GOTO 313
        GO TO 311
C
  312   CONTINUE
        TX(N)=TTX(M)
        TY(N)=TTY(M)
        TP(N)=TTP(M)
        TR(N)=TTR(M)
        TU(N)=TTU(M)
        TV(N)=TTV(M)
        GO TO 310
C
  313   CONTINUE
        TDUR=ITM(M)-ITM(M-1)
        TT=NN-ITM(M-1)
        TX(N)=(TT*TTX(M)+(TDUR-TT)*TTX(M-1))/TDUR
        TY(N)=(TT*TTY(M)+(TDUR-TT)*TTY(M-1))/TDUR
        TP(N)=(TT*TTP(M)+(TDUR-TT)*TTP(M-1))/TDUR
        TR(N)=(TT*TTR(M)+(TDUR-TT)*TTR(M-1))/TDUR
        TU(N)=(TT*TTU(M)+(TDUR-TT)*TTU(M-1))/TDUR
        TV(N)=(TT*TTV(M)+(TDUR-TT)*TTV(M-1))/TDUR
  310 CONTINUE
C-----------------------------------------------------------
      WRITE(6,400) NTYH
  400 FORMAT(1H ,5X,'************ TYPHOON DATA ***************',/,
     &       10X,'NTYH=',I3,/)
      DO 410 N=1,NTYH
        WRITE(6,420)N,TX(N),TY(N),TP(N),TR(N),TU(N),TV(N)
  410 CONTINUE
  420 FORMAT(1H ,10X,I3,2X,2F10.3,4F10.1)
      WRITE(6,430) NS1,NS2,NS3,NS4,NS5
  430 FORMAT('NS1,NS2,NS3,NS4,NS5=',5I10)
      WRITE(6,440) NE1,NE2,NE3,NE4,NE5
  440 FORMAT('NE1,NE2,NE3,NE4,NE5=',5I10)
C-----------------------------------------------------------
      RETURN
C
C ... 読詠み込みエラー
  900 CONTINUE
      CALL ERRMSG('INTYPH',6737)
      WRITE(LP,*) 'END OF FILE: INPUT DATA IS INCOMPLETE'
      CALL ABORT1('')
      END
