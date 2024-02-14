      SUBROUTINE ARCHLD(IXS,IXE,JYS,JYE,KZS,KZE)
C=========================================================
C     子領域の格子点座標値（始点、終点）を求める
C=========================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GRID.h'
      INCLUDE 'DOMAIN.h'
      INCLUDE 'FILE.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(INOUT)::IXE,IXS,JYE,JYS,KZE,KZS
C
      REAL(8)::XYZ(6)
      INTEGER::IXYZ(3)
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      REAL(8)::EPS=1.0D-3
      INTEGER::ITRACE=0
C
      REAL(8)::XE,XS,YE,YS,ZE,ZS
      INTEGER::I,ICHILD,IERR,IERROR,IPARNT,IREQ,ISTOP,ITAG,J,K,NCOUNT
      INTEGER::N,NS,NE,MXG0,MYG0,MZG0,MX0,MY0,ICHILN,NNS,NNW,NNE,NNN
      INTEGER::ICODE
C
      IPARNT = IPECON(2,NRANK+1)
      ICHILD = IPECON(3,NRANK+1)
C
      ISTOP = 0
      IXS = 0
      IXE = 0
      JYS = 0
      JYE = 0
      KZS = 0
      KZE = 0
      IF(IPARNT.GE.0) THEN
C
        XYZ(1) = XGRID(1)
        XYZ(2) = XGRID(MXM)
        XYZ(3) = YGRID(1)
        XYZ(4) = YGRID(MYM)
        XYZ(5) = ZGRID(1)
        XYZ(6) = ZGRID(MZM)
        IXYZ(1) = MXM-1
        IXYZ(2) = MYM-1
        IXYZ(3) = MZM-1
C
        NCOUNT = 6
        ITAG = 0
        CALL MPI_ISEND( XYZ,NCOUNT,MPI_DOUBLE_PRECISION,IPARNT,ITAG,
     &                  comm_model,IREQ,IERR )
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
        NCOUNT = 3
        ITAG = 0
        CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,IPARNT,ITAG,
     &                  comm_model,IREQ,IERR )
        CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
        IF(NUMPE(1,IPARNT+1).GT.1) THEN
          CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,IPARNT,
     &                    MPI_ANY_TAG,comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
          MXG = IXYZ(1)
          MYG = IXYZ(2)
          MZG = IXYZ(3)
        ENDIF
      END IF
C
      IF(ICHILD.GE.0) THEN
C
        NS = NUMPE(2,NRANK+1)
        NE = NS+NUMPE(1,NRANK+1)-1
        XS = 1.0D30
        YS = 1.0D30
        ZS = 1.0D30
        XE = - 1.0D30
        YE = - 1.0D30
        ZE = - 1.0D30
        MXG0 = 0
        MYG0 = 0
        MZG0 = 0
        MX0 = 0
        MY0 = 0
        DO 50 N=NS,NE
          ICHILN = NUMCOM(1,N)
          NCOUNT = 6
          CALL MPI_IRECV( XYZ,NCOUNT,MPI_DOUBLE_PRECISION,ICHILN,
     &                    MPI_ANY_TAG,comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
          NCOUNT = 3
          CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,ICHILN,
     &                    MPI_ANY_TAG,comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
          XS = MIN(XS,XYZ(1))
          XE = MAX(XE,XYZ(2))
          YS = MIN(YS,XYZ(3))
          YE = MAX(YE,XYZ(4))
          ZS = MIN(ZS,XYZ(5))
          ZE = MAX(ZE,XYZ(6))
          IF(NUMPE(1,NRANK+1).GT.1) THEN
            IF(N.EQ.NS) THEN
              MXG0 = IXYZ(1)
              MYG0 = IXYZ(2)
              MZG0 = IXYZ(3)
              NNS = ICHILN
              NNW = ICHILN
              NNE = IPECON(6,ICHILN+1)
              NNN = IPECON(7,ICHILN+1)
              MX0 = 1
              MY0 = 1
              IF(IPECON(4,ICHILN+1).GE.0.OR.
     &           IPECON(5,ICHILN+1).GE.0) THEN
                WRITE(LP,*) '### ( SOUTH,WEST ) PE NO. ### =',
     &                       IPECON(4,ICHILN+1),IPECON(5,ICHILN+1)
                ISTOP = 1
              END IF
            ELSE
              IF(NNE.EQ.ICHILN.AND.NNW.EQ.IPECON(5,ICHILN+1)) THEN
                MXG0 = MXG0+IXYZ(1)
                MX0 = MX0+1
                NNW = ICHILN
                NNE = IPECON(6,ICHILN+1)
              END IF
              IF(NNN.EQ.ICHILN.AND.NNS.EQ.IPECON(4,ICHILN+1)) THEN
                MYG0 = MYG0+IXYZ(2)
                MY0 = MY0+1
                NNS = ICHILN
                NNN = IPECON(7,ICHILN+1)
              END IF
            END IF
          END IF
   50   CONTINUE
C
        MXG0 = MXG0+2
        MYG0 = MYG0+2
        MZG0 = MZG0+2
        IF(NPROC.GT.1) THEN
          WRITE(LP,*) '### GLOBAL MESH MXG,MYG,MZG=',MXG0,MYG0,MZG0
          WRITE(LP,*) '### ( X-BLOCK,Y-BLOCK ) =',
     &                 MX0,MY0,NUMPE(1,NRANK+1)
        END IF
C
        IF( ICORDTYPE.EQ.2.AND.
     $      REGION(3)-REGION(1).GT.0.0D0 .AND.
     $      REGION(4)-REGION(2).GT.0.0D0 ) THEN  ! 球面が直交と接続
C          SKIP
        ELSE
        DO 100 I=1,MXM
          IF(ABS(XS-XGRID(I)).LT.EPS) IXS=I
          IF(ABS(XE-XGRID(I)).LT.EPS) IXE=I
  100   CONTINUE
        DO 150 J=1,MYM
          IF(ABS(YS-YGRID(J)).LT.EPS) JYS=J
          IF(ABS(YE-YGRID(J)).LT.EPS) JYE=J
  150   CONTINUE
        ENDIF
        DO 200 K=1,MZM
          IF(KZS.EQ.0.AND.ABS(ZS-ZGRID(K)).LT.EPS) KZS=K
          IF(KZS.EQ.0.AND.ZS.LT.ZGRID(K)) KZS=K-1
          IF(KZE.EQ.0.AND.ABS(ZE-ZGRID(K)).LT.EPS) KZE=K
          IF(KZE.EQ.0.AND.ZE.LT.ZGRID(K)) KZE=K
  200   CONTINUE
        IF(ITRACE.NE.0) WRITE(LP,600) IXS,IXE,JYS,JYE,KZS,KZE,XYZ
  600   FORMAT('# DEBUG IXS,IXE,JYS,JYE,KZS,KZE =',6I10
     $          /'       XS, XE, YS, YE, ZS, ZE =',6F10.2)
C
        IF( ICORDTYPE.EQ.2.AND.
     $      REGION(3)-REGION(1).GT.0.0D0 .AND.
     $      REGION(4)-REGION(2).GT.0.0D0 ) THEN  ! 球面が直交と接続
C          SKIP
        ELSE
        IF(IXS*IXE*JYS*JYE*KZS*KZE.EQ.0) THEN
          CALL ERRMSG('ARCHLD',6800)
          WRITE(LP,900) IXS,IXE,JYS,JYE,KZS,KZE,XYZ
  900     FORMAT('### CHILD REGION COORDINATE ERROR ###'
     $          /'    IXS,IXE,JYS,JYE,KZS,KZE =',6I10
     $          /'     XS, XE, YS, YE, ZS, ZE =',6F10.2)
          CALL ABORT1('')
        END IF
        ENDIF
C                  セル番号に変更
        IXS = IXS+1
        JYS = JYS+1
        KZS = KZS+1
C
        IF(NUMPE(1,NRANK+1).GT.1) THEN
          NS = NUMPE(2,NRANK+1)
          NE = NS+NUMPE(1,NRANK+1)-1
          IXYZ(1) = MXG0
          IXYZ(2) = MYG0
          IXYZ(3) = MZG0
          DO 300 N=NS,NE
            ICHILN = NUMCOM(1,N)
            NCOUNT = 3
            ITAG = 0
            CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,ICHILN,ITAG,
     &                      comm_model,IREQ,IERR )
            CALL MPI_WAIT(IREQ,ISTAT,IERROR)
  300     CONTINUE
        END IF
C
      END IF
C
C     親領域がない場合(最外側の領域分割)
C
      IF(IPARNT.LT.0.AND.NUMPE(1,0).GT.1) THEN
        XYZ(1) = XGRID(1)
        XYZ(2) = XGRID(MXM)
        XYZ(3) = YGRID(1)
        XYZ(4) = YGRID(MYM)
        XYZ(5) = ZGRID(1)
        XYZ(6) = ZGRID(MZM)
        IXYZ(1) = MXM-1
        IXYZ(2) = MYM-1
        IXYZ(3) = MZM-1
C
        IF(NRANK.NE.0) THEN
          NCOUNT = 6
          ITAG = 0
          CALL MPI_ISEND( XYZ,NCOUNT,MPI_DOUBLE_PRECISION,0,ITAG,
     &                    comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
          NCOUNT = 3
          ITAG = 0
          CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,0,ITAG,
     &                    comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
C
          CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,0,
     &                    MPI_ANY_TAG,comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
          MXG = IXYZ(1)
          MYG = IXYZ(2)
          MZG = IXYZ(3)
C
        ELSE
          NS = NUMPE(2,0)
          NE = NS+NUMPE(1,0)-1
          XS = 1.0D30
          YS = 1.0D30
          ZS = 1.0D30
          XE = - 1.0D30
          YE = - 1.0D30
          ZE = - 1.0D30
          MXG0 = 0
          MYG0 = 0
          MZG0 = 0
          MX0 = 0
          MY0 = 0
          DO 400 N=NS,NE
          IF(N.GT.NS) THEN
            ICHILN = NUMCOM(1,N)
            NCOUNT = 6
            CALL MPI_IRECV( XYZ,NCOUNT,MPI_DOUBLE_PRECISION,ICHILN,
     &                      MPI_ANY_TAG,comm_model,IREQ,IERR )
            CALL MPI_WAIT(IREQ,ISTAT,IERROR)
            NCOUNT = 3
            CALL MPI_IRECV( IXYZ,NCOUNT,MPI_INTEGER,ICHILN,
     &                      MPI_ANY_TAG,comm_model,IREQ,IERR )
            CALL MPI_WAIT(IREQ,ISTAT,IERROR)
          END IF
          XS = MIN(XS,XYZ(1))
          XE = MAX(XE,XYZ(2))
          YS = MIN(YS,XYZ(3))
          YE = MAX(YE,XYZ(4))
          ZS = MIN(ZS,XYZ(5))
          ZE = MAX(ZE,XYZ(6))
          IF(N.EQ.NS) THEN
            MXG0 = IXYZ(1)
            MYG0 = IXYZ(2)
            MZG0 = IXYZ(3)
            NNS = 0
            NNW = 0
            NNE = IPECON(6,1)
            NNN = IPECON(7,1)
            MX0 = 1
            MY0 = 1
            IF(IPECON(4,1).GE.0.OR.IPECON(5,1).GE.0) THEN
              WRITE(LP,*) '### ( SOUTH,WEST ) PE NO. ### =',
     &                     IPECON(4,1),IPECON(5,1)
              ISTOP = 1
            END IF
          ELSE
            IF(NNE.EQ.ICHILN.AND.NNW.EQ.IPECON(5,ICHILN+1)) THEN
              MXG0 = MXG0+IXYZ(1)
              MX0 = MX0+1
              NNW = ICHILN
              NNE = IPECON(6,ICHILN+1)
            END IF
            IF(NNN.EQ.ICHILN.AND.NNS.EQ.IPECON(4,ICHILN+1)) THEN
              MYG0 = MYG0+IXYZ(2)
              MY0 = MY0+1
              NNS = ICHILN
              NNN = IPECON(7,ICHILN+1)
            END IF
          END IF
  400   CONTINUE
        MXG = MXG0+2
        MYG = MYG0+2
        MZG = MZG0+2
        WRITE(LP,*) '### GLOBAL MESH MXG,MYG,MZG=',MXG,MYG,MZG
        WRITE(LP,*) '### ( X-BLOCK,Y-BLOCK ) =',
     &               MX0,MY0,NUMPE(1,NRANK+1)
C
C//BUG 2009.03.10 以下6行をコメント化.or.削除
CC        IXS = 0
CC        JYS = 0
CC        KZS = 0
CC        IXE = 0
CC        JYE = 0
CC        KZE = 0
C//BUG END
C
        IXYZ(1) = MXG
        IXYZ(2) = MYG
        IXYZ(3) = MZG
        DO 450 N=NS+1,NE
          ICHILN = NUMCOM(1,N)
          NCOUNT = 3
          ITAG = 0
          CALL MPI_ISEND( IXYZ,NCOUNT,MPI_INTEGER,ICHILN,ITAG,
     &                    comm_model,IREQ,IERR )
          CALL MPI_WAIT(IREQ,ISTAT,IERROR)
  450   CONTINUE
      END IF
      END IF
C
      RETURN
      END
