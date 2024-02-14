      SUBROUTINE CADMAS_PORS(INDP,INDU,INDV,INDW,LLWALL,
     $                       GV,GX,GY,GZ,ZC,
     $                       KF,KG,KP,FF,HH,HDEP)
C----------------------------------------------------------------------
C     CADMASにSTOCで用いる地形データを渡す
C     その後、CADMAS領域のインデックス、ポロシティ、他変数の値を修正する
C----------------------------------------------------------------------
      use mod_comm,only: comm_ic_mg
      IMPLICIT NONE
C
      INCLUDE 'DOMAIN.h'
      INCLUDE 'OBSTI.h'
      INCLUDE 'mpif.h'
      INCLUDE 'CADMAS.h'
      INCLUDE 'FILE.h'
C
      INTEGER::INDP(MX,MY,MZ),INDU(MX,MY,MZ)
      INTEGER::INDV(MX,MY,MZ),INDW(MX,MY,MZ)
      INTEGER::LLWALL(8,MLWALL)
      REAL(8)::GV(MX,MY,MZ),GX(MX,MY,MZ),GY(MX,MY,MZ),GZ(MX,MY,MZ)
      REAL(8)::ZC(8,MZ)
      INTEGER::KF(MX,MY),KG(MX,MY),KP(MX,MY)
      REAL(8)::FF(MX,MY,MZ),HH(MX,MY),HDEP(MX,MY)
C
      INTEGER,ALLOCATABLE::IBUF(:)
      REAL(8),ALLOCATABLE::GBUF(:)
C
C ... WORK VARIABLES
      INTEGER IERR,N,IRANK,ISIZE,ISTAT(MPI_STATUS_SIZE),IREQ,ITAG
C
      INTEGER::I,J,K,I0,I1,I2,J0,J1,J2,M,NJST1,NIST1,NSFT,IERR1,IERR2
      INTEGER::IS,JS,KS,IE,JE,KE,IDIR,N2,NN
C
C
C----------------------------------------------------------------------
C     (A) CADMASにSTOCで用いる地形データを渡す
C----------------------------------------------------------------------
C ... 通信バッファの割り当て
      ISIZE = MAX(NIST,NJST)*NKST*3
      ISIZE = MAX(ISIZE,1)
      ALLOCATE(IBUF(ISIZE),STAT=IERR)
      ISIZE = MAX(NIST,NJST)*NKST*6
      ISIZE = MAX(ISIZE,1)
      ALLOCATE(GBUF(ISIZE),STAT=IERR)
      IF(IERR.NE.0) THEN
        CALL ERRMSG('CADMAS_PORS',6840)
        WRITE(LP,*) 'ERROR: CANNOT ALLOCATE BUFFER'
        WRITE(LP,*) '       ROUTINE = CADMAS_INDX'
        CALL ABORT1('')
      END IF
C
C
C ... (1) 西側インデックスの送信
C
      IF( IWCAD.GT.0 ) THEN
      I0 = IWCAD-1
      I1 = IWCAD
      I2 = IWCAD+1
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(1,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M  = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            IBUF(M       ) = INDP(I0,J,K)
            IBUF(M+NSFT  ) = INDP(I1,J,K)
            IBUF(M+NSFT*2) = INDP(I2,J,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*3
         ITAG  = ITAGSC*8+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (2) 東側インデックスの送信
C
      IF( IECAD.GT.0 ) THEN
      I0 = IECAD+1
      I1 = IECAD
      I2 = IECAD-1
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(2,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            IBUF(M       ) = INDP(I0,J,K)
            IBUF(M+NSFT  ) = INDP(I1,J,K)
            IBUF(M+NSFT*2) = INDP(I2,J,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*3
         ITAG  = ITAGSC*9+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (3) 南側インデックスの送信
C
      IF( JSCAD.GT.0 ) THEN
      J0 = JSCAD-1
      J1 = JSCAD
      J2 = JSCAD+1
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(1,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            IBUF(M       ) = INDP(I,J0,K)
            IBUF(M+NSFT  ) = INDP(I,J1,K)
            IBUF(M+NSFT*2) = INDP(I,J2,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*3
         ITAG  = ITAGSC*10+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (4) 北側インデックスの送信
C
      IF( JNCAD.GT.0 ) THEN
      J0 = JNCAD+1
      J1 = JNCAD
      J2 = JNCAD-1
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(2,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            IBUF(M       ) = INDP(I,J0,K)
            IBUF(M+NSFT  ) = INDP(I,J1,K)
            IBUF(M+NSFT*2) = INDP(I,J2,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*3
         ITAG  = ITAGSC*11+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C OK
C
C ... (5) 西側ポーラス値の送信
C
      IF( IWCAD.GT.0 ) THEN
      I1 = IWCAD
      I2 = IWCAD+1
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(1,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M  = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            GBUF(M       ) = GV(I1,J,K)
            GBUF(M+NSFT  ) = GV(I2,J,K)
            GBUF(M+NSFT*2) = GX(I1-1,J,K)
            GBUF(M+NSFT*3) = GX(I2-1,J,K)
            GBUF(M+NSFT*4) = GY(I1,J-1,K)
            GBUF(M+NSFT*5) = GY(I2,J-1,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*6
         ITAG  = ITAGSC*12+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (6) 東側ポーラス値の送信
C
      IF( IECAD.GT.0 ) THEN
      I1 = IECAD
      I2 = IECAD-1
C
      DO N=1,NB_CADMAS
         NJST1 = JJCAD(2,N)
         IF( NJST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NJST1*NKST
         DO K=KBCAD,KTCAD
         DO J=JJCAD(3,N),JJCAD(4,N)
            M = M+1
            GBUF(M       ) = GV(I1,J,K)
            GBUF(M+NSFT  ) = GV(I2,J,K)
            GBUF(M+NSFT*2) = GX(I1,J,K)
            GBUF(M+NSFT*3) = GX(I2,J,K)
            GBUF(M+NSFT*4) = GY(I1,J-1,K)
            GBUF(M+NSFT*5) = GY(I2,J-1,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NJST1*NKST*6
         ITAG  = ITAGSC*13+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (7) 南側ポーラス値の送信
C
      IF( JSCAD.GT.0 ) THEN
      J1 = JSCAD
      J2 = JSCAD+1
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(1,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            GBUF(M       ) = GV(I,J1,K)
            GBUF(M+NSFT  ) = GV(I,J2,K)
            GBUF(M+NSFT*2) = GX(I-1,J1,K)
            GBUF(M+NSFT*3) = GX(I-1,J2,K)
            GBUF(M+NSFT*4) = GY(I,J1-1,K)
            GBUF(M+NSFT*5) = GY(I,J2-1,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*6
         ITAG  = ITAGSC*14+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C
C ... (8) 北側ポーラス値の送信
C
      IF( JNCAD.GT.0 ) THEN
      J1 = JNCAD
      J2 = JNCAD-1
C
      DO N=1,NB_CADMAS
         NIST1 = IICAD(2,N)
         IF( NIST1.EQ.0 ) CYCLE
C
         M = 0
         NSFT = NIST1*NKST
         DO K=KBCAD,KTCAD
         DO I=IICAD(3,N),IICAD(4,N)
            M = M+1
            GBUF(M       ) = GV(I,J1,K)
            GBUF(M+NSFT  ) = GV(I,J2,K)
            GBUF(M+NSFT*2) = GX(I-1,J1,K)
            GBUF(M+NSFT*3) = GX(I-1,J2,K)
            GBUF(M+NSFT*4) = GY(I,J1,K)
            GBUF(M+NSFT*5) = GY(I,J2,K)
         ENDDO
         ENDDO
C
         M     = LB_STOC
         IRANK = IB_CADMAS(N)
         ISIZE = NIST1*NKST*6
         ITAG  = ITAGSC*15+NB_CADMAS*(M-1)+N-1
         CALL MPI_ISEND(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTAT,IERR)
      ENDDO
      ENDIF
C
C ... 通信バッファの解放
      DEALLOCATE(GBUF)
      DEALLOCATE(IBUF)
C
C
C----------------------------------------------------------------------
C     (B) CADMAS領域のインデックス、ポロシティ、他変数の値を修正する
C----------------------------------------------------------------------
C
C ... (1) CADMAS領域のINDP,INDU,INDV,INDWを修正
      IS = IOBSS(1,1)
      IE = IOBSS(2,1)
      JS = IOBSS(3,1)
      JE = IOBSS(4,1)
      KS = IOBSS(5,1)
      KE = IOBSS(6,1)
C
      DO 100 K=KS,KE
      DO 100 J=JS,JE
      DO 100 I=IS,IE
         INDP(I,J,K) = 0
  100 CONTINUE
C
      DO 110 K=KS,KE
      DO 110 J=JS,JE
         DO 120 I=IS-1,IE
            INDU(I,J,K) = -4
  120    CONTINUE
         IF( INDP(IS-1,J,K).EQ.1 ) INDU(IS-1,J,K) = -1
         IF( INDP(IE+1,J,K).EQ.1 ) INDU(IE  ,J,K) = -1
  110 CONTINUE
C
      DO 130 K=KS,KE
      DO 130 I=IS,IE
         DO 140 J=JS-1,JE
            INDV(I,J,K) = -4
  140    CONTINUE
         IF( INDP(I,JS-1,K).EQ.1 ) INDV(I,JS-1,K) = -1
         IF( INDP(I,JE+1,K).EQ.1 ) INDV(I,JE  ,K) = -1
  130 CONTINUE
C
      DO 150 K=KS-1,KE
      DO 150 J=JS,JE
      DO 150 I=IS,IE
         INDW(I,J,K) = -4
  150 CONTINUE
C
C
C ... (2) CADMAS領域のLLWALL,MLWALLを修正
      M = MLWALL1
C
      DO 200 N=1,MLWALL1
         I = LLWALL(1,N)
         J = LLWALL(2,N)
         K = LLWALL(3,N)
         IDIR = LLWALL(4,N)
C
C ...... 法線方向がX方向の面
         IF( IDIR.EQ.0 .OR. IDIR.EQ.1 ) THEN
            IF( I.GE.IS-1.AND.I.LE.IE .AND.
     $          J.GE.JS  .AND.J.LE.JE .AND.
     $          K.GE.KS  .AND.K.LE.KE ) THEN
               LLWALL(4,N)=-1
               M = M-1
            ENDIF
C
C ...... 法線方向がY方向の面
         ELSE IF( IDIR.EQ.2 .OR. IDIR.EQ.3 ) THEN
            IF( I.GE.IS  .AND.I.LE.IE .AND.
     $          J.GE.JS-1.AND.J.LE.JE .AND.
     $          K.GE.KS  .AND.K.LE.KE ) THEN
               LLWALL(4,N)=-1
               M = M-1
            ENDIF
C
C ...... 法線方向がZ方向の面
         ELSE IF( IDIR.EQ.4 .OR. IDIR.EQ.5 ) THEN
            IF( I.GE.IS  .AND.I.LE.IE .AND.
     $          J.GE.JS  .AND.J.LE.JE .AND.
     $          K.GE.KS-1.AND.K.LE.KE ) THEN
               LLWALL(4,N)=-1
               M = M-1
            ENDIF
         ENDIF
  200 CONTINUE
C
      DO 210 N=1,M
         IF( LLWALL(4,N).EQ.-1 ) THEN
            DO 220 N2=N+1,MLWALL1
               IF( LLWALL(4,N2).GE.0 ) GOTO 230
  220       CONTINUE
  230       CONTINUE
C
            DO 240 NN=1,8
               LLWALL(NN,N) = LLWALL(NN,N2)
  240       CONTINUE
            LLWALL(4,N2) = -1
         ENDIF
  210 CONTINUE
C
      write(*,*) 'mlwall1(before mod)=',MLWALL1
      MLWALL1 = M
      write(*,*) 'mlwall1(mod)=',MLWALL1
C
C ... (3) CADMAS領域のGV,GX,GY,GZを修正
      DO 300 K=KS,KE
      DO 300 J=JS,JE
      DO 300 I=IS,IE
         GV(I,J,K) = 1.0D0
  300 CONTINUE
C
      DO 310 K=KS,KE
      DO 310 J=JS,JE
         DO 320 I=IS,IE-1
            GX(I,J,K) = 1.0D0
  320    CONTINUE
         IF( INDP(IS-1,J,K).EQ.0 ) GX(IS-1,J,K) = 1.0D0
         IF( INDP(IE+1,J,K).EQ.0 ) GX(IE  ,J,K) = 1.0D0
  310 CONTINUE
C
      DO 330 K=KS,KE
      DO 330 I=IS,IE
         DO 340 J=JS,JE-1
            GY(I,J,K) = 1.0D0
  340    CONTINUE
         IF( INDP(I,JS-1,K).EQ.0 ) GY(I,JS-1,K) = 1.0D0
         IF( INDP(I,JE+1,K).EQ.0 ) GY(I,JE  ,K) = 1.0D0
  330 CONTINUE
C
      DO 350 K=KS-1,KE
      DO 350 J=JS,JE
      DO 350 I=IS,IE
         GZ(I,J,K) = 1.0D0
  350 CONTINUE
C
C ... (4) CADMAS領域のKF,KG,KP,HH,HDEP,FFを修正
      DO 400 J=JS,JE
      DO 400 I=IS,IE
         KF(I,J) = MZ
         KG(I,J) = MZ
         KP(I,J) = MZ
         HH(I,J) = ZC(1,MZM)
         HDEP(I,J) = ZC(1,MZM)
  400 CONTINUE
C
      DO 410 K=KS,KE
      DO 410 J=JS,JE
      DO 410 I=IS,IE
         FF(I,J,K) = 1.0D0
  410 CONTINUE
C
      RETURN
      END
