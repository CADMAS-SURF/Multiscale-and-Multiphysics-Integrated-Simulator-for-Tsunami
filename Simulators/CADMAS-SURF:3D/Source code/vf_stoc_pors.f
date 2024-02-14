      SUBROUTINE VF_STOC_PORS(GGV,GGX,GGY,GGZ,NF,INDX,INDY,INDZ,DBUF)

CD=== 概要 ===========================================================

CDT   VF_STOC_PORS:STOCから地形データを受信する(GGV,GGX,GGY,GGZ)

C==== 宣言 ===========================================================

C     -- 大域型 --
      use mod_comm,only: comm_ic_mg
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    GGV(@FOR-3D@)    : I/O : R*8 : 空隙率
CD    GGX(@FOR-3D@)    : I/O : R*8 : x方向面積透過率
CD    GGY(@FOR-3D@)    : I/O : R*8 : y方向面積透過率
CD    GGZ(@FOR-3D@)    : I/O : R*8 : z方向面積透過率
CD    NF(@FOR-3D@)     : IN  : I*4 : セルの状態を示すインデックス
CD    INDX(@FOR-3D@)   : IN  : I*4 : x面の状態を示すインデックス
CD    INDY(@FOR-3D@)   : IN  : I*4 : y面の状態を示すインデックス
CD    INDZ(@FOR-3D@)   : IN  : I*4 : z面の状態を示すインデックス
CD    DBUF(NUMBUF*MAXBUF) : OUT :R*8 : 並列用のバッファ
      DIMENSION GGV(NUMI,NUMJ,NUMK),GGX(NUMI,NUMJ,NUMK)
      DIMENSION GGY(NUMI,NUMJ,NUMK),GGZ(NUMI,NUMJ,NUMK)
      DIMENSION NF  (NUMI,NUMJ,NUMK),INDX(NUMI,NUMJ,NUMK)
      DIMENSION INDY(NUMI,NUMJ,NUMK),INDZ(NUMI,NUMJ,NUMK)
      DIMENSION DBUF(NUMBUF*MAXBUF)

      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: GBUF
      DIMENSION ISTT(MPI_STATUS_SIZE)

      INTEGER,PARAMETER:: LMODDEP=0 ! 0: NOT MODIFY, 1:MODIFY

C==== 実行 ===========================================================
C
C ... 通信バッファの割り当て
      ISIZE = MAX(NIST,NJST)*NKST*6
      ISIZE = MAX(ISIZE,1)
      ALLOCATE(GBUF(ISIZE),STAT=IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,*) 'ERROR: CANNOT ALLOCATE BUFFER'
        WRITE(*,*) '       ROUTINE = VF_STOC_PORS'
        CALL MPI_ABORT(MPI_COMM_WORLD,IERR1,IERR2)
      END IF
C
C OK
C
C ... (5) 西側境界ポーラス値の受信
C
      IF( IWST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(1,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*6
         ITAG = ITAGSC*12+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NJCAD1*NKST
         NP=0
         DO 230 KST=1,NKST
         DO 220 JST=JJST(3,M),JJST(4,M)
            NP = NP+1
            GGV1 = GBUF(0*NSFT+NP)
            GGV2 = GBUF(1*NSFT+NP)
            GGX1 = GBUF(2*NSFT+NP)
            GGX2 = GBUF(3*NSFT+NP)
            GGY1 = GBUF(4*NSFT+NP)
            GGY2 = GBUF(5*NSFT+NP)
C
            DO 210 K=MKST(KST),MKST(KST+1)-1
               DO 200 J=MJST(JST),MJST(JST+1)-1
                 DO 100 I=MIST(1),MIST(2)-1
                   IF (NF  (I,J,K).NE.-1) GGV(I,J,K)=GGV1
                   IF (INDX(I,J,K).NE.-1) GGX(I,J,K)=GGX1
                   IF (INDY(I,J,K).NE.-1) GGY(I,J,K)=GGY1
  100            CONTINUE
                 DO 110 I=MIST(2),MIST(3)-1
                   IF (NF  (I,J,K).NE.-1) GGV(I,J,K)=GGV2
                   IF (INDX(I,J,K).NE.-1) GGX(I,J,K)=GGX2
                   IF (INDY(I,J,K).NE.-1) GGY(I,J,K)=GGY2
  110            CONTINUE
  200          CONTINUE
  210       CONTINUE
  220    CONTINUE
  230    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (6) 東側境界ポーラス値の受信
C
      IF( IEST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(2,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*6
         ITAG = ITAGSC*13+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NJCAD1*NKST
         NP=0
         DO 430 KST=1,NKST
         DO 420 JST=JJST(3,M),JJST(4,M)
            NP = NP+1
            GGV1 = GBUF(0*NSFT+NP)
            GGV2 = GBUF(1*NSFT+NP)
            GGX1 = GBUF(2*NSFT+NP)
            GGX2 = GBUF(3*NSFT+NP)
            GGY1 = GBUF(4*NSFT+NP)
            GGY2 = GBUF(5*NSFT+NP)
C
            DO 410 K=MKST(KST),MKST(KST+1)-1
               DO 400 J=MJST(JST),MJST(JST+1)-1
                 DO 300 I=MIST(NIST),MIST(NIST+1)-1
                   IF (NF  (I  ,J,K).NE.-1) GGV(I  ,J,K)=GGV1
                   IF (INDX(I+1,J,K).NE.-1) GGX(I+1,J,K)=GGX1
                   IF (INDY(I  ,J,K).NE.-1) GGY(I  ,J,K)=GGY1
  300            CONTINUE
                 DO 310 I=MIST(NIST-1),MIST(NIST)-1
                   IF (NF  (I  ,J,K).NE.-1) GGV(I  ,J,K)=GGV2
                   IF (INDX(I+1,J,K).NE.-1) GGX(I+1,J,K)=GGX2
                   IF (INDY(I  ,J,K).NE.-1) GGY(I  ,J,K)=GGY2
  310            CONTINUE
  400          CONTINUE
  410       CONTINUE
  420    CONTINUE
  430    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (7) 南側境界ポーラス値の受信
C
      IF( JSST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(1,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*6
         ITAG = ITAGSC*14+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NICAD1*NKST
         NP=0
         DO 630 KST=1,NKST
         DO 620 IST=IIST(3,M),IIST(4,M)
            NP = NP+1
            GGV1 = GBUF(0*NSFT+NP)
            GGV2 = GBUF(1*NSFT+NP)
            GGX1 = GBUF(2*NSFT+NP)
            GGX2 = GBUF(3*NSFT+NP)
            GGY1 = GBUF(4*NSFT+NP)
            GGY2 = GBUF(5*NSFT+NP)
C
            DO 610 K=MKST(KST),MKST(KST+1)-1
               DO 600 I=MIST(IST),MIST(IST+1)-1
                 DO 500 J=MJST(1),MJST(2)-1
                   IF (NF  (I,J,K).NE.-1) GGV(I,J,K)=GGV1
                   IF (INDX(I,J,K).NE.-1) GGX(I,J,K)=GGX1
                   IF (INDY(I,J,K).NE.-1) GGY(I,J,K)=GGY1
  500            CONTINUE
                 DO 510 J=MJST(2),MJST(3)-1
                   IF (NF  (I,J,K).NE.-1) GGV(I,J,K)=GGV2
                   IF (INDX(I,J,K).NE.-1) GGX(I,J,K)=GGX2
                   IF (INDY(I,J,K).NE.-1) GGY(I,J,K)=GGY2
  510            CONTINUE
  600          CONTINUE
  610       CONTINUE
  620    CONTINUE
  630    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (8) 北側境界ポーラス値の受信
C
      IF( JNST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(2,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*6
         ITAG = ITAGSC*15+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(GBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NICAD1*NKST
         NP=0
         DO 830 KST=1,NKST
         DO 820 IST=IIST(3,M),IIST(4,M)
            NP = NP+1
            GGV1 = GBUF(0*NSFT+NP)
            GGV2 = GBUF(1*NSFT+NP)
            GGX1 = GBUF(2*NSFT+NP)
            GGX2 = GBUF(3*NSFT+NP)
            GGY1 = GBUF(4*NSFT+NP)
            GGY2 = GBUF(5*NSFT+NP)
C
            DO 810 K=MKST(KST),MKST(KST+1)-1
               DO 800 I=MIST(IST),MIST(IST+1)-1
                 DO 700 J=MJST(NJST),MJST(NJST+1)-1
                   IF (NF  (I,J  ,K).NE.-1) GGV(I,J  ,K)=GGV1
                   IF (INDX(I,J  ,K).NE.-1) GGX(I,J  ,K)=GGX1
                   IF (INDY(I,J+1,K).NE.-1) GGY(I,J+1,K)=GGY1
  700            CONTINUE
                 DO 710 J=MJST(NJST-1),MJST(NJST)-1
                   IF (NF  (I,J  ,K).NE.-1) GGV(I,J  ,K)=GGV2
                   IF (INDX(I,J  ,K).NE.-1) GGX(I,J  ,K)=GGX2
                   IF (INDY(I,J+1,K).NE.-1) GGY(I,J+1,K)=GGY2
  710            CONTINUE
  800          CONTINUE
  810       CONTINUE
  820    CONTINUE
  830    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
      IF( LMODDEP.EQ.1 ) THEN
      CALL VF_P3SRD2(GGV,DBUF,0)
      CALL VF_P3SRD2(GGX,DBUF,1)
      CALL VF_P3SRD2(GGY,DBUF,2)
      CALL VF_P3SRD2(GGZ,DBUF,3)
      ENDIF
C
C ... 通信バッファの解放
      DEALLOCATE(GBUF)
C
      RETURN
      END
