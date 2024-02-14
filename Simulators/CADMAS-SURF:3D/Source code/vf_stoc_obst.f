      SUBROUTINE VF_STOC_OBST(NF,JBUF)

CD=== 概要 ===========================================================

CDT   VF_STOC_OBST:STOCから地形データを受信する(OBST)

C==== 宣言 ===========================================================

      use mod_comm,only: comm_ic_mg
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'VF_ASTOCI.h'
      INCLUDE 'VF_ASTOCR.h'
      INCLUDE 'mpif.h'

CD    -- 引数 --
CD    NF(@FOR-3D@) : I/O : I*4 : セルの状態を示すインデックス
CD    JBUF(NUMBUF*MAXBUF) : OUT :I*4 : 並列用のバッファ
      DIMENSION NF(NUMI,NUMJ,NUMK),JBUF(NUMBUF*MAXBUF)

      INTEGER,  DIMENSION(:), ALLOCATABLE :: IBUF
      DIMENSION ISTT(MPI_STATUS_SIZE)

      INTEGER,PARAMETER:: LMODDEP=0 ! 0: NOT MODIFY, 1:MODIFY

C==== 実行 ===========================================================
C
C ... 通信バッファの割り当て
      ISIZE = MAX(NIST,NJST)*NKST*3
      ISIZE = MAX(ISIZE,1)
      ALLOCATE(IBUF(ISIZE),STAT=IERR)
      IF(IERR.NE.0) THEN
        WRITE(*,*) 'ERROR: CANNOT ALLOCATE BUFFER'
        WRITE(*,*) '       ROUTINE = VF_STOC_OBST'
        CALL MPI_ABORT(MPI_COMM_WORLD,IERR1,IERR2)
      END IF
C
C
C ... (1) 西側境界インデックス値の受信
C
CDEBUG      write(*,*) 'vf_stoc_area: iwst,iest,jsst,jnst=',
CDEBUG     $   iwst,iest,jsst,jnst
      IF( IWST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(1,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*3
         ITAG = ITAGSC*8+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NJCAD1*NKST
         NP=0
         DO 150 KST=1,NKST
         DO 140 JST=JJST(3,M),JJST(4,M)
            NP = NP+1
            NF1 = IBUF(0*NSFT+NP)-1
            NF2 = IBUF(1*NSFT+NP)-1
            NF3 = IBUF(2*NSFT+NP)-1
C
            DO 130 K=MKST(KST),MKST(KST+1)-1
               DO 120 J=MJST(JST),MJST(JST+1)-1
                 I=1
C@               STOC側が構造物の場合の処理？？
                 DO 100 I=MIST(1),MIST(2)-1
                   NF(I,J,K)=NF2
  100            CONTINUE
                 DO 110 I=MIST(2),MIST(3)-1
                   NF(I,J,K)=NF3
  110            CONTINUE
  120         CONTINUE
  130       CONTINUE
  140    CONTINUE
  150    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (2) 東側境界インデックス値の受信
C
      IF( IEST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(2,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*3
         ITAG = ITAGSC*9+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NJCAD1*NKST
         NP=0
         DO 250 KST=1,NKST
         DO 240 JST=JJST(3,M),JJST(4,M)
            NP = NP+1
            NF1 = IBUF(0*NSFT+NP)-1
            NF2 = IBUF(1*NSFT+NP)-1
            NF3 = IBUF(2*NSFT+NP)-1
C
            DO 230 K=MKST(KST),MKST(KST+1)-1
               DO 220 J=MJST(JST),MJST(JST+1)-1
                 I=NUMI
C@               STOC側が構造物の場合の処理？？
                 DO 200 I=MIST(NIST),MIST(NIST+1)-1
                   NF(I,J,K)=NF2
  200            CONTINUE
                 DO 210 I=MIST(NIST-1),MIST(NIST)-1
                   NF(I,J,K)=NF3
  210            CONTINUE
  220         CONTINUE
  230       CONTINUE
  240    CONTINUE
  250    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (3) 南側境界インデックス値の受信
C
      IF( JSST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(1,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*3
         ITAG = ITAGSC*10+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NICAD1*NKST
         NP=0
         DO 350 KST=1,NKST
         DO 340 IST=IIST(3,M),IIST(4,M)
            NP = NP+1
            NF1 = IBUF(0*NSFT+NP)-1
            NF2 = IBUF(1*NSFT+NP)-1
            NF3 = IBUF(2*NSFT+NP)-1
C
            DO 330 K=MKST(KST),MKST(KST+1)-1
               DO 320 I=MIST(IST),MIST(IST+1)-1
                 J=1
C@               STOC側が構造物の場合の処理？？
                 DO 300 J=MJST(1),MJST(2)-1
                   NF(I,J,K)=NF2
  300            CONTINUE
                 DO 310 J=MJST(2),MJST(3)-1
                   NF(I,J,K)=NF3
  310            CONTINUE
  320         CONTINUE
  330       CONTINUE
  340    CONTINUE
  350    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
C ... (4) 北側境界インデックス値の受信
C
      IF( JNST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(2,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*3
         ITAG = ITAGSC*11+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(IBUF,ISIZE,MPI_INTEGER,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         IF( LMODDEP.EQ.1 ) THEN
         NSFT=NICAD1*NKST
         NP=0
         DO 450 KST=1,NKST
         DO 440 IST=IIST(3,M),IIST(4,M)
            NP = NP+1
            NF1 = IBUF(0*NSFT+NP)-1
            NF2 = IBUF(1*NSFT+NP)-1
            NF3 = IBUF(2*NSFT+NP)-1
C
            DO 430 K=MKST(KST),MKST(KST+1)-1
               DO 420 I=MIST(IST),MIST(IST+1)-1
                 J=NUMJ
C@               STOC側が構造物の場合の処理？？
                 DO 400 J=MJST(NJST),MJST(NJST+1)-1
                   NF(I,J,K)=NF2
  400            CONTINUE
                 DO 410 J=MJST(NJST-1),MJST(NJST)-1
                   NF(I,J,K)=NF3
  410            CONTINUE
  420         CONTINUE
  430       CONTINUE
  440    CONTINUE
  450    CONTINUE
         ENDIF
         ENDDO
      ENDIF
C
      IF( LMODDEP.EQ.1 ) THEN
      CALL VF_P3SRI2(NF,JBUF,0)
      ENDIF
C
C ... 通信バッファの解放
      DEALLOCATE(IBUF)
C
      RETURN
      END
