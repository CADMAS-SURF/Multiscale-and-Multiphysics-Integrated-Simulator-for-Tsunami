      SUBROUTINE VF_STOC_RECV(STBUF,NBUF)

CD=== 概要 ===========================================================

CDT   VF_STOC_RECV:STOCから境界値を受信する

C==== 宣言 ===========================================================

      use mod_comm,only: comm_ic_mg
C     -- 大域型 --
      IMPLICIT INTEGER(I-N),DOUBLE PRECISION(A-H,O-Z)

C     -- 大域変数 --
      INCLUDE 'mpif.h'
      include 'VF_ASTOCI.h'
      include 'VF_ASTOCR.h'

CD    -- 引数 --
      DIMENSION STBUF(NBUF)
      DIMENSION ISTT(MPI_STATUS_SIZE)

C==== 実行 ===========================================================
C
C ... (1) 時刻の送受信(デバッグ用)
C
      N = LB_CADMAS
      IRANK = IB_STOC(1)
      ISIZE = 1
      M     = 1
      ITAG = ITAGSC*16+NB_CADMAS*(M-1)+N-1
      CALL MPI_IRECV(T1,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $               ITAG,comm_ic_mg,IREQ,IERR)
      CALL MPI_WAIT(IREQ,ISTT,IERR)
C      WRITE(*,*) 'TIME=',T1,TNOW
C
C ... (2) 西側境界値の受信
C
      IF( IWST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(1,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*4
         ITAG = ITAGSC*17+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                 ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         NSFT=NJCAD1*NKST
         NP=0
         DO 110 K=1,NKST
         DO 100 J=JJST(3,M),JJST(4,M)
            NP = NP+1
            UWST(J,K,1) = STBUF(0*NSFT+NP)
            UWST(J,K,2) = STBUF(1*NSFT+NP)
            UWST(J,K,3) = STBUF(2*NSFT+NP)
            UWST(J,K,4) = STBUF(3*NSFT+NP)
  100    CONTINUE
  110    CONTINUE
         ENDDO
      ENDIF
C
C ... (3) 東側境界値の受信
C
      IF( IEST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NJCAD1 = JJST(2,M)
         IF( NJCAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NJCAD1*NKST*4
         ITAG = ITAGSC*18+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         NSFT=NJCAD1*NKST
         NP=0
         DO 130 K=1,NKST
         DO 120 J=JJST(3,M),JJST(4,M)
            NP = NP+1
            UEST(J,K,1) = STBUF(0*NSFT+NP)
            UEST(J,K,2) = STBUF(1*NSFT+NP)
            UEST(J,K,3) = STBUF(2*NSFT+NP)
            UEST(J,K,4) = STBUF(3*NSFT+NP)
  120    CONTINUE
  130    CONTINUE
         ENDDO
      ENDIF
C
C ... (4) 南側境界値の受信
C
      IF( JSST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(1,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*4
         ITAG = ITAGSC*19+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         NSFT=NICAD1*NKST
         NP=0
         DO 150 K=1,NKST
         DO 140 I=IIST(3,M),IIST(4,M)
            NP = NP+1
            VSST(I,K,1) = STBUF(0*NSFT+NP)
            VSST(I,K,2) = STBUF(1*NSFT+NP)
            VSST(I,K,3) = STBUF(2*NSFT+NP)
            VSST(I,K,4) = STBUF(3*NSFT+NP)
  140    CONTINUE
  150    CONTINUE
         ENDDO
      ENDIF
C
C ... (5) 北側境界値の受信
C
      IF( JNST.EQ.1 ) THEN
         DO M=1,NB_STOC
         NICAD1 = IIST(2,M)
         IF( NICAD1.EQ.0 ) CYCLE
C
         N = LB_CADMAS
         IRANK = IB_STOC(M)
         ISIZE = NICAD1*NKST*4
         ITAG = ITAGSC*20+NB_CADMAS*(M-1)+N-1
         CALL MPI_IRECV(STBUF,ISIZE,MPI_DOUBLE_PRECISION,IRANK,
     $                  ITAG,comm_ic_mg,IREQ,IERR)
         CALL MPI_WAIT(IREQ,ISTT,IERR)
C
         NSFT=NICAD1*NKST
         NP=0
         DO 170 K=1,NKST
         DO 160 I=IIST(3,M),IIST(4,M)
            NP = NP+1
            VNST(I,K,1) = STBUF(0*NSFT+NP)
            VNST(I,K,2) = STBUF(1*NSFT+NP)
            VNST(I,K,3) = STBUF(2*NSFT+NP)
            VNST(I,K,4) = STBUF(3*NSFT+NP)
  160    CONTINUE
  170    CONTINUE
         ENDDO
      ENDIF
C
      RETURN
      END
