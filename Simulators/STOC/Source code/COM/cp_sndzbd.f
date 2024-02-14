      SUBROUTINE CP_SNDZBD(ZBED_ML,BUF,MX_ML,MY_ML,
     1                     IEAS,IWES,JSOU,JNOR,
     2                     NESXM,NESXP,NESYM,NESYP,ICHILD)
C-----------------------------------------------------------------------
C     親のZBEDを子に転送する
C-----------------------------------------------------------------------
C
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'CONNEC.h'
      INCLUDE 'GLOBAL.h'
C
      INTEGER,INTENT(INOUT)::MX_ML,MY_ML
      INTEGER,INTENT(INOUT)::IEAS,IWES,JSOU,JNOR
      INTEGER,INTENT(INOUT)::NESXM,NESXP,NESYM,NESYP
      INTEGER,INTENT(INOUT)::ICHILD
C
      REAL(8),INTENT(INOUT)::ZBED_ML(MX_ML,MY_ML)
      REAL(8),INTENT(INOUT)::BUF(*)
C
      INTEGER::ISTAT(MPI_STATUS_SIZE)
C
      INTEGER::I,IERROR,IREQ,ITAG,J,NCOUNT,N,NS,NE,ICHILN
C
      NS = NUMPE(2,NRANK+1)
      NE = NS+NUMPE(1,NRANK+1)-1
      ITAG=0
C
C ... 子にZBED_MLを送信する。
C
      NCOUNT=0
      DO 100 J=JSOU-1,JNOR+1
      DO 100 I=IWES-1,IWES+NESXM
      NCOUNT=NCOUNT+1
      BUF(NCOUNT)=ZBED_ML(I,J)
  100 CONTINUE
C
      DO 110 J=JSOU-1,JNOR+1
      DO 110 I=IEAS-NESXP,IEAS+1
      NCOUNT=NCOUNT+1
      BUF(NCOUNT)=ZBED_ML(I,J)
  110 CONTINUE
C
      DO 120 J=JSOU-1,JSOU+NESYM
      DO 120 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      BUF(NCOUNT)=ZBED_ML(I,J)
  120 CONTINUE
C
      DO 130 J=JNOR-NESYP,JNOR+1
      DO 130 I=IWES+NESXM+1,IEAS-NESXP-1
      NCOUNT=NCOUNT+1
      BUF(NCOUNT)=ZBED_ML(I,J)
  130 CONTINUE
C
      DO 140 N=NS,NE
      ICHILN = NUMCOM(1,N)
      write(6,*) 'snddep nrank,ichiln=',nrank,ichiln
      CALL MPI_ISEND(BUF,NCOUNT,MPI_DOUBLE_PRECISION,ICHILN,
     *               ITAG,comm_model,IREQ,IERROR)
      CALL MPI_WAIT(IREQ,ISTAT,IERROR)
  140 CONTINUE
C
      RETURN
      END
