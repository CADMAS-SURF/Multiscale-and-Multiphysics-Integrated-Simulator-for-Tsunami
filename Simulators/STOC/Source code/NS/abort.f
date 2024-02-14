      SUBROUTINE ABORT1(CROUT)
C======================================================================
C     異常時における終了処理ルーチン
C======================================================================
ccc      use mod_comm,only: comm_model
      IMPLICIT NONE
C
ccc      INCLUDE 'mpif.h'
      INCLUDE 'FILE.h'
C
      CHARACTER(*),INTENT(IN)::CROUT
C      INTEGER :: IERR,ICODE
C
C      ICODE=1
      WRITE(LP,*) CROUT
      WRITE(LP,*) '### error stop'
ccc      CALL MPI_ABORT( MPI_COMM_WORLD,ICODE,IERR )
      stop
C
      RETURN
      END
