      SUBROUTINE ABORT1(CROUT)
C======================================================================
C     異常時における終了処理ルーチン
C======================================================================
      use mod_comm,only: comm_model
      IMPLICIT NONE
C
      INCLUDE 'mpif.h'
      INCLUDE 'FILE.h'
C
      CHARACTER(*),INTENT(IN)::CROUT
      INTEGER :: IERR,ICODE
C
      ICODE=1
      WRITE(LP,*) CROUT
      CALL MPI_ABORT( MPI_COMM_WORLD,ICODE,IERR )
C
      RETURN
      END
