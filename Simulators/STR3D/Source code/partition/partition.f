      SUBROUTINE PARTITION(KK,RR,IFL,ICK)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER*8 MUSED

      DIMENSION KK(*),RR(*),IFL(*),ICK(*)

      IF( MYRANK == 0 ) THEN
        CALL PART(KK,IFL,ICK(4))
        IF( KK(92) > 0 ) CALL PART_C(KK,IFL(11))
      ENDIF

      CALL BCAST_TBL(KK,RR,ICK)

      IF( MYRANK == 0 ) THEN
        DO IP = 1, NPROCS - 1
          CALL SEND_TBL(KK,IP)
        ENDDO
      ELSE
        CALL RECV_TBL(KK)
        IF( ICK(1) == 1 ) CALL DATA_CHECK(KK,RR)
      ENDIF

      IF( MYRANK == 0 ) THEN
        CALL DEALLOC(KK,IFL(11),ICK(3))
        WRITE(IFL(11),'(/X,A,F8.1,A)')
     &    'SUB. PARTITION  : ', MUSED()*4.D-6, '(MB) USED.'
      ENDIF

      END
