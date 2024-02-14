      SUBROUTINE SF_COMM_INIT()

      USE MOD_COMM, ONLY: COMM_WORK_2FC_STR, COMM_2FC_STR

      IMPLICIT REAL*8(A-H,O-Z)

      LOGICAL IEX

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      COMM_2FC_STR = COMM_WORK_2FC_STR

      ICPL = 0

      CALL SF_C_MPI_COMM_SIZE(MPROCS)

      IF( MPROCS > NPROCS ) THEN

        ICPL = 2

!        IF( MYRANK == 0 ) THEN
!          CALL SF_C_MPI_COMM_RANK(IRANK)
!        ELSE
!          IRANK = 0
!        ENDIF
!
!        CALL SF_C_MPI_ALLREDUCE_I(IRANK,IROOTC,1)
!
!        IRANK = 0
!
!        CALL SF_C_MPI_ALLREDUCE_I(IRANK,IROOTS,1)

        CALL SF_C_MPI_COMM_RANK(IRANK)

        IF( MYRANK == 0 ) THEN
          JRANK = IRANK
        ELSE
          JRANK = 0
        ENDIF

        IF( IRANK == 0 ) THEN
          IROOTC = JRANK
          DO I = 1, MPROCS - 1
            CALL SF_C_MPI_RECV_I(JRANK,1,I)
            IROOTC = IROOTC + JRANK
          ENDDO
          DO I = 1, MPROCS - 1
            CALL SF_C_MPI_SEND_I(IROOTC,1,I)
          ENDDO
        ELSE
          CALL SF_C_MPI_SEND_I(JRANK,1,0)
          CALL SF_C_MPI_RECV_I(IROOTC,1,0)
        ENDIF

        JRANK = 0

        IF( IRANK == 0 ) THEN
          IROOTS = JRANK
          DO I = 1, MPROCS - 1
            CALL SF_C_MPI_RECV_I(JRANK,1,I)
            IROOTS = IROOTS + JRANK
          ENDDO
          DO I = 1, MPROCS - 1
            CALL SF_C_MPI_SEND_I(IROOTS,1,I)
          ENDDO
        ELSE
          CALL SF_C_MPI_SEND_I(JRANK,1,0)
          CALL SF_C_MPI_RECV_I(IROOTS,1,0)
        ENDIF

      ENDIF

      IF( MYRANK == 0 .AND. ICPL == 0 ) THEN
        INQUIRE( FILE = 'data.bdf', EXIST = IEX )
        IF( IEX ) ICPL = 1
      ENDIF

      CALL VF_P1BCSI(ICPL,1,0)

      IPART = 0

      IF( ICPL == 2 ) THEN
        PLOWER2 = 1.D-1
        GMIN = 1.D-1
        FMIN = 1.D-1
      ELSE
        PLOWER2 = 0.D0
        GMIN = 0.D0
        FMIN = 0.D0
      ENDIF

      END
