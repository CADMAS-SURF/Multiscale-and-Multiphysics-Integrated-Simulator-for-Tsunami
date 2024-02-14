      SUBROUTINE SF_STM_COMM_INIT()

      USE MOD_COMM, ONLY: COMM_WORK_2FC_STM, COMM_2FC_STM

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_APARAI.h'
      INCLUDE 'SF_STRUCT.h'

      COMM_2FC_STM = COMM_WORK_2FC_STM

      ISTM = 0

      CALL SF_STM_C_MPI_COMM_SIZE(MPROCS)

      IF( MPROCS > NPROCS ) THEN

        ISTM = 1

        IF( ICPL == 1 )
     &    CALL VF_A2ERR('SF_STM_COMM_INIT','STR PROCESS NOT FOUND.')

        CALL SF_STM_C_MPI_COMM_RANK(IRANK)

        IF( MYRANK == 0 ) THEN
          JRANK = IRANK
        ELSE
          JRANK = 0
        ENDIF

        IF( IRANK == 0 ) THEN
          IROOTC = JRANK
          DO I = 1, MPROCS - 1
            CALL SF_STM_C_MPI_RECV_I(JRANK,1,I)
            IROOTC = IROOTC + JRANK
          ENDDO
          DO I = 1, MPROCS - 1
            CALL SF_STM_C_MPI_SEND_I(IROOTC,1,I)
          ENDDO
        ELSE
          CALL SF_STM_C_MPI_SEND_I(JRANK,1,0)
          CALL SF_STM_C_MPI_RECV_I(IROOTC,1,0)
        ENDIF

        JRANK = 0

        IF( IRANK == 0 ) THEN
          IROOTSTM = JRANK
          DO I = 1, MPROCS - 1
            CALL SF_STM_C_MPI_RECV_I(JRANK,1,I)
            IROOTSTM = IROOTSTM + JRANK
          ENDDO
          DO I = 1, MPROCS - 1
            CALL SF_STM_C_MPI_SEND_I(IROOTSTM,1,I)
          ENDDO
        ELSE
          CALL SF_STM_C_MPI_SEND_I(JRANK,1,0)
          CALL SF_STM_C_MPI_RECV_I(IROOTSTM,1,0)
        ENDIF

      ENDIF

      CALL VF_P1BCSI(ISTM,1,0)
   
      IF( ICPL == 2 .AND. MYRANK == 0 )
     &  CALL SF_C_MPI_SEND_I(ISTM,1,IROOTS)

      END
