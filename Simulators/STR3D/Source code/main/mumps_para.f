      SUBROUTINE MUMPS_PARA()

      USE M_MUMPS
      USE MPI_PARAM

      MUMPS_PAR%COMM = MYWORLD

      DO

        CALL M_MPI_BCAST_I(MUMPS_PAR%JOB,1)

        IF( MUMPS_PAR%JOB == 0 ) RETURN

        CALL DMUMPS(MUMPS_PAR)

        IF( MUMPS_PAR%JOB == -1 ) THEN
          MUMPS_PAR%ICNTL(1) = 0
          MUMPS_PAR%ICNTL(2) = 0
          MUMPS_PAR%ICNTL(4) = 1
        ENDIF

      ENDDO

      END
