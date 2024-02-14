      SUBROUTINE ERRSTP(NO,ITO)

      INCLUDE 'mpif.h'

      WRITE(ITO,'(A)') '*** ERROR NO. ***'
      WRITE(ITO,'(I3)') NO

      CALL MPI_ABORT(MPI_COMM_WORLD,0,IERR)

      END
