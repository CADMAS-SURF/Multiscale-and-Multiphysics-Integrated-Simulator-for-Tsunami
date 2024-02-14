      SUBROUTINE NPFLOW0(ITO)

      IMPLICIT REAL*8(A-H,O-Z)

      DUM = 0.
      CALL M_MPI_REDUCE_D(DUM,RNRM,1,0)
      DUM = 0.
      CALL M_MPI_REDUCE_D(DUM,DFI,1,0)
      DUM = 0.
      CALL M_MPI_REDUCE_D(DUM,DFO,1,0)

      RNRM = DSQRT( RNRM )

      WRITE(ITO,'(/A//A)') 
     &  '   * FLOW BALANCE',
     &  '        RNRM       INFLOW      OUTFLOW    IN - OUT'

      WRITE(ITO,'(3X,1P4E12.4)') RNRM,DFI,DFO,DFI-DFO

      END
