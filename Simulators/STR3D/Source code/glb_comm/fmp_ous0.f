      SUBROUTINE FMP_OUS0(IOUT,IFL)

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/

      CALL M_MPI_RECV_I(IOUT,1,1)

      CALL M_MPI_RECV_I(ISTEP,1,1)

      CALL M_MPI_RECV_D(TIME,1,1)

      WRITE(IFL,'(I0,A)') IOUT,C

      WRITE(IFL,'(A,I0,A,1PE10.4)') 'Step ',ISTEP,'  Time ',TIME

      WRITE(IFL,'(A)') '0,23,'

      WRITE(IFL,'(1PE10.4,A)') TIME,C

      WRITE(IFL,'(A)') '1,'

      WRITE(IFL,'(A)') '<NULL>'

      END
