      SUBROUTINE FMP_OUS(IOUT,ISTEP,TIME,IFL)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/

      IF( MYRANK == 0 ) THEN

        WRITE(IFL,'(I0,A)') IOUT,C

        WRITE(IFL,'(A,I0,A,1PE10.4)') 'Step ',ISTEP,'  Time ',TIME

        WRITE(IFL,'(A)') '0,23,'

        WRITE(IFL,'(1PE10.4,A)') TIME,C

        WRITE(IFL,'(A)') '1,'

        WRITE(IFL,'(A)') '<NULL>'

      ELSEIF( MYRANK == 1 ) THEN

        CALL M_MPI_SEND_I(IOUT,1,0)

        CALL M_MPI_SEND_I(ISTEP,1,0)

        CALL M_MPI_SEND_D(TIME,1,0)

      ENDIF

      END
