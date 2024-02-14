      SUBROUTINE RESFRC0(RR,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RR(*),FNRM(3),RNRM(3),DUM(3)

      CALL M_MPI_RECV_I(ISTEP,1,1)
      CALL M_MPI_RECV_D(TIM,1,1)
      CALL M_MPI_RECV_I(ITER2,1,1)
      CALL M_MPI_RECV_I(ITER,1,1)

      DUM(:) = 0.
      CALL M_MPI_REDUCE_D(DUM,FNRM,3,0)

      FNRM(1) = DSQRT( FNRM(1) )
      FNRM(2) = DSQRT( FNRM(2) )
      FNRM(3) = DSQRT( FNRM(3) )

      DUM(:) = 0.
      CALL M_MPI_REDUCE_D(DUM,RNRM,3,0)

      RNRM(1) = DSQRT( RNRM(1) )
      RNRM(2) = DSQRT( RNRM(2) )
      RNRM(3) = DSQRT( RNRM(3) )

      ICONV = 0

      IF( ITER > 0 ) THEN
        RRES = RNRM(1) / FNRM(1)
        IF( RRES < RR(6) .OR. RNRM(1) < RR(3) ) ICONV = 1
      ENDIF

      CALL M_MPI_BCAST_I(ICONV,1)

      IF( ITER == 0 ) THEN

        IF( ITER2 == 1 ) WRITE(ITO,'(/A,I0,A,1PE10.4,A)')
     &    ' ***** STEP NO. = ',ISTEP,'   TIME = ',TIM,' *****'

        WRITE(ITO,'(/A//A,A)') 
     &    '   * RESIDUAL FORCE',
     &    '     ITER     RNRM1       FNRM1       RRES        RNRM2  ',
     &    '     FNRM2       RNRM3       FNRM3'

      ELSE

        WRITE(ITO,'(7X,I2,1P7E12.4)') ITER,RNRM(1),FNRM(1),RRES,RNRM(2)
     &                               ,FNRM(2),RNRM(3),FNRM(3)

      ENDIF

      END
