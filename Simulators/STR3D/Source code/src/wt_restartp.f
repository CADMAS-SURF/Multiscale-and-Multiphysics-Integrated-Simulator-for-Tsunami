      SUBROUTINE WT_RESTARTP(IFL,IFLT,KK,ISTEP,TIM3,DT1,ISEND)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*)

      IF( ISTEP == KK(7) ) ISEND = 1

      IF( MYRANK == 1 ) THEN
        CALL M_MPI_SEND_I(30,1,0)  ! SEND IOP=30 TO GLB_COMM
        CALL M_MPI_SEND_I(ISEND,1,0)
      ENDIF

      IRW = 0

      IF( ICPL == 2 ) THEN
        IF( ISEND == 1 ) THEN
          CALL M_MPI_BCAST_I(N,1)
          IF( N > 0 ) THEN
            CALL M_MPI_BCAST_I(NNOW,1)
            IRW = 1
          ENDIF
        ENDIF
      ELSE
        IF( IROUT(ISTEP) > 0 ) IRW = 1
      ENDIF

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(IRW,1,0)

      IF( IRW == 0 ) RETURN

      N1 = KK(8)
      N3 = KK(8) + KK(28) + KK(94)
      N4 = KK(8) + KK(28) + KK(94) + KK(108)

      NELM = KK(12)

      WRITE(IFL) ISTEP,TIM3,DT1,NNOW

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(ISTEP,1,0)

      WRITE(IFL) UG1(:,1:N3),UG2(:,1:N3),UG3(:,1:N3),POS(:,1:N4)
     &          ,POSO(:,1:N4),FTID(:,1:N1),FCO(:,1:N1,:),FCK(:,1:N1,:)
     &          ,FCD(:,1:N1,:),FCM(:,1:N1,:),FCMD(:,1:N1,:)
     &          ,DMT(:,1:NELM),EPSG(:,1:NELM),SIGG(:,1:NELM)

      IF( KK(80) == 1 ) WRITE(IFL) IST(:,1:NELM),SIGY(:,1:NELM)

      IF( KK(92) > 0 ) WRITE(IFL) ISLV0,ISLVP,ISTICK

      IF( KK(25) > 0 )
     &  WRITE(IFL) PG1(1:N3),PG2(1:N3),PG3(1:N3),FCP(:,1:N1,:)

      IF( ISTM == 1 ) WRITE(IFL) GRID(:,1:N1),SUMZ(1:N1)

      IF( MYRANK == 1 ) CALL M_MPI_SEND_D(TIM3,1,0)

      END
