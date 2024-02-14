      SUBROUTINE RD_RESTARTP(IFL,KK,ISTART,TIM3,DT1)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*)

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(31,1,0)  ! SEND IOP=31 TO GLB_COMM

      IRST = KK(84)

      IF( MYRANK == 1 ) CALL M_MPI_SEND_I(IRST,1,0)

      N1 = KK(8)
      N3 = KK(8) + KK(28) + KK(94)
      N4 = KK(8) + KK(28) + KK(94) + KK(108)

      NELM = KK(12)

      DO

        READ(IFL) ISTEP,TIM3,DT1,NNOW

        READ(IFL) UG1(:,1:N3),UG2(:,1:N3),UG3(:,1:N3),POS(:,1:N4)
     &           ,POSO(:,1:N4),FTID(:,1:N1),FCO(:,1:N1,:),FCK(:,1:N1,:)
     &           ,FCD(:,1:N1,:),FCM(:,1:N1,:),FCMD(:,1:N1,:)
     &           ,DMT(:,1:NELM),EPSG(:,1:NELM),SIGG(:,1:NELM)

        IF( KK(80) == 1 ) READ(IFL) IST(:,1:NELM),SIGY(:,1:NELM)

        IF( KK(92) > 0 ) READ(IFL) ISLV0,ISLVP,ISTICK

        IF( KK(25) > 0 )
     &    READ(IFL) PG1(1:N3),PG2(1:N3),PG3(1:N3),FCP(:,1:N1,:)

        IF( ISTM == 1 ) READ(IFL) GRID(:,1:N1),SUMZ(1:N1)

        IF( ICPL == 2 ) THEN
          IF( NNOW >= IRST ) EXIT
        ELSE
          IF( ISTEP == IRST ) EXIT
        ENDIF

      ENDDO

      ISTART = ISTEP + 1

      END
