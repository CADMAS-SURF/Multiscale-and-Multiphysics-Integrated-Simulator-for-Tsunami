      SUBROUTINE RD_RESTART(IFL,KK,ISTART,TIM3,DT1)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*)

      IRST = KK(84)

      DO

        READ(IFL) ISTEP,TIM3,DT1,NNOW

        READ(IFL) UG1,UG2,UG3,POS,POSO,FTID,FCO,FCK,FCD,FCM,FCMD
     &           ,DMT,EPSG,SIGG

        IF( KK(16) > 0 ) READ(IFL) VG

        IF( KK(80) == 1 ) READ(IFL) IST,SIGY

        IF( KK(92) > 0 ) READ(IFL) ISLV,RSLV,U0,RL0,IFRIC,ISTK,FRIC

        IF( KK(25) > 0 ) READ(IFL) PG1,PG2,PG3,FCP

        IF( ISTM == 1 ) READ(IFL) GRID,SUMZ

        IF( ICPL == 2 ) THEN
          IF( NNOW >= IRST ) EXIT
        ELSE
          IF( ISTEP == IRST ) EXIT
        ENDIF

      ENDDO

      ISTART = ISTEP + 1

      END
