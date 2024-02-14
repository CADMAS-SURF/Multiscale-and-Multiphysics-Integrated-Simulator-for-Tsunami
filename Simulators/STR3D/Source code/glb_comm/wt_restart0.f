      SUBROUTINE WT_RESTART0(KK,IFL,FLNAME,NLEN)

      USE M_VAL
      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*256 FLNAME
      DIMENSION KK(*),IFL(*),NNOW(100),TNOW(100)

      DATA INIT / 0 /

      CALL M_MPI_RECV_I(ISEND,1,1)

      N = 1

      IF( ICPL == 2 .AND. ISEND == 1 ) THEN
        CALL C_MPI_RECV_I(N,1,IROOTC)
        IF( N > 0 ) THEN
          CALL C_MPI_RECV_I(NNOW,N,IROOTC)
          CALL C_MPI_RECV_D(TNOW,N,IROOTC)
        ENDIF
        CALL M_MPI_BCAST_I(N,1)
        IF( N > 0 ) CALL M_MPI_BCAST_I(NNOW(N),1)
      ENDIF

      CALL M_MPI_RECV_I(IRW,1,1)

      IF( IRW == 0 ) RETURN

      IF( INIT == 0 ) THEN

        INIT = 1

        OPEN(IFL(16),FILE=FLNAME(1:NLEN)//'.rsto000',FORM='UNFORMATTED')

        OPEN(IFL(18),FILE=FLNAME(1:NLEN)//'.rtm')

        IF( ICPL == 2 )
     &    WRITE(IFL(18),'(A/A/)')
     &    '     [ C A D M A S ]          [ S  T  R ]',
     &    '    STEP        TIME       STEP        TIME'

      ENDIF

      CALL M_MPI_RECV_I(ISTEP,1,1)

      WRITE(IFL(16)) ISTEP,NNOW(N)

      IF( KK(92) > 0 )
     &  WRITE(IFL(16)) ISLV0,ISLVP,RSLV0,U0,RL0,IFRIC,ISTK,FRIC

      CALL M_MPI_RECV_D(TIM3,1,1)

      IF( ICPL == 2 ) THEN
        DO I = 1, N
          WRITE(IFL(18),'(2(I8,F15.6))') NNOW(I),TNOW(I),ISTEP,TIM3
        ENDDO
      ELSE
        WRITE(IFL(18),'(F15.6)') TIM3
      ENDIF

      END
