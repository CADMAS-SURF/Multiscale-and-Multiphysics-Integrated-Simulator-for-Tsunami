      SUBROUTINE SF_STM_SCATTER_D(X,X0)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION X0(NUMI0,NUMJ0),X(NUMI,NUMJ),IPT(4)

      REAL(8), POINTER :: XW(:,:)

      IF( MYRANK == 0 ) THEN

        X(:,:) = X0(MYGIS:MYGIE,MYGJS:MYGJE)

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(IPT,4,IP)

          IS = IPT(1)
          IE = IPT(2)
          JS = IPT(3)
          JE = IPT(4)
          
          NI = IE - IS + 1
          NJ = JE - JS + 1

          ALLOCATE( XW(NI,NJ) )

          XW(:,:) = X0(IS:IE,JS:JE)

          CALL SF_MPI_SEND_D(XW,NI*NJ,IP)

          DEALLOCATE( XW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        IPT(1) = MYGIS
        IPT(2) = MYGIE
        IPT(3) = MYGJS
        IPT(4) = MYGJE

        CALL SF_MPI_SEND_I(IPT,4,0)

        CALL SF_MPI_RECV_D(X,NUMI*NUMJ,0)

      ENDIF

      END

