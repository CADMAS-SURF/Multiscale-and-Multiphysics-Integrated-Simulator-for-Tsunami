      SUBROUTINE SF_STM_GATHER_D(X0,X,ID)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION X0(NUMI0,NUMJ0),X(NUMI,NUMJ),IPT(4)

      REAL(8), POINTER :: XW(:,:)

      IF( ID == 1 .AND. MYMIE == 1 ) THEN
        IOP = 1
      ELSE
        IOP = 0
      ENDIF

      IF( ID == 2 .AND. MYMJE == 1 ) THEN
        JOP = 1
      ELSE
        JOP = 0
      ENDIF

      IF( MYRANK == 0 ) THEN

        X0(:,:) = 0.D0

        X0(MYIS:MYIE+IOP,MYJS:MYJE+JOP) = X(MYIS:MYIE+IOP,MYJS:MYJE+JOP)

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(IPT,4,IP)

          IS = IPT(1)
          NI = IPT(2)
          JS = IPT(3)
          NJ = IPT(4)

          ALLOCATE( XW(NI,NJ) )

          CALL SF_MPI_RECV_D(XW,NI*NJ,IP)

          X0(IS:IS+NI-1,JS:JS+NJ-1) = XW(:,:)

          DEALLOCATE( XW )

        ENDDO

      ELSEIF( MYRANK > 0 ) THEN

        IS = MYGIS + MYMIS
        NI = MYIE - MYIS + 1 + IOP

        JS = MYGJS + MYMJS
        NJ = MYJE - MYJS + 1 + JOP

        IPT(1) = IS
        IPT(2) = NI
        IPT(3) = JS
        IPT(4) = NJ

        CALL SF_MPI_SEND_I(IPT,4,0)

        ALLOCATE( XW(NI,NJ) )

        XW(:,:) = X(MYIS:MYIE+IOP,MYJS:MYJE+JOP)

        CALL SF_MPI_SEND_D(XW,NI*NJ,0)

        DEALLOCATE( XW )

      ENDIF

      END

