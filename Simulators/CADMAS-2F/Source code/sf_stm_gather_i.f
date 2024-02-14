      SUBROUTINE SF_STM_GATHER_I(K0,K,ID)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'VF_APARAI.h'

      DIMENSION K0(NUMI0,NUMJ0),K(NUMI,NUMJ),IPT(4)

      INTEGER, POINTER :: KW(:,:)

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

        K0(:,:) = 0

        K0(MYIS:MYIE+IOP,MYJS:MYJE+JOP) = K(MYIS:MYIE+IOP,MYJS:MYJE+JOP)

        DO IP = 1, NPROCS - 1

          CALL SF_MPI_RECV_I(IPT,4,IP)

          IS = IPT(1)
          NI = IPT(2)
          JS = IPT(3)
          NJ = IPT(4)

          ALLOCATE( KW(NI,NJ) )

          CALL SF_MPI_RECV_I(KW,NI*NJ,IP)

          K0(IS:IS+NI-1,JS:JS+NJ-1) = KW(:,:)

          DEALLOCATE( KW )

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

        ALLOCATE( KW(NI,NJ) )

        KW(:,:) = K(MYIS:MYIE+IOP,MYJS:MYJE+JOP)

        CALL SF_MPI_SEND_I(KW,NI*NJ,0)

        DEALLOCATE( KW )

      ENDIF

      END

