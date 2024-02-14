      SUBROUTINE PART_SEND_TBLX2(KK)

      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION KK(*)

      INTEGER, POINTER :: IW(:,:),NW(:),LNOD(:)
      INTEGER, POINTER :: NPIMPXW(:),NIMPXW(:),IPIMPXW(:,:)
      INTEGER, POINTER :: IDXIMPXW(:,:,:),NODIMPXW(:,:)
      INTEGER, POINTER :: NPEXPXW(:),NEXPXW(:),IPEXPXW(:,:)
      INTEGER, POINTER :: IDXEXPXW(:,:,:),NODEXPXW(:,:)

      NNOD = KK(8)

      NPECG = NPROCS - 1

!     --- NPIMPXW, NIMPXW, IPIMPXW, IDXIMPXW, NODIMPXW ---

      MIMP = MAXVAL(NN_EXTX)

      ALLOCATE( IW(MIMP,NPECG) )
      ALLOCATE( NW(NPECG) )

      ALLOCATE( NPIMPXW(NPECG) )
      ALLOCATE( NIMPXW(NPECG) )
      ALLOCATE( IPIMPXW(NPECG,NPECG) )
      ALLOCATE( IDXIMPXW(2,NPECG,NPECG) )
      ALLOCATE( NODIMPXW(MIMP,NPECG) )

      DO IP = 1, NPECG

        IW(:,:) = 0
        NW(:) = 0

        DO I = 1, NN_EXTX(IP)
          ND = NODX(I,IP)
          JP = NODP(1,ND)
          NW(JP) = NW(JP) + 1
          IW( NW(JP), JP ) = ND
        ENDDO

        IC = 0
        IE = 0

        DO JP = 1, NPECG
          N = NW(JP)
          IF( N > 0 ) THEN
            IC = IC + 1
            IS = IE + 1
            IE = IE + N
            IPIMPXW(IC,IP) = JP
            IDXIMPXW(1,IC,IP) = IS
            IDXIMPXW(2,IC,IP) = IE
            NODIMPXW(IS:IE,IP) = IW(1:N,JP)
          ENDIF
        ENDDO

        NPIMPXW(IP) = IC
        NIMPXW(IP) = IE

      ENDDO

      DEALLOCATE( IW )
      DEALLOCATE( NW )

!     --- NPEXPXW, NEXPXW, IPEXPXW, IDXEXPXW, NODEXPXW ---

      MEXP = 0

      DO IP = 1, NPECG

        IE = 0

        DO JP = 1, NPECG

          DO J = 1, NPIMPXW(JP)

            IF( IPIMPXW(J,JP) == IP ) THEN
              JS = IDXIMPXW(1,J,JP)
              JE = IDXIMPXW(2,J,JP)
              N = JE - JS + 1
              IE = IE + N
              EXIT
            ENDIF

          ENDDO

        ENDDO

        MEXP = MAX0( MEXP, IE )

      ENDDO

      ALLOCATE( NPEXPXW(NPECG) )
      ALLOCATE( NEXPXW(NPECG) )
      ALLOCATE( IPEXPXW(NPECG,NPECG) )
      ALLOCATE( IDXEXPXW(2,NPECG,NPECG) )
      ALLOCATE( NODEXPXW(MEXP,NPECG) )

      DO IP = 1, NPECG

        IC = 0
        IE = 0

        DO JP = 1, NPECG

          DO J = 1, NPIMPXW(JP)

            IF( IPIMPXW(J,JP) == IP ) THEN
              IC = IC + 1
              IPEXPXW(IC,IP) = JP
              JS = IDXIMPXW(1,J,JP)
              JE = IDXIMPXW(2,J,JP)
              N = JE - JS + 1
              IS = IE + 1
              IE = IE + N
              IDXEXPXW(1,IC,IP) = IS
              IDXEXPXW(2,IC,IP) = IE
              NODEXPXW(IS:IE,IP) = NODIMPXW(JS:JE,JP)
              EXIT
            ENDIF

          ENDDO

        ENDDO

        NPEXPXW(IP) = IC
        NEXPXW(IP) = IE

      ENDDO

!     --- IMPORT & EXPORT NODE ---

      ALLOCATE( LNOD(NNOD) )

      DO IP = 1, NPECG

        LNOD(:) = 0

        DO I = 1, NN_EXT(IP)
          LNOD( NOD(I,IP) ) = I
        ENDDO

        DO I = 1, NN_EXTX(IP)
          LNOD( NODX(I,IP) ) = NN_EXT(IP) + NN_EXTC(IP) + NG(IP)
     &                         + NGC(IP) + I
        ENDDO

        NODIMPXW(1:NIMPXW(IP),IP) = LNOD( NODIMPXW(1:NIMPXW(IP),IP) )
        NODEXPXW(1:NEXPXW(IP),IP) = LNOD( NODEXPXW(1:NEXPXW(IP),IP) )

        CALL M_MPI_SEND_I(NPIMPXW(IP),1,IP)
        CALL M_MPI_SEND_I(NIMPXW(IP),1,IP)
        IF( NPIMPXW(IP) > 0 ) THEN
          CALL M_MPI_SEND_I(IPIMPXW(1,IP),NPIMPXW(IP),IP)
          CALL M_MPI_SEND_I(IDXIMPXW(1,1,IP),2*NPIMPXW(IP),IP)
          CALL M_MPI_SEND_I(NODIMPXW(1,IP),NIMPXW(IP),IP)
        ENDIF

        CALL M_MPI_SEND_I(NPEXPXW(IP),1,IP)
        CALL M_MPI_SEND_I(NEXPXW(IP),1,IP)
        IF( NPEXPXW(IP) > 0 ) THEN
          CALL M_MPI_SEND_I(IPEXPXW(1,IP),NPEXPXW(IP),IP)
          CALL M_MPI_SEND_I(IDXEXPXW(1,1,IP),2*NPEXPXW(IP),IP)
          CALL M_MPI_SEND_I(NODEXPXW(1,IP),NEXPXW(IP),IP)
        ENDIF

      ENDDO

      DEALLOCATE( NPIMPXW )
      DEALLOCATE( NIMPXW )
      DEALLOCATE( IPIMPXW )
      DEALLOCATE( IDXIMPXW )
      DEALLOCATE( NODIMPXW )

      DEALLOCATE( NPEXPXW )
      DEALLOCATE( NEXPXW )
      DEALLOCATE( IPEXPXW )
      DEALLOCATE( IDXEXPXW )
      DEALLOCATE( NODEXPXW )

      DEALLOCATE( LNOD )

      END
