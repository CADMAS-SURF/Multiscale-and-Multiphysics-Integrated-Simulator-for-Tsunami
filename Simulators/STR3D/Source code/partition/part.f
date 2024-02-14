      SUBROUTINE PART(KK,IFL,ICHK)

      USE M_VAL
      USE MPI_PARAM
      USE M_PART

      IMPLICIT REAL*8(A-H,O-Z)

      INTEGER*8 MUSED

      DIMENSION KK(*),IFL(*)

      INTEGER, POINTER :: IEPT(:),IENOD(:),IEP(:),INP(:)
      INTEGER, POINTER :: IFIL(:),IW(:,:),NW(:)

      WRITE(IFL(11),'(/X,A,F8.1,A)')
     &  'SUB. PART       : ', MUSED()*4.D-6, '(MB) USED.'

      NNOD = KK(8)
      NSOL = KK(10)
      NELM = KK(12)

      NPECG = NPROCS - 1

!     --- IEP, INP ---

      NN = 0

      DO I = 1, NSOL
        N = IELM(3,I)
        NN = NN + N
      ENDDO

      ALLOCATE( IEPT(0:NSOL) )
      ALLOCATE( IENOD(0:NN-1) )
      ALLOCATE( IEP(NSOL) )
      ALLOCATE( INP(NNOD) )

      IP = -1

      IEPT(0) = 0

      DO I = 1, NSOL

        N = IELM(3,I)

        IENOD(IP+1:IP+N) = IELM(8:7+N,I) - 1

        IP = IP + N

        IEPT(I) = IP + 1

      ENDDO

      NCOM = 3

      CALL METIS_PARTITION(NSOL,NNOD,IEPT,IENOD,NCOM,NPECG,IEP,INP)

      IEP(:) = IEP(:) + 1
      INP(:) = INP(:) + 1

      DEALLOCATE( IEPT )
      DEALLOCATE( IENOD )

      IF( ICHK == 1 ) CALL ENS_GEOM_P(KK,NPECG,IEP,IFL(20),IFL(21))

!     --- NN_INT, NN_EXT, NE, NOD, IEL ---

      MINT = 0
      ME = 0

      DO IP = 1, NPECG

        IN = 0

        DO I = 1, NNOD
          IF( INP(I) == IP ) IN = IN + 1
        ENDDO

        MINT = MAX0(MINT,IN)

        IE = 0

        DO I = 1, NSOL
          IF( IEP(I) == IP ) IE = IE + 1
        ENDDO

        ME = MAX0(ME,IE)

      ENDDO

      ALLOCATE( NN_INT(NPECG) )
      ALLOCATE( NN_EXT(NPECG) )
      ALLOCATE( NE(NPECG) )

      ALLOCATE( NOD(2*MINT,NPECG) )
      ALLOCATE( IEL(2*ME,NPECG) )

      ALLOCATE( IFIL(NNOD) )

      DO IP = 1, NPECG

        IN = 0
        IFIL(:) = 0

        DO I = 1, NNOD
          IF( INP(I) == IP ) THEN
            IN = IN + 1
            NOD(IN,IP) = I
            IFIL(I) = 1
          ENDIF
        ENDDO

        NN_INT(IP) = IN

        IE = 0

        DO I = 1, NELM
          NC = IELM(3,I)
          DO J = 1, NC
            JC = IELM(7+J,I)
            IF( INP(JC) == IP ) THEN
              IE = IE + 1
              IEL(IE,IP) = I
              DO K = 1, NC
                KC = IELM(7+K,I)
                IF( IFIL(KC) == 0 ) THEN
                  IN = IN + 1
                  NOD(IN,IP) = KC
                  IFIL(KC) = 1
                ENDIF
              ENDDO
              EXIT
            ENDIF
          ENDDO
        ENDDO

        NN_EXT(IP) = IN
        NE(IP) = IE

      ENDDO

      DEALLOCATE( IFIL )

!     --- NPEW, NIMPW, IPEW, IDXIMPW, NODIMPW ---

      MIMP = 0

      DO IP = 1, NPECG
        MIMP = MAX0( MIMP, NN_EXT(IP) - NN_INT(IP) )
      ENDDO

      ALLOCATE( IW(MIMP,NPECG) )
      ALLOCATE( NW(NPECG) )

      ALLOCATE( NPEW(NPECG) )
      ALLOCATE( NIMPW(NPECG) )

      ALLOCATE( IPEW(NPECG,NPECG) )
      ALLOCATE( IDXIMPW(2,NPECG,NPECG) )
      ALLOCATE( NODIMPW(MIMP,NPECG) )

      DO IP = 1, NPECG

        IW(:,:) = 0
        NW(:) = 0

        DO I = NN_INT(IP) + 1, NN_EXT(IP)
          ND = NOD(I,IP)
          JP = INP( ND )
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
            IPEW(IC,IP) = JP
            IDXIMPW(1,IC,IP) = IS
            IDXIMPW(2,IC,IP) = IE
            NODIMPW(IS:IE,IP) = IW(1:N,JP)
          ENDIF
        ENDDO

        NPEW(IP) = IC
        NIMPW(IP) = IE

      ENDDO

      DEALLOCATE( IW )
      DEALLOCATE( NW )

      DEALLOCATE( IEP )
      DEALLOCATE( INP )

!     --- NEXPW, IDXEXPW, NODEXPW ---

      MEXP = 0

      DO IP = 1, NPECG

        IE = 0

        DO I = 1, NPEW(IP)

          JP = IPEW(I,IP)

          DO J = 1, NPEW(JP)

            IF( IPEW(J,JP) == IP ) THEN
              JS = IDXIMPW(1,J,JP)
              JE = IDXIMPW(2,J,JP)
              N = JE - JS + 1
              IE = IE + N
              EXIT
            ENDIF

          ENDDO

        ENDDO

        MEXP = MAX0( MEXP, IE )

      ENDDO

      ALLOCATE( NEXPW(NPECG) )

      ALLOCATE( IDXEXPW(2,NPECG,NPECG) )
      ALLOCATE( NODEXPW(MEXP,NPECG) )

      DO IP = 1, NPECG

        IE = 0

        DO I = 1, NPEW(IP)

          JP = IPEW(I,IP)

          DO J = 1, NPEW(JP)

            IF( IPEW(J,JP) == IP ) THEN
              JS = IDXIMPW(1,J,JP)
              JE = IDXIMPW(2,J,JP)
              N = JE - JS + 1
              IS = IE + 1
              IE = IE + N
              IDXEXPW(1,I,IP) = IS
              IDXEXPW(2,I,IP) = IE
              NODEXPW(IS:IE,IP) = NODIMPW(JS:JE,JP)
              EXIT
            ENDIF

          ENDDO

        ENDDO

        NEXPW(IP) = IE

      ENDDO

      END
