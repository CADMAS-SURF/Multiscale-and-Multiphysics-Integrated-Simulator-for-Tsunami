      SUBROUTINE OUTSOIL(KK,IELM,NM,VELG,MGP,VELE,NP_ENS,NG_ENS,IG_ENS
     &                  ,IFL)

      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),IELM(NM,*),VEL(3,20),VELG(3,MGP,*),VELE(3,*)
     &         ,NP_ENS(6),NG_ENS(*),IG_ENS(*)

      INTEGER, POINTER :: NAVE(:)
      REAL(8), POINTER :: VELP(:,:)

      NNOD = KK(8)
      NELM = KK(12)
      NNODI = KK(26)

      ALLOCATE( NAVE(NNOD) )
      ALLOCATE( VELP(3,NNOD) )

      NAVE(:) = 0
      VELP(:,:) = 0.

      DO I = 1, NELM

        ITYP = IELM(2,I)
        ND   = IELM(3,I)

        IF( ITYP /= 6 ) CYCLE

        SELECT CASE( ND )
        CASE( 4 )
          CALL VELTE1(VEL,VELG(1,1,I))
        CASE( 10 )
          CALL VELTE2(VEL,VELG(1,1,I))
        CASE( 6, 15 )
          CALL VELPN2(VEL,VELG(1,1,I),ND)
        CASE( 8 )
          CALL VELHX2(VEL,VELG(1,1,I),VELE(1,I),ND,2)
        CASE( 20 )
          CALL VELHX2(VEL,VELG(1,1,I),VELE(1,I),ND,3)
        END SELECT

        CALL VAVRG(VELP,VEL,IELM(8,I),ND,NAVE)

      ENDDO

      IF( MYRANK == 0 ) THEN

        IPS = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + 1
        IPE = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + NP_ENS(6)

        CALL WTSOIL(VELP,NNOD,IPS,IPE,NG_ENS,IG_ENS,IFL)

      ELSE

        CALL M_MPI_SEND_D(VELP,3*NNODI,0)

      ENDIF

      DEALLOCATE( NAVE )
      DEALLOCATE( VELP )

      END
