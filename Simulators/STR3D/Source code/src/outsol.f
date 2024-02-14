      SUBROUTINE OUTSOL(KK,GRID,IELM,NM,EPSG,SIGG,MGP,PG,NP_ENS,NG_ENS
     &                 ,IG_ENS,ITO,IFL)
C
      USE MPI_PARAM
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION KK(*),EPSG(6,MGP,*),SIGG(6,MGP,*),GRID(3,*),IELM(NM,*)
     &         ,SIG(6,20),EPS(6,20),PRNSIG(3,20),PRNEPS(3,20),VMSIG(20)
     &         ,NP_ENS(6),NG_ENS(*),IG_ENS(*),IFL(*),PG(*)
C
      INTEGER, POINTER :: IAVE(:)
      REAL(8), POINTER :: SIGP(:,:), EPSP(:,:), PRNSIGP(:,:),
     &                    PRNEPSP(:,:), VMSIGP(:)
C-----------------------------------------------------------------------
      NNOD = KK(8)
      NELM = KK(12)
      NNODI = KK(26)
C
      ALLOCATE( IAVE(NNOD) )
      ALLOCATE( SIGP(6,NNOD) )
      ALLOCATE( EPSP(6,NNOD) )
      ALLOCATE( PRNSIGP(3,NNOD) )
      ALLOCATE( PRNEPSP(3,NNOD) )
      ALLOCATE( VMSIGP(NNOD) )
C
      IAVE(:) = 0
      SIGP(:,:) = 0.
      EPSP(:,:) = 0.
      PRNSIGP(:,:) = 0.
      PRNEPSP(:,:) = 0.
      VMSIGP(:) = 0.
C
      DO I = 1, NELM

        ITYP = IELM(2,I)
        NNP = IELM(3,I)

        IF( ITYP /= 2 .AND. ITYP /= 6 ) CYCLE

        SELECT CASE( NNP )
        CASE( 4 )
          CALL STRTE1(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I))
        CASE( 10 )
          CALL STRTE2(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I))
        CASE( 6 )
          CALL STRPN1(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I),GRID,IELM(8,I))
        CASE( 15 )
          CALL STRPN2(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I))
        CASE( 8 )
          CALL STRHX1(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I),GRID,IELM(8,I))
        CASE( 20 )
          CALL STRHX2(EPS,SIG,EPSG(1,1,I),SIGG(1,1,I))
        END SELECT

        CALL MERGS(IAVE,SIGP,EPSP,SIG,EPS,PRNSIGP,PRNEPSP,PRNSIG
     &            ,PRNEPS,VMSIGP,VMSIG,IELM(8,I),NNP)
        
      ENDDO
C
C      IF( KK(25) > 0 ) THEN
C        DO I = 1, NNOD
C          SIGP(1:3,I) = SIGP(1:3,I) - PG(I)
C        ENDDO
C      ENDIF
C
      DO I = 1, NNOD
        CALL PRNSTR(1,SIGP(1,I),EPSP(1,I),PRNSIGP(1,I),PRNEPSP(1,I)
     &             ,VMSIGP(I),ITO)
      ENDDO
C
      IF( MYRANK == 0 ) THEN
C
        ICW = 0
C
        IPS = 1
        IPE = NP_ENS(2)
C
        CALL WTSOL(SIGP,EPSP,PRNSIGP,NNOD,IPS,IPE,NG_ENS,IG_ENS,IFL,ICW)
C
        IPS = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + 1
        IPE = NP_ENS(2) + NP_ENS(3) + NP_ENS(4) + NP_ENS(6)
C
        CALL WTSOL(SIGP,EPSP,PRNSIGP,NNOD,IPS,IPE,NG_ENS,IG_ENS,IFL,ICW)
C
      ELSE
C
        CALL M_MPI_SEND_D(SIGP,6*NNODI,0)
        CALL M_MPI_SEND_D(EPSP,6*NNODI,0)
        CALL M_MPI_SEND_D(PRNSIGP,3*NNODI,0)
C
      ENDIF
C
      DEALLOCATE( IAVE )
      DEALLOCATE( SIGP )
      DEALLOCATE( EPSP )
      DEALLOCATE( PRNSIGP )
      DEALLOCATE( PRNEPSP )
      DEALLOCATE( VMSIGP )
C
      END
