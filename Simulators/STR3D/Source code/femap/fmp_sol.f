      SUBROUTINE FMP_SOL(IOUT,KK,INDG,GRID,IELM,NM,EPSG,SIGG,MGP,PG,ITO
     &                  ,IFL)
C
      USE MPI_PARAM
      IMPLICIT REAL*8(A-H,O-Z)
      CHARACTER*1 C /','/
      DIMENSION KK(*),EPSG(6,MGP,*),SIGG(6,MGP,*),INDG(*),GRID(3,*)
     &         ,IELM(NM,*),SIG(6,20),EPS(6,20),PRNSIG(3,20),PRNEPS(3,20)
     &         ,VMSIG(20),PG(*)
C
      INTEGER, POINTER :: IAVE(:)
      REAL(8), POINTER :: SIGP(:,:), EPSP(:,:), PRNSIGP(:,:),
     &                    PRNEPSP(:,:), VMSIGP(:)
C-----------------------------------------------------------------------
      NNOD = KK(8)
      NELM = KK(12)
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
        NNP  = IELM(3,I)

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
        WRITE(IFL,'(3(I0,A))') IOUT,C,30,C,1,C
C
        WRITE(IFL,'(A)') 'X Normal Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,1,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,31,C,1,C
C
        WRITE(IFL,'(A)') 'Y Normal Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,2,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,32,C,1,C
C
        WRITE(IFL,'(A)') 'Z Normal Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,3,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,33,C,1,C
C
        WRITE(IFL,'(A)') 'XY Shear Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,4,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,34,C,1,C
C
        WRITE(IFL,'(A)') 'YZ Shear Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,5,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,35,C,1,C
C
        WRITE(IFL,'(A)') 'ZX Shear Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,SIGP,6,6,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,36,C,1,C
C
        WRITE(IFL,'(A)') 'Max Prin Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,PRNSIGP,1,3,NNOD,IFL)
C
        WRITE(IFL,'(3(I0,A))') IOUT,C,37,C,1,C
C
        WRITE(IFL,'(A)') 'Min Prin Stress'
C
        CALL WT_VEC(0,0,0,7,INDG,PRNSIGP,3,3,NNOD,IFL)
C
      ELSE
C
        NNODI = KK(26)
C
        CALL M_MPI_SEND_D(SIGP,6*NNODI,0)
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
