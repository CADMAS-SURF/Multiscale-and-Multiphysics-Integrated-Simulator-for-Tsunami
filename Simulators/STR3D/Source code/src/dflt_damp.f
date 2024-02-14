      SUBROUTINE DFLT_DAMP(AMAT,NMAT,GRID,IELM,NM,NELM,W4,ITO)

      USE MPI_PARAM

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION GRID(3,*),IELM(NM,NELM),AMAT(33,NMAT),NP(20),X(3,20)

      REAL(8), POINTER :: RLB(:)

      ALLOCATE( RLB(NELM) )

      DO I = 1, NELM

        ITYP = IELM(2,I)
        NNP = IELM(3,I)

        NP(1:NNP) = IELM(8:7+NNP,I)        

        X(:,1:NNP) = GRID(:,NP(1:NNP))

        SELECT CASE( ITYP )
        CASE( 2, 6 )

          SELECT CASE( NNP )
          CASE( 4 )
            CALL TETVOL(X,V)
            V = V * 6.D0
          CASE( 10 )
            CALL EVOLTE2(V,X,ITO)
            V = V * 6.D0
          CASE( 6, 15 )
            CALL EVOLPN2(V,X,NNP,ITO)
            V = V * 2.D0
          CASE( 8, 20 )
            CALL EVOLHX2(V,X,NNP,ITO)
          END SELECT

          RLB(I) = V ** ( 1.D0 / 3.D0 )

        CASE( 3, 4 )

          CALL VECML2(RLB(I),X,3)
          RLB(I) = RLB(I) * 1.D-1

        END SELECT
        
      ENDDO

      DO IMAT = 1, NMAT

        E   = AMAT(1,IMAT)
        RHO = AMAT(3,IMAT)
        GE  = AMAT(4,IMAT)

        IF( GE > 0.D0 ) CYCLE

        RLB_MIN = 1.D20

        DO I = 1, NELM
          IF( IELM(4,I) == IMAT .AND. RLB(I) < RLB_MIN ) 
     &      RLB_MIN = RLB(I)
        ENDDO

        IF( MYRANK > 0 ) THEN
          CALL CG_MPI_ALLREDUCE_D(RLB_MIN,DMIN,1,2)
          RLB_MIN = DMIN
        ENDIF

        CK = RLB_MIN * DSQRT( RHO / E )

        AMAT(4,IMAT) = CK * W4

      ENDDO

      DEALLOCATE( RLB )

      END
