      SUBROUTINE GSURF(XG,XX,IELM,NM,NELM,IBEL,NDIM)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION XG(NDIM,*),XX(NDIM,*),IELM(NM,NELM),IBEL(NELM)

      IP = 1

      DO I = 1, NELM

        IF( IBEL(I) == 0 ) CYCLE

        NNP = IELM(3,I)

        SELECT CASE( NNP )
        CASE( 6 )
          CALL GSFPN(XG(1,IP),XX,IELM(8,I),NDIM)
          IP = IP + 3
        CASE( 8 )
          CALL GSFHX(XG(1,IP),XX,IELM(8,I),NDIM)
          IP = IP + 6
        END SELECT

      ENDDO

      END
