      SUBROUTINE SF_BND_GELEM(IEB,ID,I,J,K,XX,YY,ZZ,GELM,IELM)

      IMPLICIT REAL*8(A-H,O-Z)

      INCLUDE 'VF_A0PRM.h'
      INCLUDE 'VF_ANUMBI.h'
      INCLUDE 'SF_STRUCT.h'

      DIMENSION GELM(3,NELM),IELM(24,NELM),XX(MAXG1,NUMI),YY(MAXG1,NUMJ)
     &         ,ZZ(MAXG1,NUMK),GBND(3)

      SELECT CASE( ID )
      CASE( 1 )
        GBND(1) = XX(1,I)
        GBND(2) = ( YY(1,J) + YY(1,J+1) ) * .5D0
        GBND(3) = ( ZZ(1,K) + ZZ(1,K+1) ) * .5D0
      CASE( 2 )
        GBND(1) = ( XX(1,I) + XX(1,I+1) ) * .5D0
        GBND(2) = YY(1,J)
        GBND(3) = ( ZZ(1,K) + ZZ(1,K+1) ) * .5D0
      CASE( 3 )
        GBND(1) = ( XX(1,I) + XX(1,I+1) ) * .5D0
        GBND(2) = ( YY(1,J) + YY(1,J+1) ) * .5D0
        GBND(3) = ZZ(1,K)
      END SELECT

      DIST_MIN = 1.D20

      DO IE = 1, NELM
        CALL SF_LENGTH(DIST,GBND,GELM(1,IE),3)
        IF( DIST < DIST_MIN ) THEN
          DIST_MIN = DIST
          I_MIN = IE
        ENDIF
      ENDDO

      IF( IELM(2,I_MIN) == 1 ) THEN
        IEB = I_MIN
      ELSE
        IEB = 0
      ENDIF

      END