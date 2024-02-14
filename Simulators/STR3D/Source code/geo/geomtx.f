      SUBROUTINE GEOMTX(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,KN,GRID,AMAT
     &                 ,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EKUP(ND3,ND),EKPP(ND,ND),ECPU(ND,ND3),ECPP(ND,ND)
     &         ,EMPU(ND,ND3),KN(ND),GRID(3,*),X(3,ND),AMAT(*)

      X(:,:) = GRID(:,KN(:))

      POR  = AMAT(7)
      RKF  = AMAT(8)
      RK   = AMAT(9)
      RHOF = AMAT(10)

      SELECT CASE( ND )
      CASE( 4 )
        CALL GMTXTE1(EKUP,EKPP,ECPU,ECPP,EMPU,X,ITO)
      CASE( 10 )
        CALL GMTXTE2(EKUP,EKPP,ECPU,ECPP,EMPU,X,ITO)
      CASE( 6, 15 )
        CALL GMTXPN2(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,X,ITO)
      CASE( 8, 20 ) 
        CALL GMTXHX2(EKUP,EKPP,ECPU,ECPP,EMPU,ND,ND3,X,ITO)
      END SELECT

      EKPP(:,:) = RK * EKPP(:,:)
      ECPP(:,:) = POR / RKF * ECPP(:,:)
      EMPU(:,:) = RK * RHOF * EMPU(:,:)

      END
