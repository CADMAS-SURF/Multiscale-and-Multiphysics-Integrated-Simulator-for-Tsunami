      SUBROUTINE GEOMTX_S(EKPP,ECPP,ND,NN,KN,GRID,AMAT,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EKPP(NN),ECPP(NN),KN(ND),GRID(3,*),X(3,ND),AMAT(*)

      X(:,:) = GRID(:,KN(:))

      POR  = AMAT(7)
      RKF  = AMAT(8)
      RK   = AMAT(9)

      SELECT CASE( ND )
      CASE( 4 )
        CALL GMTXTE1_S(EKPP,ECPP,X,ITO)
      CASE( 10 )
        CALL GMTXTE2_S(EKPP,ECPP,X,ITO)
      CASE( 6, 15 )
        CALL GMTXPN2_S(EKPP,ECPP,ND,NN,X,ITO)
      CASE( 8, 20 ) 
        CALL GMTXHX2_S(EKPP,ECPP,ND,NN,X,ITO)
      END SELECT

      EKPP(:) = RK * EKPP(:)
      ECPP(:) = POR / RKF * ECPP(:)

      END
