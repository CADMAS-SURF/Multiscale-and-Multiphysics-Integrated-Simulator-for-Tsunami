      SUBROUTINE EFRCP(FCP,GRID,PPND,KN,ND,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FCP(3,*),GRID(3,*),PPND(*),KN(ND),X(3,ND),PB(ND)
     &         ,FP(3,ND)

      X(:,:) = GRID(:,KN(:))

      PB(:) = PPND(KN(:))

      SELECT CASE( ND )
      CASE( 4 )
        CALL EFPTE1(FP,X,PB,ITO)
      CASE( 10 )
        CALL EFPTE2(FP,X,PB,ITO)
      CASE( 6 )
        CALL EFPPN2(FP,ND,3,2,X,PB,ITO)
      CASE( 15 )
        CALL EFPPN2(FP,ND,7,3,X,PB,ITO)
      CASE( 8 ) 
        CALL EFPHX2(FP,ND,2,X,PB,ITO)
      CASE( 20 ) 
        CALL EFPHX2(FP,ND,3,X,PB,ITO)
      END SELECT

      FCP(:,KN(:)) = FCP(:,KN(:)) + FP(:,:)

      END
