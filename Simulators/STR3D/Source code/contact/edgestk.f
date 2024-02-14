      SUBROUTINE EDGESTK(ISLV,RSLV,POSS,MA,IG,POS,EPS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),POS(3,*),IG(2),V(3),ISLV(2),RSLV(3),POSS(3)

      X(:,1:2) = POS(:,IG(:))
      X(:,3) = POSS(:)

      CALL LENCD(T,V,X)

      CALL VECML2(A,V,3)

      IF( T > 0.D0 .AND. T < 1.D0 .AND. A < EPS ) THEN
        ISLV(1) = 14
        ISLV(2) = MA
        RSLV(1) = T
      ENDIF

      END
