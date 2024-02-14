      SUBROUTINE FRICFACE(U0,RL0,NS,IELC,UG,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,4),POS(3,*),IELC(3),RL0(3),E(3),U0(3,4),UG(6,*)
C-----------------------------------------------------------------------
      U0(:,1:3) = UG(1:3,IELC(:))
      U0(:,4)   = UG(1:3,NS)
C
      X(:,1:3) = POS(:,IELC(:))
      X(:,4)   = POS(:,NS)
C
      CALL AREACD(RL0,E,H,X)
C
      END
