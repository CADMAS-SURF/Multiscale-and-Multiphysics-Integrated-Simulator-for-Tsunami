      SUBROUTINE FRICEDGE(U0,RL0,NS,IG,UG,POS)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,3),POS(3,*),IG(2),RL0(2),V(3),U0(3,3),UG(6,*)
C-----------------------------------------------------------------------
      U0(:,1:2) = UG(1:3,IG(:))
      U0(:,3)   = UG(1:3,NS)
C
      X(:,1:2) = POS(:,IG(:))
      X(:,3)   = POS(:,NS)
C
      CALL LENCD(RL0(2),V,X)
C
      RL0(1) = 1.D0 - RL0(2)
C
      END
