      SUBROUTINE MPCCORR11(UG,POS,NS,MA)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION POS(3,*),DUG(3),UG(6,*)
C----&------------------------------------------------------------------
      DUG(:) = POS(:,MA) - POS(:,NS)
C
      UG(1:3,NS) = UG(1:3,NS) + DUG(:)
      POS(:,NS) = POS(:,NS) + DUG(:)
C
      END
