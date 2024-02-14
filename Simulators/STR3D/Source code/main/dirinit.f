      SUBROUTINE DIRINIT(VG,NP,V,GRID)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION E(3,3),GRID(3,*),NP(2),VG(3,2,2),V(3)

      E(:,1) = GRID(:,NP(2)) - GRID(:,NP(1))
      CALL DIRCOS(E(1,1),E(1,1),3)
      CALL CROSS2(E(1,1),V,E(1,3))
      CALL CROSS(E(1,3),E(1,1),E(1,2))

      VG(:,:,1) = E(:,2:3)
      VG(:,:,2) = E(:,2:3)

      END
     