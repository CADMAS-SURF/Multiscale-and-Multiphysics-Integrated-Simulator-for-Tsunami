      SUBROUTINE BLMBM(B,BX,BY,BZ,UX,UY,UZ,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION B(3,12),BX(3,12),BY(3,12),BZ(3,12),UX(3),UY(3),UZ(3)
     &         ,WK1(12),WK2(12)


      B(1,:) = BX(1,:)

      B(2,:) = BY(1,:) + BX(2,:)

      B(3,:) = BZ(1,:) + BX(3,:)

      IF( IGNL == 0 ) RETURN

      CALL AXB(WK1,UX,BX,1,3,12)

      B(1,:) = B(1,:) + WK1(:)

      CALL AXB(WK1,UX,BY,1,3,12)
      CALL AXB(WK2,UY,BX,1,3,12)

      B(2,:) = B(2,:) + WK1(:) + WK2(:)

      CALL AXB(WK1,UX,BZ,1,3,12)
      CALL AXB(WK2,UZ,BX,1,3,12)

      B(3,:) = B(3,:) + WK1(:) + WK2(:)

      END

      