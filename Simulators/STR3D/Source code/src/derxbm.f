      SUBROUTINE DERXBM(BX,UX,S,T,RL,D1,D2,U,VS,VT,AS,AT)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BX(3,12),UX(3),U(3,2),VS(3,2),VT(3,2),AS(3,3,2)
     &         ,AT(3,3,2)

      BX(:,:) = 0.

      BX(1,1) = -1.D0
      BX(2,2) = -1.D0
      BX(3,3) = -1.D0

      BX(:,4:6) = -.5D0 * ( S * D1 * AS(:,:,1) + T * D2 * AT(:,:,1) )

      BX(1,7) = 1.D0
      BX(2,8) = 1.D0
      BX(3,9) = 1.D0

      BX(:,10:12) = .5D0 * ( S * D1 * AS(:,:,2) + T * D2 * AT(:,:,2) )

      BX(:,:) = BX(:,:) / RL

      UX(:) = -U(:,1) + U(:,2) + S * .5D0 * D1 * ( -VS(:,1) + VS(:,2) )
     &                         + T * .5D0 * D2 * ( -VT(:,1) + VT(:,2) )

      UX(:) = UX(:) / RL

      END

      