      SUBROUTINE ETAPN1S(ES,DNDX,GQD)

!     ES   : OUT : ES(:,1,ig) = [E23] (積分点ig)
!                  ES(:,2,ig) = [E31] (積分点ig)
!     DNDX : IN  : DNDX(i,j,ig) = ∂Nj/∂xi (積分点ig)
!     GQD  : IN  : [E] (γyz,γzxの変換行列)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(3,18,3),DNDX(3,6,3),FTF(171,3,3),E(171,6),EB(171,6)
     &         ,GQD(6,6),ES(171,2,3)

      DO IG = 1, 3

        F(:,:,:) = 0.

        DO I = 1, 3
          DO J = 1, 6
            J0 = 3 * ( J - 1 )
            DO K = 1, 3
              F(K,J0+K,I) = DNDX(I,J,IG)
            ENDDO
          ENDDO
        ENDDO

        CALL MATML(FTF(1,2,3),1,F(1,1,2),3,F(1,1,3),2,18,18,3)
        CALL MATML(FTF(1,3,2),1,F(1,1,3),3,F(1,1,2),2,18,18,3)

        E(:,IG) = FTF(:,2,3) + FTF(:,3,2)

        CALL MATML(FTF(1,3,1),1,F(1,1,3),3,F(1,1,1),2,18,18,3)
        CALL MATML(FTF(1,1,3),1,F(1,1,1),3,F(1,1,3),2,18,18,3)

        E(:,IG+3) = FTF(:,3,1) + FTF(:,1,3)

      ENDDO

      CALL MATML(EB,2,E,2,GQD,3,171,6,6)

      DO IG = 1, 3
        ES(:,1,IG) = EB(:,IG)
        ES(:,2,IG) = EB(:,IG+3)
      ENDDO

      END
