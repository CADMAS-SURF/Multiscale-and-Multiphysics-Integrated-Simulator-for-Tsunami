      SUBROUTINE ETAHX1(E,DNDX)

!     E    : OUT : [Eij]
!                  E(:,1) = [E11] at (XG1,XG2,XG3)
!                  E(:,2) = [E22] at (XG1,XG2,XG3)
!                  E(:,3) = [E33] at (XG1,XG2,XG3)
!                  E(:,4) = 2*[E12] at (0,0,XG3) (次数低減積分)
!                  E(:,5) = 2*[E23] at (XG1,0,0) (次数低減積分)
!                  E(:,6) = 2*[E31] at (0,XG2,0) (次数低減積分)
!     DNDX : IN  : DNDX(i,j,1) = ∂Nj/∂xi at (XG1,XG2,XG3)
!                  DNDX(i,j,2) = ∂Nj/∂xi at (0,0,XG3)
!                  DNDX(i,j,3) = ∂Nj/∂xi at (XG1,0,0)
!                  DNDX(i,j,4) = ∂Nj/∂xi at (0,XG2,0)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(3,24,3),DNDX(3,8,4),FTF(300,3,3),E(300,6)

!     --- E11, E22, E33 ---

      F(:,:,:) = 0.

      DO I = 1, 3
        DO J = 1, 8
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J,1)
          ENDDO
        ENDDO
      ENDDO

      DO I = 1, 3
        CALL MATML(FTF(1,I,I),1,F(1,1,I),3,F(1,1,I),2,24,24,3)
      ENDDO

      E(:,1) = FTF(:,1,1)
      E(:,2) = FTF(:,2,2)
      E(:,3) = FTF(:,3,3)

!     --- E12 ---

      F(:,:,:) = 0.

      DO I = 1, 2
        DO J = 1, 8
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J,2)
          ENDDO
        ENDDO
      ENDDO

      CALL MATML(FTF(1,1,2),1,F(1,1,1),3,F(1,1,2),2,24,24,3)
      CALL MATML(FTF(1,2,1),1,F(1,1,2),3,F(1,1,1),2,24,24,3)

      E(:,4) = FTF(:,1,2) + FTF(:,2,1)

!     --- E23 ---

      F(:,:,:) = 0.

      DO I = 2, 3
        DO J = 1, 8
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J,3)
          ENDDO
        ENDDO
      ENDDO

      CALL MATML(FTF(1,2,3),1,F(1,1,2),3,F(1,1,3),2,24,24,3)
      CALL MATML(FTF(1,3,2),1,F(1,1,3),3,F(1,1,2),2,24,24,3)

      E(:,5) = FTF(:,2,3) + FTF(:,3,2)

!     --- E31 ---

      F(:,:,:) = 0.

      DO I = 1, 3, 2
        DO J = 1, 8
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J,4)
          ENDDO
        ENDDO
      ENDDO

      CALL MATML(FTF(1,3,1),1,F(1,1,3),3,F(1,1,1),2,24,24,3)
      CALL MATML(FTF(1,1,3),1,F(1,1,1),3,F(1,1,3),2,24,24,3)

      E(:,6) = FTF(:,3,1) + FTF(:,1,3)

      END
