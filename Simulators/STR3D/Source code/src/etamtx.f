      SUBROUTINE ETAMTX(E,DNDX,N,N3,NL)

!     E    : OUT : [Eij]
!                  E(:,1) = [E11]
!                  E(:,2) = [E22]
!                  E(:,3) = [E33]
!                  E(:,4) = 2*[E12]
!                  E(:,5) = 2*[E23]
!                  E(:,6) = 2*[E31]
!     DNDX : IN  : DNDX(i,j) = ∂Nj/∂xi
!     N    : IN  : 節点数
!     N3   : IN  : 3*節点数
!     NL   : IN  : 要素剛性行列のサイズ

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION F(3,N3,3),DNDX(3,N),FTF(NL,3,3),E(NL,6)

      F(:,:,:) = 0.

      DO I = 1, 3
        DO J = 1, N
          J0 = 3 * ( J - 1 )
          DO K = 1, 3
            F(K,J0+K,I) = DNDX(I,J)
          ENDDO
        ENDDO
      ENDDO

      DO I = 1, 3
        DO J = 1, 3
          CALL MATML(FTF(1,I,J),1,F(1,1,I),3,F(1,1,J),2,N3,N3,3)
        ENDDO
      ENDDO

      E(:,1) = FTF(:,1,1)
      E(:,2) = FTF(:,2,2)
      E(:,3) = FTF(:,3,3)
      E(:,4) = FTF(:,1,2) + FTF(:,2,1)
      E(:,5) = FTF(:,2,3) + FTF(:,3,2)
      E(:,6) = FTF(:,3,1) + FTF(:,1,3)

      END
