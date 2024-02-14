      SUBROUTINE BLMPN1S(BLS,DNDX,DUDX,GQD,IGNL)
C
C     BLS  : OUT : BLS(1,:,ig) = [BL]の5行目の成分(積分点ig)
C                  BLS(2,:,ig) = [BL]の6行目の成分(積分点ig)
C     DNDX : IN  : DNDX(i,j,ig) = ∂Nj/∂xi (積分点ig)
C     DUDX : IN  : DUDX(i,j,ig) = ∂uj/∂xi (積分点ig)
C     GQD  : IN  : [E] (γyz,γzxの変換行列)
C     IGNL : IN  : =0:微小変形, =1:大変形
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION BLS(2,18,3),DNDX(3,6,3),DUDX(3,3,3),BL(6,18),GQD(6,6)
     &         ,BLB(6,18)
C
      BL(:,:) = 0.

      DO I = 1, 3

        DO J = 1, 6

          J0 = 3 * ( J - 1 )

          BL(I  ,J0+2) = DNDX(3,J,I)
          BL(I  ,J0+3) = DNDX(2,J,I)
          BL(I+3,J0+1) = DNDX(3,J,I)
          BL(I+3,J0+3) = DNDX(1,J,I)

          IF( IGNL == 0 ) CYCLE

          DO K = 1, 3
            BL(I  ,J0+K) = BL(I  ,J0+K)
     &                     + DUDX(2,K,I) * DNDX(3,J,I)
     &                     + DUDX(3,K,I) * DNDX(2,J,I)
            BL(I+3,J0+K) = BL(I+3,J0+K)
     &                     + DUDX(3,K,I) * DNDX(1,J,I)
     &                     + DUDX(1,K,I) * DNDX(3,J,I)
          ENDDO

        ENDDO

      ENDDO

      CALL AXB(BLB,GQD,BL,6,6,18)

      DO I = 1, 3
        BLS(1,:,I) = BLB(I,:)
        BLS(2,:,I) = BLB(I+3,:)
      ENDDO

      END