      SUBROUTINE ESTFHX2(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

!     ESTF : OUT : 要素剛性行列(上三角,1次元配列)
!                  配列要素の並びは3×3の行列を例にとると以下の通り
!
!                    1 2 3
!                      4 5
!                        6
!
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     S    : IN  : 応力
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,20),U(3,20),GRID(3,*),UG(6,*),NP(20),D(21,3,3,3)
     &         ,DNDX(3,20),DUDX(3,3),BL(6,60),BLTD(60,6),BLTDBL(1830)
     &         ,E(1830,6),SE(1830),S(6,3,3,3),ESTF(1830)

      INCLUDE 'gauss_ln_3.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      ESTF(:) = 0.

      DO I = 1, 3
      DO J = 1, 3
      DO K = 1, 3

        XG1 = XG(I,3)
        XG2 = XG(J,3)
        XG3 = XG(K,3)
        WGT = WG(I,3)*WG(J,3)*WG(K,3)

        CALL DERXHX2(DNDX,DET,XG1,XG2,XG3,X0,20,ITO)
        CALL MATML(DUDX,2,DNDX,2,U,3,3,3,20)

        CALL BLMTX(BL,DNDX,DUDX,20,IGNL)
        CALL MATML(BLTD,2,BL,3,D(1,I,J,K),1,60,6,6)
        CALL MATML(BLTDBL,1,BLTD,2,BL,2,60,60,6)

        IF( IGNL > 0 ) THEN
          CALL ETAMTX(E,DNDX,20,60,1830)
          CALL MATML(SE,2,E,2,S(1,I,J,K),2,1830,1,6)
        ELSE
          SE(:) = 0.
        ENDIF

        ESTF(:) = ESTF(:) + ( BLTDBL(:) + SE(:) ) * DET * WGT

      ENDDO
      ENDDO
      ENDDO

      END
