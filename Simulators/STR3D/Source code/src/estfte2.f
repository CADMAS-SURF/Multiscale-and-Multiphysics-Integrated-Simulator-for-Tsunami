      SUBROUTINE ESTFTE2(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

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

      DIMENSION X0(3,10),U(3,10),GRID(3,*),UG(6,*),NP(10),D(21,5)
     &         ,DNDX(3,10),DUDX(3,3),BL(6,30),BLTD(30,6),BLTDBL(465)
     &         ,E(465,6),SE(465),S(6,5),ESTF(465)

      INCLUDE 'gauss_te_5.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      ESTF(:) = 0.

      DO I = 1, 5

        CALL DERXTE2(DNDX,DET,RLG(1,I),X0,ITO)
        CALL MATML(DUDX,2,DNDX,2,U,3,3,3,10)

        CALL BLMTX(BL,DNDX,DUDX,10,IGNL)
        CALL MATML(BLTD,2,BL,3,D(1,I),1,30,6,6)
        CALL MATML(BLTDBL,1,BLTD,2,BL,2,30,30,6)

        IF( IGNL > 0 ) THEN
          CALL ETAMTX(E,DNDX,10,30,465)
          CALL MATML(SE,2,E,2,S(1,I),2,465,1,6)
        ELSE
          SE(:) = 0.
        ENDIF

        ESTF(:) = ESTF(:) + ( BLTDBL(:) + SE(:) ) * DET * WG(I)

      ENDDO

      END
