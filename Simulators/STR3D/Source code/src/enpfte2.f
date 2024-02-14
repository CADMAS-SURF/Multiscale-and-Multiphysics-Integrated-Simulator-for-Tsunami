      SUBROUTINE ENPFTE2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FC   : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,10),U(3,10),GRID(3,*),UG(6,*),NP(10),DNDX(3,10)
     &         ,DUDX(3,3),BL(6,30),EPS0(6),EPS(6,5),DEPS(6),D(21,5)
     &         ,DS(6),S(6,5),BLTS(3,10),FC(3,10)

      INCLUDE 'gauss_te_5.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      FC(:,:) = 0.

      DO I = 1, 5

        CALL DERXTE2(DNDX,DET,RLG(1,I),X0,ITO)
        CALL MATML(DUDX,2,DNDX,2,U,3,3,3,10)

        CALL BLMTX(BL,DNDX,DUDX,10,IGNL)

        EPS0(:) = EPS(:,I)
        CALL GREEN(EPS(:,I),DUDX,IGNL)
        DEPS(:) = EPS(:,I) - EPS0(:)

        CALL MATML(DS,2,D(1,I),1,DEPS,2,6,1,6)
        S(:,I) = S(:,I) + DS(:)

        CALL MATML(BLTS,2,BL,3,S(1,I),2,30,1,6)

        FC(:,:) = FC(:,:) + BLTS(:,:) * DET * WG(I)

      ENDDO

      END
