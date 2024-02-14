      SUBROUTINE ENPFTE1(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FC   : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,4),U(3,4),GRID(3,*),UG(6,*),NP(4),DNDX(3,4)
     &         ,DUDX(3,3),BL(6,12),EPS0(6),EPS(6),DEPS(6),D(21),DS(6)
     &         ,S(6),BLTS(3,4),FC(3,4)

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      CALL DERXTE1(DNDX,DET,X0,ITO)
      CALL MATML(DUDX,2,DNDX,2,U,3,3,3,4)

      CALL BLMTX(BL,DNDX,DUDX,4,IGNL)

      EPS0(:) = EPS(:)
      CALL GREEN(EPS(:),DUDX,IGNL)
      DEPS(:) = EPS(:) - EPS0(:)

      CALL MATML(DS,2,D,1,DEPS,2,6,1,6)
      S(:) = S(:) + DS(:)

      CALL MATML(BLTS,2,BL,3,S,2,12,1,6)

      FC(:,:) = BLTS(:,:) * DET / 6.D0

      END
