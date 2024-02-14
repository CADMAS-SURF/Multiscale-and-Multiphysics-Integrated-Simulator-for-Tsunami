      SUBROUTINE ENPFHX2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FC   : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,20),U(3,20),GRID(3,*),UG(6,*),NP(20),D(21,3,3,3)
     &         ,DNDX(3,20),DUDX(3,3),BL(6,60),EPS0(6),EPS(6,3,3,3)
     &         ,DEPS(6),DS(6),S(6,3,3,3),BLTS(3,20),FC(3,20)

      INCLUDE 'gauss_ln_3.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      FC(:,:) = 0.

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

        EPS0(:) = EPS(:,I,J,K)
        CALL GREEN(EPS(:,I,J,K),DUDX,IGNL)
        DEPS(:) = EPS(:,I,J,K) - EPS0(:)

        CALL MATML(DS,2,D(1,I,J,K),1,DEPS,2,6,1,6)
        S(:,I,J,K) = S(:,I,J,K) + DS(:)

        CALL MATML(BLTS,2,BL,3,S(1,I,J,K),2,60,1,6)

        FC(:,:) = FC(:,:) + BLTS(:,:) * DET * WGT

      ENDDO
      ENDDO
      ENDDO

      END
