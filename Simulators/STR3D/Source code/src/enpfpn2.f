      SUBROUTINE ENPFPN2(FC,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FC   : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION X0(3,15),U(3,15),GRID(3,*),UG(6,*),NP(15),D(21,7,3)
     &         ,DNDX(3,15),DUDX(3,3),BL(6,45),EPS0(6),EPS(6,7,3),DEPS(6)
     &         ,DS(6),S(6,7,3),BLTS(3,15),FC(3,15)

      INCLUDE 'gauss_pn_73.h'

      X0(:,:) = GRID(:,NP(:))
      U(:,:) = UG(1:3,NP(:))

      FC(:,:) = 0.

      DO J = 1, 3
        DO I = 1, 7

          CALL DERXPN2(DNDX,DET,15,X0,RG(I),SG(I),TG(J),ITO)
          CALL MATML(DUDX,2,DNDX,2,U,3,3,3,15)

          CALL BLMTX(BL,DNDX,DUDX,15,IGNL)

          EPS0(:) = EPS(:,I,J)
          CALL GREEN(EPS(:,I,J),DUDX,IGNL)
          DEPS(:) = EPS(:,I,J) - EPS0(:)

          CALL MATML(DS,2,D(1,I,J),1,DEPS,2,6,1,6)
          S(:,I,J) = S(:,I,J) + DS(:)

          CALL MATML(BLTS,2,BL,3,S(1,I,J),2,45,1,6)

          FC(:,:) = FC(:,:) + BLTS(:,:) * DET * WGI(I) * WGJ(J)

        ENDDO
      ENDDO

      END
