      SUBROUTINE ENPFPN1(FCG,EPS,S,GRID,UG,NP,D,IGNL,ITO)

!     FCG  : OUT : 等価節点力
!     EPS  : OUT : 歪
!     S    : OUT : 応力
!     GRID : IN  : 節点座標
!     UG   : IN  : 節点変位
!     NP   : IN  : 要素の構成節点番号
!     D    : IN  : 構成則マトリックス
!     IGNL : IN  : =0:微小変形, =1:大変形

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION TE(3,3),X0(3,6),U(3,6),GRID(3,*),UG(6,*),NP(6),TR(6,6)
     &         ,DNDX(3,6,3),DUDX(3,3,3),BLS(2,18,3),EPSS(2,3),BL(6,18)
     &         ,D(21,3,2),EPS0(6),EPS(6,3,2),DEPS(6),DS(6),S(6,3,2)
     &         ,BLTS(3,6),FC(3,6),FCG(3,6)

      INCLUDE 'gauss_pn_32.h'

      CALL LCCPN1(TE,X0,U,GRID,UG,NP)

      CALL TRPN1S(TR,X0,ITO)

      DO I = 1, 3
        CALL DERXPN2(DNDX(1,1,I),DET,6,X0,RG(I),SG(I),0.D0,ITO)
        CALL MATML(DUDX(1,1,I),2,DNDX(1,1,I),2,U,3,3,3,6)
      ENDDO

      CALL BLMPN1S(BLS,DNDX,DUDX,TR,IGNL)

      CALL GREEN_PN1S(EPSS,DUDX,TR,IGNL)

      FC(:,:) = 0.

      DO J = 1, 2
        DO I = 1, 3

          CALL DERXPN2(DNDX,DET,6,X0,RG(I),SG(I),TG(J),ITO)
          CALL MATML(DUDX,2,DNDX,2,U,3,3,3,6)

          CALL BLMPN1(BL,DNDX,DUDX,IGNL)
          BL(5:6,:) = BLS(:,:,I)

          EPS0(:) = EPS(:,I,J)
          CALL GREEN_PN1(EPS(:,I,J),DUDX,IGNL)
          EPS(5:6,I,J) = EPSS(:,I)
          DEPS(:) = EPS(:,I,J) - EPS0(:)

          CALL MATML(DS,2,D(1,I,J),1,DEPS,2,6,1,6)
          S(:,I,J) = S(:,I,J) + DS(:)

          CALL MATML(BLTS,2,BL,3,S(1,I,J),2,18,1,6)

          FC(:,:) = FC(:,:) + BLTS(:,:) * DET / 6.D0

        ENDDO
      ENDDO

      CALL MATML(FCG,2,TE,3,FC,2,3,6,3)

      END
