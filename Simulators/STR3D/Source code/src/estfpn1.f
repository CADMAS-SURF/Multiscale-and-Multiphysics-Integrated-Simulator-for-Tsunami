      SUBROUTINE ESTFPN1(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

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

      DIMENSION TE(3,3),X0(3,6),U(3,6),GRID(3,*),UG(6,*),NP(6),TR(6,6)
     &         ,DNDX(3,6,3),DUDX(3,3,3),BLS(2,18,3),ES(171,2,3),BL(6,18)
     &         ,D(21,3,2),BLTD(18,6),BLTDBL(171),E(171,6),SE(171)
     &         ,S(6,3,2),ESTF(171),WK(324)

      INCLUDE 'gauss_pn_32.h'

      CALL LCCPN1(TE,X0,U,GRID,UG,NP)

      CALL TRPN1S(TR,X0,ITO)

      DO I = 1, 3
        CALL DERXPN2(DNDX(1,1,I),DET,6,X0,RG(I),SG(I),0.D0,ITO)
        CALL MATML(DUDX(1,1,I),2,DNDX(1,1,I),2,U,3,3,3,6)
      ENDDO

      CALL BLMPN1S(BLS,DNDX,DUDX,TR,IGNL)

      IF( IGNL > 0 ) CALL ETAPN1S(ES,DNDX,TR)

      ESTF(:) = 0.

      DO J = 1, 2
        DO I = 1, 3

          CALL DERXPN2(DNDX,DET,6,X0,RG(I),SG(I),TG(J),ITO)
          CALL MATML(DUDX,2,DNDX,2,U,3,3,3,6)

          CALL BLMPN1(BL,DNDX,DUDX,IGNL)
          BL(5:6,:) = BLS(:,:,I)
          CALL MATML(BLTD,2,BL,3,D(1,I,J),1,18,6,6)
          CALL MATML(BLTDBL,1,BLTD,2,BL,2,18,18,6)

          IF( IGNL > 0 ) THEN
            CALL ETAPN1(E,DNDX)
            E(:,5:6) = ES(:,:,I)
            CALL MATML(SE,2,E,2,S(1,I,J),2,171,1,6)
          ELSE
            SE(:) = 0.
          ENDIF

          ESTF(:) = ESTF(:) + ( BLTDBL(:) + SE(:) ) * DET / 6.D0

        ENDDO
      ENDDO

      CALL STFGL(ESTF,WK,TE,18,3)

      END
