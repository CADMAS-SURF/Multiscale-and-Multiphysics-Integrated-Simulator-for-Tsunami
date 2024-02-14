      SUBROUTINE ESTFHX1(ESTF,GRID,UG,NP,D,S,IGNL,ITO)

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

      DIMENSION TE(3,3),X0(3,8),U(3,8),GRID(3,*),UG(6,*),NP(8)
     &         ,D(21,2,2,2),DNDX(3,8,4),DUDX(3,3,4),BL(6,24),BLTD(24,6)
     &         ,BLTDBL(300),E(300,6),SE(300),S(6,2,2,2),ESTF(300)
     &         ,WK(576)

      INCLUDE 'gauss_ln_3.h'

      CALL LCCHX1(TE,X0,U,GRID,UG,NP)

      ESTF(:) = 0.

      DO I = 1, 2
      DO J = 1, 2
      DO K = 1, 2

        XG1 = XG(I,2)
        XG2 = XG(J,2)
        XG3 = XG(K,2)
        WGT = WG(I,2)*WG(J,2)*WG(K,2)

        CALL DERXHX1(DNDX,DET,XG1,XG2,XG3,X0,ITO)
        DO L = 1, 4
          CALL MATML(DUDX(1,1,L),2,DNDX(1,1,L),2,U,3,3,3,8)
        ENDDO

        CALL BLMHX1(BL,DNDX,DUDX,IGNL)
        CALL MATML(BLTD,2,BL,3,D(1,I,J,K),1,24,6,6)
        CALL MATML(BLTDBL,1,BLTD,2,BL,2,24,24,6)

        IF( IGNL > 0 ) THEN
          CALL ETAHX1(E,DNDX)
          CALL MATML(SE,2,E,2,S(1,I,J,K),2,300,1,6)
        ELSE
          SE(:) = 0.
        ENDIF

        ESTF(:) = ESTF(:) + ( BLTDBL(:) + SE(:) ) * DET * WGT

      ENDDO
      ENDDO
      ENDDO

      CALL STFGL(ESTF,WK,TE,24,3)

      END
