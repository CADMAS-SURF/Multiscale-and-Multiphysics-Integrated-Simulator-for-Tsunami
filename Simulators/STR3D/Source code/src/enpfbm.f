      SUBROUTINE ENPFBM(FCG,EPS,S,GRID,UG,VG,NP,D1,D2,V,D,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION TE(3,3),U(3,2),VS(3,2),VT(3,2),VST(3,2),VTT(3,2)
     &         ,AS(3,3,2),AT(3,3,2),GRID(3,*),UG(6,*),VG(3,2,2),NP(2)
     &         ,V(3),BX(3,12),BY(3,12),BZ(3,12),UX(3),UY(3),UZ(3)
     &         ,B(3,12),EPS0(3),EPS(3,3,3),DEPS(3),DS(3),D(6,3,3)
     &         ,S(3,3,3),BTS(6,2),FC(6,2),FCG(6,2)

      INCLUDE 'gauss_ln_3.h'

      CALL LCCBM(RL,TE,U,VST,VTT,AS,AT,VS,VT,GRID,UG,VG,NP,V,IGNL)

      CALL DERXBM0(BY,BZ,UY,UZ,VS,VT,AS,AT)

      FC(:,:) = 0.

      DO I = 1, 3
      DO J = 1, 3

        SG = XG(I,3)
        TG = XG(J,3)
        WGT = WG(I,3)*WG(J,3)

        CALL DERXBM(BX,UX,SG,TG,RL,D1,D2,U,VS,VT,AS,AT)

        CALL BLMBM(B,BX,BY,BZ,UX,UY,UZ,IGNL)

        EPS0(:) = EPS(:,I,J)
        CALL GREEN_BM(EPS(1,I,J),UX,UY,UZ,IGNL)
        DEPS(:) = EPS(:,I,J) - EPS0(:)

        CALL MATML(DS,2,D(1,I,J),1,DEPS,2,3,1,3)
        S(:,I,J) = S(:,I,J) + DS(:)

        CALL MATML(BTS,2,B,3,S(1,I,J),2,12,1,3)

        FC(:,:) = FC(:,:) + BTS(:,:) * WGT * D1 * D2 * RL * .25D0

      ENDDO
      ENDDO

      CALL MATML(FCG,2,TE,3,FC,2,3,4,3)

      END
