      SUBROUTINE ESTFBM(ESTF,GRID,UG,VG,NP,D1,D2,V,D,S,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION TE(3,3),U(3,2),VS(3,2),VT(3,2),VST(3,2),VTT(3,2)
     &         ,AS(3,3,2),AT(3,3,2),GRID(3,*),UG(6,*),VG(3,2,2),NP(2)
     &         ,V(3),BX(3,12),BY(3,12),BZ(3,12),UX(3),UY(3),UZ(3)
     &         ,B(3,12),D(6,3,3),S(3,3,3),BTD(12,3),BTDB(78),E(78,3)
     &         ,SE(78),ESTF(78),WK(144)

      INCLUDE 'gauss_ln_3.h'

      CALL LCCBM(RL,TE,U,VST,VTT,AS,AT,VS,VT,GRID,UG,VG,NP,V,IGNL)

      CALL DERXBM0(BY,BZ,UY,UZ,VS,VT,AS,AT)

      ESTF(:) = 0.

      DO I = 1, 3
      DO J = 1, 3

        SG = XG(I,3)
        TG = XG(J,3)
        WGT = WG(I,3)*WG(J,3)

        CALL DERXBM(BX,UX,SG,TG,RL,D1,D2,U,VS,VT,AS,AT)

        CALL BLMBM(B,BX,BY,BZ,UX,UY,UZ,IGNL)
        CALL MATML(BTD,2,B,3,D(1,I,J),1,12,3,3)
        CALL MATML(BTDB,1,BTD,2,B,2,12,12,3)

        IF( IGNL > 0 ) THEN
          CALL ETABM(E,SG,TG,RL,D1,D2,VST,VTT,BX,BY,BZ,UX,UY,UZ)
          SE(:) = S(1,I,J) * E(:,1) + 2.D0 * S(2,I,J) * E(:,2)
     &                              + 2.D0 * S(3,I,J) * E(:,3)
        ELSE
          SE(:) = 0.
        ENDIF

        ESTF(:) = ESTF(:) 
     &            + ( BTDB(:) + SE(:) ) * WGT * D1 * D2 * RL * .25D0

      ENDDO
      ENDDO

      CALL STFGL(ESTF,WK,TE,12,3)

      END
