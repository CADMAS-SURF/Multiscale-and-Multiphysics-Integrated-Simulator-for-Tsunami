      SUBROUTINE GNPFPN2(FI,FO,FP,VELG,VELE,ND,NI,NJ,X,UD,UDD,P,PB,PD
     &                  ,POR,RKF,RK,RHOF,GRAV,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RG(7,2),SG(7,2),TG(3,2),WGI(7,2),WGJ(3,2),RN(ND),X(3,ND)
     &         ,DNDX(3,ND),GRP(3),P(ND),UDDG(3),UDD(3,ND),VELG(3,NI,NJ)
     &         ,GRAV(3),FIG(ND),FI(ND),UD(3,ND),PD(ND),FO(ND),PB(ND)
     &         ,FP(3,ND),VELE(3)

      DATA RG / .5D0, .5D0, 0.D0, 4*0.D0,
     &  0.101286507323456D0, 0.797426985353087D0, 0.101286507323456D0,
     &  0.470142064105115D0, 0.470142064105115D0, 0.059715871789770D0,
     &  0.333333333333333D0 /
      DATA SG / 0.D0, .5D0, .5D0, 4*0.D0,
     &  0.101286507323456D0, 0.101286507323456D0, 0.797426985353087D0,
     &  0.059715871789770D0, 0.470142064105115D0, 0.470142064105115D0,
     &  0.333333333333333D0 /
      DATA TG / -.577350269189626D0, .577350269189626D0, 0.D0,
     &  -.774596669241483D0, 0.0D0, .774596669241483D0 /

      DATA WGI / 3*0.166666666666667D0, 4*0.D0,
     &  3*0.062969590272414D0, 3*0.066197076394253D0, 0.1125D0 /
      DATA WGJ / 2*1.D0, 0.D0,
     &  0.55555555555555556D0, 0.88888888888888889D0,
     &  0.55555555555555556D0 /

      IF( ND == 6 ) THEN
        ID = 1
      ELSE
        ID = 2
      ENDIF

      FI(:) = 0.
      FO(:) = 0.
      FP(:,:) = 0.

      DO J = 1, NJ
        DO I = 1, NI

          R = RG(I,ID)
          S = SG(I,ID)
          T = TG(J,ID)

          WGT = WGI(I,ID)*WGJ(J,ID)

          CALL SFNPN2(RN,ND,R,S,T)

          CALL DERXPN2(DNDX,DET,ND,X,R,S,T,ITO)

          CALL AXB(GRP,DNDX,P,3,ND,1)
          CALL AXB(UDDG,UDD,RN,3,ND,1)
                
          VELG(:,I,J) = RK * ( -GRP(:) + RHOF*GRAV(:) - RHOF*UDDG(:) )

          CALL ATXB(FIG,DNDX,VELG(1,I,J),3,ND,1)

          FI(:) = FI(:) - FIG(:) * DET * WGT

          CALL VECML1(DUD,DNDX,UD,ND*3)
          CALL VECML1(PDG,RN,PD,ND)

          FO(:) = FO(:) - RN(:) * ( DUD + POR / RKF * PDG ) * DET * WGT

          CALL VECML1(PG,RN,PB,ND)

          FP(:,:) = FP(:,:) + DNDX(:,:) * PG * DET * WGT

        ENDDO
      ENDDO

!     ----- VELOCITY OF CENTER POINT -----

      IF( ND == 6 ) THEN

        R = 1.D0 / 3.D0
        S = 1.D0 / 3.D0
        T = 0.D0

        CALL SFNPN2(RN,ND,R,S,T)

        CALL DERXPN2(DNDX,DET,ND,X,R,S,T,ITO)

        CALL AXB(GRP,DNDX,P,3,ND,1)
        CALL AXB(UDDG,UDD,RN,3,ND,1)
                
        VELE(:) = RK * ( -GRP(:) + RHOF*GRAV(:) - RHOF*UDDG(:) )

      ELSE
       
        VELE(:) = VELG(:,7,2)

      ENDIF

      END
