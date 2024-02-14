      SUBROUTINE GNPFTE1(FI,FO,FP,VELG,VELE,X,UD,UDD,P,PB,PD,POR,RKF,RK
     &                  ,RHOF,GRAV,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RN(4),DNDX(3,4),X(3,4),GRP(3),P(4),UDDG(3),UDD(3,4)
     &         ,GRAV(3),FI(4),UD(3,4),PD(4),FO(4),PB(4),FP(3,4),VELE(3)
     &         ,VELG(3)

      RN(:) = .25D0

      CALL DERXTE1(DNDX,DET,X,ITO)

      CALL AXB(GRP,DNDX,P,3,4,1)
      CALL AXB(UDDG,UDD,RN,3,4,1)

      VELG(:) = RK * ( -GRP(:) + RHOF*GRAV(:) - RHOF*UDDG(:) )

      CALL ATXB(FI,DNDX,VELG,3,4,1)

      FI(:) = -FI(:) * DET / 6.D0

      CALL VECML1(DUD,DNDX,UD,12)
      CALL VECML1(PDG,RN,PD,4)

      FO(:) = -RN(:) * ( DUD + POR / RKF * PDG ) * DET / 6.D0

      CALL VECML1(PG,RN,PB,4)

      FP(:,:) = DNDX(:,:) * PG * DET / 6.D0

!     ----- VELOCITY OF CENTER POINT -----

      VELE(:) = VELG(:)

      END
