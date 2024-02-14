      SUBROUTINE GNPFHX2(FI,FO,FP,VELG,VELE,ND,NG,X,UD,UDD,P,PB,PD,POR
     &                  ,RKF,RK,RHOF,GRAV,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XG(3,3),WG(3,3),RN(ND),DNDX(3,ND),X(3,ND),GRP(3),P(ND)
     &         ,UDDG(3),UDD(3,ND),VELG(3,NG,NG,NG),GRAV(3),FIG(ND)
     &         ,FI(ND),UD(3,ND),PD(ND),FO(ND),PB(ND),FP(3,ND),VELE(3)

      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /

      FI(:) = 0.
      FO(:) = 0.
      FP(:,:) = 0.

      DO I = 1, NG
      DO J = 1, NG
      DO K = 1, NG

        XG1 = XG(I,NG)
        XG2 = XG(J,NG)
        XG3 = XG(K,NG)

        WGT=WG(I,NG)*WG(J,NG)*WG(K,NG)

        CALL SFNHX2(XG1,XG2,XG3,ND,RN)

        CALL DERXHX2(DNDX,DET,XG1,XG2,XG3,X,ND,ITO)

        CALL AXB(GRP,DNDX,P,3,ND,1)
        CALL AXB(UDDG,UDD,RN,3,ND,1)
                
        VELG(:,I,J,K) = RK * ( -GRP(:) + RHOF*GRAV(:) - RHOF*UDDG(:) )

        CALL ATXB(FIG,DNDX,VELG(1,I,J,K),3,ND,1)

        FI(:) = FI(:) - FIG(:) * DET * WGT

        CALL VECML1(DUD,DNDX,UD,ND*3)
        CALL VECML1(PDG,RN,PD,ND)

        FO(:) = FO(:) - RN(:) * ( DUD + POR / RKF * PDG ) * DET * WGT

        CALL VECML1(PG,RN,PB,ND)

        FP(:,:) = FP(:,:) + DNDX(:,:) * PG * DET * WGT

      ENDDO
      ENDDO
      ENDDO

!     ----- VELOCITY OF CENTER POINT -----

      IF( ND == 8 ) THEN

        XG1 = 0.
        XG2 = 0.
        XG3 = 0.

        CALL SFNHX2(XG1,XG2,XG3,ND,RN)

        CALL DERXHX2(DNDX,DET,XG1,XG2,XG3,X,ND,ITO)

        CALL AXB(GRP,DNDX,P,3,ND,1)
        CALL AXB(UDDG,UDD,RN,3,ND,1)
                
        VELE(:) = RK * ( -GRP(:) + RHOF*GRAV(:) - RHOF*UDDG(:) )

      ELSE
       
        VELE(:) = VELG(:,2,2,2)

      ENDIF

      END
