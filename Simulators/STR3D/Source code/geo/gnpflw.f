      SUBROUTINE GNPFLW(FLI,FLO,FCP,VELG,VELE,GRID,UG1,UG2,UG3,PG1,PG2
     &                 ,PG3,KN,ND,AMAT,GRAV,DT1,DT2,ALP,BETA,IDYN,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION FI(ND),FO(ND),FP(3,ND),VELG(3,*),VELE(3),GRID(3,*)
     &         ,UG1(6,*),UG2(6,*),UG3(6,*),PG1(*),PG2(*),PG3(*),KN(ND)
     &         ,AMAT(*),GRAV(3),X(3,ND),UD(3,ND),UDD(3,ND),P(ND),PB(ND)
     &         ,PD(ND),FLI(*),FLO(*),FCP(3,*)

      POR  = AMAT(7)
      RKF  = AMAT(8)
      RK   = AMAT(9)
      RHOF = AMAT(10)

      X(:,:) = GRID(:,KN(:))

      IF( IDYN == 1 ) THEN

        DT12 = .5D0 * ( DT1 + DT2 )

        UD(:,:) = ( UG3(1:3,KN(:)) - UG1(1:3,KN(:)) ) / DT12 * .5D0

        UDD(:,:) = ( ( UG3(1:3,KN(:)) - UG2(1:3,KN(:)) ) / DT2 -
     &               ( UG2(1:3,KN(:)) - UG1(1:3,KN(:)) ) / DT1 ) / DT12

        P(:) = ( 1.D0 - ALP ) * PG1(KN(:)) + ALP * PG3(KN(:))

        PB(:) = BETA * PG3(KN(:)) + ( 1.D0 - 2.D0 * BETA ) * PG2(KN(:))
     &        + BETA * PG1(KN(:))

        PD(:) = ( PG3(KN(:)) - PG1(KN(:)) ) / DT12 * .5D0

      ELSEIF( IDYN == 0 ) THEN

        UD(:,:) = 0.

        UDD(:,:) = 0.

        P(:) = PG3(KN(:))

        PB(:) = PG3(KN(:))

        PD(:) = 0.

      ENDIF

      SELECT CASE( ND )
      CASE( 4 )
        CALL GNPFTE1(FI,FO,FP,VELG,VELE,X,UD,UDD,P,PB,PD,POR,RKF,RK,RHOF
     &              ,GRAV,ITO)
      CASE( 10 )
        CALL GNPFTE2(FI,FO,FP,VELG,VELE,X,UD,UDD,P,PB,PD,POR,RKF,RK,RHOF
     &              ,GRAV,ITO)
      CASE( 6 )
        CALL GNPFPN2(FI,FO,FP,VELG,VELE,ND,3,2,X,UD,UDD,P,PB,PD,POR,RKF
     &              ,RK,RHOF,GRAV,ITO)
      CASE( 15 )
        CALL GNPFPN2(FI,FO,FP,VELG,VELE,ND,7,3,X,UD,UDD,P,PB,PD,POR,RKF
     &              ,RK,RHOF,GRAV,ITO)
      CASE( 8 ) 
        CALL GNPFHX2(FI,FO,FP,VELG,VELE,ND,2,X,UD,UDD,P,PB,PD,POR,RKF,RK
     &              ,RHOF,GRAV,ITO)
      CASE( 20 ) 
        CALL GNPFHX2(FI,FO,FP,VELG,VELE,ND,3,X,UD,UDD,P,PB,PD,POR,RKF,RK
     &              ,RHOF,GRAV,ITO)
      END SELECT

      FLI(KN(:)) = FLI(KN(:)) + FI(:)

      FLO(KN(:)) = FLO(KN(:)) + FO(:)

      FCP(:,KN(:)) = FCP(:,KN(:)) + FP(:,:)

      END
