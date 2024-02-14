      SUBROUTINE ENPFTRS(FC,EPS,S,GRID,UG,NP,A,GR,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XR0(3),XR(3),UR(3),GRID(3,*),UG(6,*),NP(2),ER(3),FC(3,2)

      XR0(:) = GRID(:,NP(2)) - GRID(:,NP(1))

      CALL VECML2(AXR0,XR0,3)

      UR(:) = UG(1:3,NP(2)) - UG(1:3,NP(1))

      EPS0 = EPS

      IF( IGNL == 0 ) THEN

        ER(:) = XR0(:) / AXR0

        CALL VECML1(AUR,UR,ER,3)

        EPS = AUR / AXR0

      ELSE

        XR(:) = XR0(:) + UR(:)

        CALL VECML2(AXR,XR,3)

        ER(:) = XR(:) / AXR

        EPS = AXR / AXR0 - 1.D0

      ENDIF

      DEPS = EPS - EPS0

      S = S + GR * DEPS

      FT = S * A

      FC(:,1) = -FT * ER(:)
      FC(:,2) =  FT * ER(:)

      END