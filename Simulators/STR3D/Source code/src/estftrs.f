      SUBROUTINE ESTFTRS(ESTF,GRID,UG,NP,A,GR,S,IGNL)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION GRID(3,*),UG(6,*),NP(2),XR0(3),XR(3),UR(3),ER(3),E(3,3)
     &         ,RK(3,3),ESTF(21)

      DATA E / 1.D0, 0.D0, 0.D0,
     &         0.D0, 1.D0, 0.D0,
     &         0.D0, 0.D0, 1.D0 /

      XR0(:) = GRID(:,NP(2)) - GRID(:,NP(1))

      CALL VECML2(AXR0,XR0,3)

      IF( IGNL == 0 ) THEN

        ER(:) = XR0(:) / AXR0

      ELSE

        UR(:) = UG(1:3,NP(2)) - UG(1:3,NP(1))

        XR(:) = XR0(:) + UR(:)

        CALL VECML2(AXR,XR,3)

        ER(:) = XR(:) / AXR

      ENDIF

      DO I = 1, 3
        DO J = 1, 3
          RK(I,J) = GR * A / AXR0 * ER(I) * ER(J)
          IF( IGNL > 0 )
     &      RK(I,J) = RK(I,J) + S * A / AXR * ( E(I,J) - ER(I) * ER(J) )
        ENDDO
      ENDDO

      IP = 0

      DO I = 1, 6
        DO J = I, 6
          IP = IP + 1
          IF( I <= 3 ) THEN
            IF( J <= 3 ) THEN
              ESTF(IP) = RK(I,J)
            ELSE
              ESTF(IP) = -RK(I,J-3)
            ENDIF
          ELSE
            ESTF(IP) = RK(I-3,J-3)
          ENDIF
        ENDDO
      ENDDO

      END
