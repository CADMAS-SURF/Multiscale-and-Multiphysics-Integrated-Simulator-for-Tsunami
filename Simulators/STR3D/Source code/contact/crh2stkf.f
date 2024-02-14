      SUBROUTINE CRH2STKF(CRH,NS,IELC,IFCQ,POS)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION X(3,4),POS(3,*),IELC(3),RL(3),E(3),CRH(*)

      X(:,1:3) = POS(:,IELC(:))
      X(:,4) = POS(:,NS)

      CALL AREACD(RL,E,H,X)

      IF( IFCQ == 0 ) THEN

        IP = 0

        DO I = 1, 3
          DO J = 1, 3
            IP = IP + 1
            CRH(IP) = RL(J)
          ENDDO
        ENDDO

      ELSE

        IP = 0

        DO I = 1, 3
          DO J = 1, 2
            IP = IP + 1
            CRH(IP) = RL(J) + .25D0 * RL(3)
          ENDDO
          DO J = 3, 4
            IP = IP + 1
            CRH(IP) = .25D0 * RL(3)
          ENDDO
        ENDDO

      ENDIF

      END
