      SUBROUTINE SF_CUT2TR(SS,P0,X,EPS)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P0(2,3),P1(2,3,2),P2(2,3,2),P3(2,3,2),TR(4,2),X0(2,2)
     &         ,X(2,3)

      CALL SF_S34(S,X,3)
      EPSA = -S * 1.D-9

      CALL SF_TRM2(TR,X0,X)

      SS = 0.

      CALL SF_CUT2X(N1,P1(1,1,1),P1(1,1,2),P0,0.D0,EPS,EPSA,1,2,1)

      DO I = 1, N1

        CALL SF_TRNS2(P1(1,1,I),X0(1,1),TR(1,1))

        CALL SF_CUT2X(N2,P2(1,1,1),P2(1,1,2),P1(1,1,I),0.D0,EPS,EPSA,1
     &               ,2,1)
        DO J = 1, N2

          CALL SF_TRNS2(P2(1,1,J),X0(1,2),TR(1,2))

          CALL SF_CUT2X(N3,P3(1,1,1),P3(1,1,2),P2(1,1,J),0.D0,EPS,EPSA
     &                 ,1,2,1)
          DO K = 1, N3
            CALL SF_S34(S,P3(1,1,K),3)
            SS = SS + S
          ENDDO

        ENDDO

      ENDDO

      END