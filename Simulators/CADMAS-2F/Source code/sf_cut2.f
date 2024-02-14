      SUBROUTINE SF_CUT2(SS,P0,XG,YG,EPS)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P0(2,3),XG(2),YG(2),P1(2,3,2),P2(2,3,2),P3(2,3,2)
     &         ,P4(2,3,2)
!-----------------------------------------------------------------------
      DX = XG(2) - XG(1)
      DY = YG(2) - YG(1)

      EPSA = DX * DY * 1.D-9

      SS = 0.

      CALL SF_CUT2X(N1,P1(1,1,1),P1(1,1,2),P0,XG(1),EPS,EPSA,1,2,1)
      DO I = 1, N1
        CALL SF_CUT2X(N2,P2(1,1,1),P2(1,1,2),P1(1,1,I),XG(2),EPS,EPSA,1
     &               ,2,2)
        DO J = 1, N2
          CALL SF_CUT2X(N3,P3(1,1,1),P3(1,1,2),P2(1,1,J),YG(1),EPS,EPSA
     &                 ,2,1,1)
          DO K = 1, N3
            CALL SF_CUT2X(N4,P4(1,1,1),P4(1,1,2),P3(1,1,K),YG(2),EPS
     &                   ,EPSA,2,1,2)
            DO L = 1, N4
              CALL SF_S34(S,P4(1,1,L),3)
              SS = SS + S
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      END