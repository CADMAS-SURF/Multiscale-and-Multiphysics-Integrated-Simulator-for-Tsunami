      SUBROUTINE SF_CUT3(VV,SX,SY,SZ,P0,XG,YG,ZG,EPS)

      IMPLICIT REAL*8(A-H,O-Z)

      DIMENSION P0(3,4),XG(2),YG(2),ZG(2),P1(3,4,3),P2(3,4,3),P3(3,4,3)
     &         ,P4(3,4,3),P5(3,4,3),P6(3,4,3)
!-----------------------------------------------------------------------
      DX = XG(2) - XG(1)
      DY = YG(2) - YG(1)
      DZ = ZG(2) - ZG(1)

      EPSV = DX * DY * DZ * 1.D-9

      VV = 0.
      SX = 0.
      SY = 0.
      SZ = 0.

      CALL SF_CUT3X(N1,P1(1,1,1),P1(1,1,2),P1(1,1,3),SX,P0,XG(1),YG,ZG
     &             ,EPS,EPSV,1,2,3,1)
      DO I = 1, N1
        CALL SF_CUT3X(N2,P2(1,1,1),P2(1,1,2),P2(1,1,3),SX,P1(1,1,I)
     &               ,XG(2),YG,ZG,EPS,EPSV,1,2,3,2)
        DO J = 1, N2
          CALL SF_CUT3X(N3,P3(1,1,1),P3(1,1,2),P3(1,1,3),SY,P2(1,1,J)
     &                 ,YG(1),ZG,XG,EPS,EPSV,2,3,1,1)
          DO K = 1, N3
            CALL SF_CUT3X(N4,P4(1,1,1),P4(1,1,2),P4(1,1,3),SY,P3(1,1,K)
     &                   ,YG(2),ZG,XG,EPS,EPSV,2,3,1,2)
            DO L = 1, N4
              CALL SF_CUT3X(N5,P5(1,1,1),P5(1,1,2),P5(1,1,3),SZ
     &                     ,P4(1,1,L),ZG(1),XG,YG,EPS,EPSV,3,1,2,1)
              DO M = 1, N5
                CALL SF_CUT3X(N6,P6(1,1,1),P6(1,1,2),P6(1,1,3),SZ
     &                       ,P5(1,1,M),ZG(2),XG,YG,EPS,EPSV,3,1,2,2)
                DO N = 1, N6
                  CALL SF_TETVOL(P6(1,1,N),V)
                  VV = VV + V
                ENDDO
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDDO

      END