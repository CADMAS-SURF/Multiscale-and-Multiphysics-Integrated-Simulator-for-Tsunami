      SUBROUTINE EFPHX2(FP,ND,NG,X,PB,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XG(3,3),WG(3,3),RN(ND),DNDX(3,ND),X(3,ND),PB(ND)
     &         ,FP(3,ND)

      DATA XG / 3*0.0D0
     &        , -.577350269189626D0, .577350269189626D0, 0.0D0
     &        , -.774596669241483D0, 0.0D0, .774596669241483D0 /
      DATA WG / 2.0D0, 2*0.0D0
     &        , 2*1.0D0, 0.0D0
     &        , 0.55555555555555556D0, 0.88888888888888889D0
     &        , 0.55555555555555556D0 /

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

        CALL VECML1(PG,RN,PB,ND)

        FP(:,:) = FP(:,:) + DNDX(:,:) * PG * DET * WGT

      ENDDO
      ENDDO
      ENDDO

      END
