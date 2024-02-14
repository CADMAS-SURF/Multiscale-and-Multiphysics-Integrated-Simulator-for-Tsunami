      SUBROUTINE EFPTE1(FP,X,PB,ITO)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RN(4),DNDX(3,4),X(3,4),PB(4),FP(3,4)

      RN(:) = .25D0

      CALL DERXTE1(DNDX,DET,X,ITO)

      CALL VECML1(PG,RN,PB,4)

      FP(:,:) = DNDX(:,:) * PG * DET / 6.D0

      END
