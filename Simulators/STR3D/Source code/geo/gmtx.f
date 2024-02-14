      SUBROUTINE GMTX(RKUP,RKPP,CPU,CPP,RMPU,RN,DNDX,N,N3)

      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION RKUP(N3,N),RKPP(N,N),CPU(N,N3),CPP(N,N),RMPU(N,N3)
     &         ,RN(N),DNDX(3,N),RN3(3,N3)

      CALL AXB(RKUP,DNDX,RN,N3,1,N)

      CALL ATXB(RKPP,DNDX,DNDX,3,N,N)

      CALL AXB(CPU,RN,DNDX,N,1,N3)

      CALL AXB(CPP,RN,RN,N,1,N)

      RN3(:,:) = 0.

      DO J = 1, N
        JJ = 3 * ( J - 1)
        DO I = 1, 3
          RN3(I,JJ+I) = RN(J)
        ENDDO
      ENDDO

      CALL ATXB(RMPU,DNDX,RN3,3,N,N3)

      END
