      SUBROUTINE GSFHX(XG,XX,NP,N)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION II(4,6),IG(4),NP(8),XG(N,6),XX(N,*)
      DATA II / 1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6 /

      DO I = 1, 6

        IG(:) = NP( II(:,I) )

        CALL MEAN4(XG(1,I),XX,N,IG,4)

      ENDDO

      END
