      SUBROUTINE GSFPN(XG,XX,NP,N)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION II(4,5),IG(4),NP(6),XG(N,3),XX(N,*)
      DATA II / 1,2,3,0, 4,6,5,0, 1,4,5,2, 2,5,6,3, 3,6,4,1 /

      DO I = 1, 3

        IG(:) = NP( II(:,I+2) )

        CALL MEAN4(XG(1,I),XX,N,IG,4)

      ENDDO

      END
