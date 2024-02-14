      SUBROUTINE SF_GSFPN(XG,XX,NP)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION II(4,3),IG(4),NP(6),XG(3,3),XX(3,*),X(4)
      DATA II / 1,4,5,2, 2,5,6,3, 3,6,4,1 /

      DO I = 1, 3

        IG(:) = NP( II(:,I) )

        DO J = 1, 3

          X(:) = XX(J,IG(:))

          CALL SF_MEAN1(XG(J,I),X,4)

        ENDDO

      ENDDO

      END
