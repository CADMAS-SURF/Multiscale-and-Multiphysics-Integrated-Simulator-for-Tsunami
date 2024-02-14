      SUBROUTINE SF_GSFHX(XG,XX,NP)

      IMPLICIT REAL*8(A-H,O-Z)      
      DIMENSION II(4,6),IG(4),NP(8),XG(3,6),XX(3,*),X(4)
      DATA II / 1,2,3,4, 1,5,6,2, 2,6,7,3, 3,7,8,4, 4,8,5,1, 5,8,7,6 /

      DO I = 1, 6

        IG(:) = NP( II(:,I) )

        DO J = 1, 3

          X(:) = XX(J,IG(:))

          CALL SF_MEAN1(XG(J,I),X,4)

        ENDDO

      ENDDO

      END
