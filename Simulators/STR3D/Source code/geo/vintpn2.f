      SUBROUTINE VINTPN2(XN,XG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XG(3,7,3),XD(3,7,3),XN(3,15)
C-----------------------------------------------------------------------
      R = DSQRT(5.D0/3.D0)
C
      XD(:,:,1) = R * XG(:,:,1) + ( 1.D0 - R ) * XG(:,:,2)
      XD(:,:,2) = XG(:,:,2)
      XD(:,:,3) = R * XG(:,:,3) + ( 1.D0 - R ) * XG(:,:,2)
C
      A = ( 9.D0 + 2.D0 * DSQRT(15.D0) ) / 21.D0
      RA = 2.D0/3.D0 / ( A - 1.D0/3.D0 )
      B = ( 9.D0 - 2.D0 * DSQRT(15.D0) ) / 21.D0
      RB = 1.D0/3.D0 / ( 1.D0/3.D0 - B )
C
      DO J = 1, 3
C
        SELECT CASE(J)
        CASE(1)
          I0 = 0
        CASE(2)
          I0 = 9
        CASE(3)
          I0 = 3
        END SELECT
C
        DO I = 1, 3
          XN(:,I0+I) = RA * XD(:,I,J) + ( 1.D0 - RA ) * XD(:,7,J)
        ENDDO
C
        SELECT CASE(J)
        CASE(1)
          I0 = 3
        CASE(2)
          CYCLE
        CASE(3)
          I0 = 9
        END SELECT
C
        DO I = 4, 6
          XN(:,I0+I) = RB * XD(:,I,J) + ( 1.D0 - RB ) * XD(:,7,J)
        ENDDO
C
      ENDDO
C
      END
