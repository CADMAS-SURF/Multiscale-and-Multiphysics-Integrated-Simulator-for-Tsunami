      SUBROUTINE VINTPN1(XN,XG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XG(3,3,2),XD(3,6),XN(3,6)
C-----------------------------------------------------------------------
      R = .5D0 * ( DSQRT(3.D0) - 1.D0 )
C
      DO I = 1, 3
        XD(:,I)   = ( 1.D0 + R ) * XG(:,I,1) - R * XG(:,I,2)
        XD(:,I+3) = ( 1.D0 + R ) * XG(:,I,2) - R * XG(:,I,1)
      ENDDO
C
      XN(:,1) = XD(:,3) + XD(:,1) - XD(:,2)
      XN(:,2) = XD(:,1) + XD(:,2) - XD(:,3)
      XN(:,3) = XD(:,2) + XD(:,3) - XD(:,1)
      XN(:,4) = XD(:,6) + XD(:,4) - XD(:,5)
      XN(:,5) = XD(:,4) + XD(:,5) - XD(:,6)
      XN(:,6) = XD(:,5) + XD(:,6) - XD(:,4)
C
      END
