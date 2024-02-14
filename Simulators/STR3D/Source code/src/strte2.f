      SUBROUTINE STRTE2(EPS,SIG,EPSG,SIGG)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION EPSG(6,5),SIGG(6,5),EPS(6,10),SIG(6,10),II(2,10)
      DATA II / 8*0,  1,2,  2,3,  3,1,  1,4,  2,4,  3,4 /
C-----------------------------------------------------------------------
      DO 400 I=1,4
        DO 410 J=1,6
          EPS(J,I)=3.D0*EPSG(J,I)-2.D0*EPSG(J,5)
          SIG(J,I)=3.D0*SIGG(J,I)-2.D0*SIGG(J,5)
  410   CONTINUE
  400 CONTINUE
C
      DO 500 I=5,10
        DO 510 J=1,6
          EPS(J,I)=1.5D0*( EPSG(J,II(1,I)) + EPSG(J,II(2,I)) )
     &            -2.D0*EPSG(J,5)
          SIG(J,I)=1.5D0*( SIGG(J,II(1,I)) + SIGG(J,II(2,I)) )
     &            -2.D0*SIGG(J,5)
  510   CONTINUE
  500 CONTINUE
C
      END
