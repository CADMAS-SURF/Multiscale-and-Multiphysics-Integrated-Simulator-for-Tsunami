      SUBROUTINE STRHX1(EPS0,SIG0,EPSG,SIGG,GRID,KN)
C
      IMPLICIT REAL*8(A-H,O-Z)
      DIMENSION XYZ(3,8),GRID(3,*),KN(8),TE(3,3),TE1(6,6),TE2(6,6)
     &         ,EPS(6),SIG(6),EPS0(6,8),SIG0(6,8),EPSEL(6),SIGEL(6)
     &         ,EPSG(6,2,2,2),SIGG(6,2,2,2),IG(3,8)
      INCLUDE 'gauss_ln_3.h'
      DATA IG / 1,1,1, 2,1,1, 2,2,1, 1,2,1,
     &          1,1,2, 2,1,2, 2,2,2, 1,2,2 /
C-----------------------------------------------------------------------
      CALL TRNSMTX3(8,KN,GRID,TE,XYZ)
      CALL CHTNSR(TE,TE1,TE2)
C
      CALL MEAN3(EPSEL,EPSG,6,8)
      CALL MEAN3(SIGEL,SIGG,6,8)
C
      FAC=1.D0/XG(2,2)
C
      DO 300 I=1,8
        II=IG(1,I)
        JJ=IG(2,I)
        KK=IG(3,I)
        CALL RMULT5( EPS, 1.-FAC, EPSEL, FAC, EPSG(1,II,JJ,KK), 6 )
        CALL RMULT5( SIG, 1.-FAC, SIGEL, FAC, SIGG(1,II,JJ,KK), 6 )
        CALL ATXB(EPS0(1,I),TE1,EPS,6,6,1)
        CALL ATXB(SIG0(1,I),TE2,SIG,6,6,1)
  300 CONTINUE
C
      END
